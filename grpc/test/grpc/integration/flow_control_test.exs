defmodule GRPC.Integration.FlowControlTest do
  use GRPC.Integration.TestCase

  require Record

  # Where a stream's unread request data sits on the server: cowboy's HTTP/2 connection and its
  # streams, the HTTP/2 machine's per-stream receive windows, and grpc_stream_h's buffer.
  @cowboy Path.join(Mix.Project.deps_paths()[:cowboy], "src/cowboy_http2.erl")
  @cowlib Path.join(Mix.Project.deps_paths()[:cowlib], "src/cow_http2_machine.erl")
  @stream_h Path.join(Mix.Project.deps_paths()[:grpc_server], "src/grpc_stream_h.erl")
  Record.defrecordp(:conn, :state, Record.extract(:state, from: @cowboy))
  Record.defrecordp(:conn_stream, :stream, Record.extract(:stream, from: @cowboy))
  Record.defrecordp(:machine, :http2_machine, Record.extract(:http2_machine, from: @cowlib))
  Record.defrecordp(:machine_stream, :stream, Record.extract(:stream, from: @cowlib))
  Record.defrecordp(:stream_h, :state, Record.extract(:state, from: @stream_h))

  @window 65_535
  @note_bytes 16 * 1024

  defmodule StalledServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def get_feature(point, _stream), do: %Routeguide.Feature{location: point, name: "ok"}

    # Tells the test it has started, then reads nothing until the test sends :read.
    def route_chat(req_enum, _stream) do
      send(:persistent_term.get({GRPC.Integration.FlowControlTest, :test}), {:stalled, self()})

      receive do
        :read -> Enum.each(req_enum, fn _ -> :ok end)
      end
    end
  end

  setup do
    :persistent_term.put({__MODULE__, :test}, self())
    on_exit(fn -> :persistent_term.erase({__MODULE__, :test}) end)
  end

  test "a handler that stops reading holds its client to the stream window" do
    run_server(StalledServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      stalled = open_stalled_stream(channel)

      for _ <- 1..64, do: GRPC.Stub.send_request(stalled.stream, note())

      assert {:blocked, buffered} = await_blocked(stalled.stream_id)
      assert buffered <= @window

      send(stalled.handler, :read)
    end)
  end

  test "with default windows, streams that stop reading leave room for others on their connection" do
    run_server(StalledServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      stalled = stall_two_streams(channel)

      assert {:ok, %Routeguide.Feature{name: "ok"}} =
               Routeguide.RouteGuide.Stub.get_feature(channel, %Routeguide.Point{},
                 timeout: 2_000
               )

      for s <- stalled, do: send(s.handler, :read)
    end)
  end

  test "with a connection window above the stream windows, stalled streams leave room for others" do
    adapter_opts = [
      initial_connection_window_size: 16 * @window,
      connection_window_margin_size: 16 * @window
    ]

    run_server(
      StalledServer,
      fn port ->
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
        stalled = stall_two_streams(channel)

        assert {:ok, %Routeguide.Feature{name: "ok"}} =
                 Routeguide.RouteGuide.Stub.get_feature(channel, %Routeguide.Point{},
                   timeout: 2_000
                 )

        for s <- stalled, do: send(s.handler, :read)
      end,
      0,
      adapter_opts: adapter_opts
    )
  end

  defp note do
    %Routeguide.RouteNote{location: %Routeguide.Point{}, message: :binary.copy("x", @note_bytes)}
  end

  # Two bidi streams whose handlers read nothing, each sent more than a stream window. Together
  # they want more than the connection's default window.
  defp stall_two_streams(channel) do
    first = open_stalled_stream(channel, [])
    second = open_stalled_stream(channel, [first.stream_id])

    for s <- [first, second] do
      for _ <- 1..8, do: GRPC.Stub.send_request(s.stream, note())
    end

    await_shut([first.stream_id, second.stream_id])
    [first, second]
  end

  # Waits until the senders can't send another message: the connection's window, or every stalled
  # stream's own window, has less than one message left.
  defp await_shut(stream_ids, deadline \\ System.monotonic_time(:millisecond) + 5_000) do
    {state, _} = :sys.get_state(connection())
    m = conn(state, :http2_machine)

    stream_left =
      for id <- stream_ids,
          do: m |> machine(:streams) |> Map.fetch!(id) |> machine_stream(:remote_window)

    cond do
      machine(m, :remote_window) < @note_bytes ->
        :connection_shut

      Enum.all?(stream_left, &(&1 < @note_bytes)) ->
        :streams_shut

      System.monotonic_time(:millisecond) > deadline ->
        {:still_sending, machine(m, :remote_window), stream_left}

      true ->
        await_shut(stream_ids, deadline)
    end
  end

  # Opens a bidi stream whose handler reads nothing, and finds its HTTP/2 stream on the server.
  defp open_stalled_stream(channel, known_ids \\ []) do
    stream = Routeguide.RouteGuide.Stub.route_chat(channel)
    GRPC.Stub.send_request(stream, note())
    assert_receive {:stalled, handler}, 2_000

    [stream_id] =
      for {id, s} <- server_streams(),
          match?({:grpc_stream_h, _}, conn_stream(s, :state)),
          id not in known_ids,
          do: id

    %{stream: stream, handler: handler, stream_id: stream_id}
  end

  # Follows the stalled stream until its receive window is shut (`{:blocked, buffered}`) or its
  # buffer passes the window (`{:overflowed, buffered}`). Each read is a synchronous call into the
  # connection process, so data in flight lands between reads.
  defp await_blocked(stream_id, deadline \\ System.monotonic_time(:millisecond) + 5_000) do
    {state, _} = :sys.get_state(connection())

    left =
      state
      |> conn(:http2_machine)
      |> machine(:streams)
      |> Map.fetch!(stream_id)
      |> machine_stream(:remote_window)

    {:grpc_stream_h, handler} =
      state |> conn(:streams) |> Map.fetch!(stream_id) |> conn_stream(:state)

    buffered = byte_size(stream_h(handler, :read_body_buffer))

    cond do
      buffered > @window -> {:overflowed, buffered}
      left < @note_bytes -> {:blocked, buffered}
      System.monotonic_time(:millisecond) > deadline -> {:still_sending, buffered, left}
      true -> await_blocked(stream_id, deadline)
    end
  end

  defp connection do
    [conn] = for ref <- Map.keys(:ranch.info()), pid <- :ranch.procs(ref, :connections), do: pid
    conn
  end

  defp server_streams do
    {state, _} = :sys.get_state(connection())
    conn(state, :streams)
  end
end
