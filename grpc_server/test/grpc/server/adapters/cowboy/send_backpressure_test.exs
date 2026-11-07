defmodule GRPC.Server.Adapters.Cowboy.SendBackpressureTest do
  # Not async: the tests sample process state, and other servers' load would blur it.
  use GRPC.Integration.TestCase, async: false

  import GRPC.DataCase, only: [attach_telemetry: 1]

  alias GRPC.Server.Adapters.Cowboy.Handler

  # A raw HTTP/2 client that never sends WINDOW_UPDATE holds the server to the initial 65,535 byte
  # windows. A client library would read and credit the data.
  @window 65_535
  @name_size 16 * 1024
  @message_size @name_size + 4
  @total 400
  # Cowboy's own buffers, kept small so they don't hide the queue the server builds.
  @buffers [max_stream_buffer_size: 32_768, max_connection_buffer_size: 65_536]
  @shutdown_timeout 300
  @send_error [:grpc, :server, :rpc, :send_error]

  # Messages the connection can hold before it reports its buffers full, plus the one send
  # that crosses the limit and the one waiting for an ack.
  @held div(@window + 32_768, @message_size) + 3
  @stages 2

  defmodule FloodServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    # The request's `lo.latitude` picks the way replies are sent.
    def list_features(%Routeguide.Rectangle{lo: %{latitude: way}}, stream) do
      {counter, test, total} =
        :persistent_term.get({GRPC.Server.Adapters.Cowboy.SendBackpressureTest, :ctx})

      send(test, {:producer, self(), stream.payload.pid})

      features =
        Stream.repeatedly(fn ->
          :counters.add(counter, 1, 1)
          %Routeguide.Feature{name: :binary.copy("x", 16 * 1024)}
        end)
        |> Stream.take(total)

      case way do
        1 ->
          Enum.each(features, &GRPC.Server.send_reply(stream, &1))

        2 ->
          features
          |> GRPC.Stream.from(max_demand: 1, stages: 2)
          |> GRPC.Stream.run_with(stream)

        3 ->
          Process.flag(:trap_exit, true)

          try do
            Enum.each(features, &GRPC.Server.send_reply(stream, &1))
            send(test, {:producer_done, self()})
          catch
            kind, reason -> send(test, {:producer_stopped, self(), kind, reason})
          end

        4 ->
          # Stops at the first send that times out. Waits for the test to open the client's
          # window. The reply of `get_bindings/1` comes after the stream process has acked
          # the timed out send, so the mailbox is checked after the ack was sent.
          try do
            Enum.each(features, &GRPC.Server.send_reply(stream, &1))
          catch
            :error, %GRPC.RPCError{status: 4} ->
              send(test, {:timed_out, self()})

              receive do
                :window_open -> :ok
              end

              Handler.get_bindings(stream.payload.pid)
              {:messages, messages} = Process.info(self(), :messages)
              send(test, {:mailbox, self(), messages})
          end
      end
    end
  end

  # Cowboy acks a send that fills its buffer, and holds the ack of the next one. A unary
  # handler that sends one big reply first has its returned reply held, as if other calls
  # on the connection had filled the buffer.
  defmodule BigReplyServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def get_feature(_point, stream) do
      GRPC.Server.send_reply(stream, %Routeguide.Feature{name: :binary.copy("x", 200_000)})
      %Routeguide.Feature{name: "last"}
    end
  end

  setup do
    counter = :counters.new(1, [])
    ctx = {counter, self(), @total}
    :persistent_term.put({__MODULE__, :ctx}, ctx)
    on_exit(fn -> :persistent_term.erase({__MODULE__, :ctx}) end)
    %{counter: counter}
  end

  describe "a client that doesn't read" do
    test "holds back GRPC.Server.send_reply/3", %{counter: counter} do
      with_stalled_client(1, fn _producer, handler, _socket ->
        {sent, samples} = settle_and_sample(handler, counter)

        assert sent <= @held, "the producer sent #{sent} messages, at most #{@held} fit"
        assert_bounded(samples, @held)
        assert Enum.max_by(samples, & &1.queued).queued <= 1 * @message_size
      end)
    end

    test "holds back GRPC.Stream.run_with/3", %{counter: counter} do
      with_stalled_client(2, fn _producer, handler, _socket ->
        {sent, samples} = settle_and_sample(handler, counter)

        assert sent <= @held + @stages, "the stream sent #{sent} messages"
        assert_bounded(samples, @held + @stages)
        assert Enum.max_by(samples, & &1.queued).queued <= @stages * @message_size
      end)
    end

    test "releases the producer, with an exit, when the client disconnects", %{counter: counter} do
      with_stalled_client(3, fn producer, handler, socket ->
        settle_and_sample(handler, counter)

        :ok = :gen_tcp.close(socket)

        # Cowboy shuts the stream process down, and kills it after `shutdown_timeout` because
        # the process is blocked and can't see the shutdown. That is the bound.
        assert_receive {:producer_stopped, ^producer, :exit, :shutdown}, @shutdown_timeout + 3_000
        refute_received {:producer_done, ^producer}
      end)
    end
  end

  describe "a client that reads" do
    test "gets every reply from GRPC.Server.send_reply/3" do
      assert read_all(1) == @total
    end

    test "gets every reply from GRPC.Stream.run_with/3" do
      assert read_all(2) == @total
    end
  end

  describe "the wait for a client that doesn't read" do
    test "ends after :send_timeout" do
      with_stalled_client(
        3,
        fn producer, _handler, _socket ->
          assert_receive {:producer_stopped, ^producer, :error,
                          %GRPC.RPCError{status: 4, message: message}},
                         5_000

          assert message =~ "Timed out"
        end,
        adapter_opts: [send_timeout: 200] ++ @buffers
      )
    end

    test "ends at the call's deadline" do
      with_stalled_client(
        3,
        fn producer, _handler, _socket ->
          assert_receive {:producer_stopped, ^producer, :error, %GRPC.RPCError{status: 4}}, 5_000
        end,
        headers: [{"grpc-timeout", "200m"}]
      )
    end

    test "does not leave a late ack in the sender's mailbox" do
      with_stalled_client(
        4,
        fn producer, _handler, socket ->
          assert_receive {:timed_out, ^producer}, 5_000

          open_window(socket)
          send(producer, :window_open)

          assert_receive {:mailbox, ^producer, messages}, 5_000

          refute Enum.any?(messages, &stray_ack?/1),
                 "late ack in the mailbox: #{inspect(messages)}"
        end,
        adapter_opts: [send_timeout: 200] ++ @buffers
      )
    end

    test "does not queue a reply when the deadline has passed" do
      payload = %{pid: self(), send_timeout: nil, deadline: System.monotonic_time(:millisecond)}

      error =
        assert_raise GRPC.RPCError, fn ->
          GRPC.Server.Adapters.Cowboy.send_reply(payload, %Routeguide.Feature{}, [])
        end

      assert error.status == GRPC.Status.deadline_exceeded()
      refute_received {:stream_body, _, _, _, _, _}
    end

    test "waits at most the time left to the deadline" do
      deadline = System.monotonic_time(:millisecond) + 60_000
      wait = Handler.send_timeout(%{send_timeout: nil, deadline: deadline})
      assert wait in 1..60_000
      assert Handler.send_timeout(%{send_timeout: 100, deadline: deadline}) == 100
    end

    test "a unary reply that times out gets DEADLINE_EXCEEDED, not UNKNOWN" do
      attach_telemetry(@send_error)

      run_server(
        BigReplyServer,
        fn port ->
          point = Protobuf.encode(%Routeguide.Point{})
          socket = open_call(port, "/routeguide.RouteGuide/GetFeature", point, [])

          # The send timed out. The stream process is still stuck, so the client can't see
          # the status yet.
          Process.sleep(1500)
          assert_receive {:telemetry, @send_error, _, %{reason: %GRPC.RPCError{status: 4}}}, 5_000

          open_window(socket)
          assert %{"grpc-status" => "4"} = read_trailers(socket)
          :gen_tcp.close(socket)
        end,
        0,
        adapter_opts: [send_timeout: 200] ++ @buffers
      )
    end

    test ":send_timeout is checked when the server starts" do
      assert_raise ArgumentError, ~r/invalid :send_timeout/, fn ->
        GRPC.Server.Adapters.Cowboy.child_spec(nil, %{"x" => FloodServer}, 0,
          adapter_opts: [send_timeout: -1]
        )
      end
    end
  end

  # Serves FloodServer, and opens a ListFeatures call that the client never reads.
  defp with_stalled_client(way, fun, opts \\ []) do
    adapter_opts =
      Keyword.merge(Keyword.get(opts, :adapter_opts, @buffers),
        shutdown_timeout: @shutdown_timeout
      )

    headers = Keyword.get(opts, :headers, [])

    run_server(
      FloodServer,
      fn port ->
        socket = open_stalled(port, way, headers)
        assert_receive {:producer, producer, handler}, 5_000
        fun.(producer, handler, socket)

        # Let the server finish with this call before the test ends.
        refs = for pid <- [producer, handler], do: Process.monitor(pid)
        :gen_tcp.close(socket)

        for ref <- refs do
          assert_receive {:DOWN, ^ref, :process, _, _}, @shutdown_timeout + 3_000
        end
      end,
      0,
      adapter_opts: adapter_opts
    )
  end

  # Waits until the stream process waits on cowboy and the producer has stopped moving, then takes
  # five samples of the stream process's queue.
  defp settle_and_sample(handler, counter) do
    :ok = await_settled(handler, counter, 5, :counters.get(counter, 1), deadline())

    samples =
      for _ <- 1..5 do
        sample = sample(handler)
        Process.sleep(5)
        sample
      end

    sent = :counters.get(counter, 1)
    {sent, samples}
  end

  defp await_settled(_handler, _counter, 0, _last, _deadline), do: :ok

  defp await_settled(handler, counter, steady, last, deadline) do
    if System.monotonic_time(:millisecond) > deadline, do: flunk("the sender never settled")

    Process.sleep(10)
    now = :counters.get(counter, 1)

    steady =
      if now == last and blocked?(handler), do: steady - 1, else: 5

    await_settled(handler, counter, steady, now, deadline)
  end

  defp blocked?(handler) do
    case Process.info(handler, :current_function) do
      {:current_function, {:cowboy_req, :stream_body, 2}} -> true
      _ -> false
    end
  end

  defp deadline, do: System.monotonic_time(:millisecond) + 10_000

  # The messages waiting in the stream process, and the bytes of reply data in them.
  defp sample(handler) do
    {:messages, messages} = Process.info(handler, :messages)

    queued =
      for {:stream_body, data, _opts, _fin, _transcode} <- Enum.map(messages, &strip_ack/1),
          reduce: 0,
          do: (acc -> acc + IO.iodata_length(data))

    %{len: length(messages), queued: queued}
  end

  # The branch's messages end with the sender's ack; master's don't.
  defp strip_ack({:stream_body, d, o, f, t, _ack}), do: {:stream_body, d, o, f, t}
  defp strip_ack(other), do: other

  defp assert_bounded(samples, limit) do
    for %{len: len} <- samples do
      assert len <= limit, "the stream process had #{len} messages queued"
    end
  end

  defp stray_ack?([_alias | :stream_body_ack]), do: true
  defp stray_ack?({_ref, :stream_body_ack}), do: true
  defp stray_ack?(_other), do: false

  defp open_stalled(port, way, extra_headers) do
    body = Protobuf.encode(%Routeguide.Rectangle{lo: %Routeguide.Point{latitude: way}})
    open_call(port, "/routeguide.RouteGuide/ListFeatures", body, extra_headers)
  end

  # Opens a call on a raw socket, and reads nothing.
  defp open_call(port, path, body, extra_headers) do
    {:ok, socket} = :gen_tcp.connect(~c"localhost", port, [:binary, active: false, nodelay: true])

    headers = [
      {":method", "POST"},
      {":scheme", "http"},
      {":path", path},
      {":authority", "localhost:#{port}"},
      {"content-type", "application/grpc"},
      {"te", "trailers"}
      | extra_headers
    ]

    {block, _} = :cow_hpack.encode(headers, :cow_hpack.init())

    :ok =
      :gen_tcp.send(socket, [
        "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n",
        :cow_http2.settings(%{}),
        :cow_http2.headers(1, :nofin, block),
        :cow_http2.data(1, :fin, <<0, byte_size(body)::32, body::binary>>)
      ])

    socket
  end

  defp open_window(socket) do
    :ok =
      :gen_tcp.send(socket, [
        :cow_http2.window_update(10_000_000),
        :cow_http2.window_update(1, 10_000_000)
      ])
  end

  # Reads frames until the stream ends, and returns the trailers.
  defp read_trailers(socket), do: read_frames(socket, <<>>, :cow_hpack.init(), %{})

  defp read_frames(socket, buffer, hpack, acc) do
    case :cow_http2.parse(buffer) do
      {:ok, {:headers, 1, fin, _head_fin, block}, rest} ->
        {headers, hpack} = :cow_hpack.decode(block, hpack)
        acc = Map.merge(acc, Map.new(headers))
        if fin == :fin, do: acc, else: read_frames(socket, rest, hpack, acc)

      {:ok, {:data, 1, :fin, _data}, _rest} ->
        acc

      {:ok, _frame, rest} ->
        read_frames(socket, rest, hpack, acc)

      {:ignore, rest} ->
        read_frames(socket, rest, hpack, acc)

      _more ->
        {:ok, data} = :gen_tcp.recv(socket, 0, 5_000)
        read_frames(socket, buffer <> data, hpack, acc)
    end
  end

  # Reads a whole reply stream with gun, which credits the server's windows as it reads.
  defp read_all(way) do
    run_server(FloodServer, fn port ->
      {:ok, conn} = :gun.open(~c"localhost", port, %{protocols: [:http2]})
      {:ok, :http2} = :gun.await_up(conn, 5_000)
      body = Protobuf.encode(%Routeguide.Rectangle{lo: %Routeguide.Point{latitude: way}})

      ref =
        :gun.post(
          conn,
          "/routeguide.RouteGuide/ListFeatures",
          [{"content-type", "application/grpc"}, {"te", "trailers"}],
          <<0, byte_size(body)::32, body::binary>>
        )

      {bytes, status} = collect(conn, ref, <<>>, nil)
      :gun.close(conn)
      assert status == "0"
      count_messages(bytes, 0)
    end)
  end

  defp collect(conn, ref, acc, status) do
    case :gun.await(conn, ref, 10_000) do
      {:response, _, _, _} -> collect(conn, ref, acc, status)
      {:data, :fin, data} -> {acc <> data, status}
      {:data, :nofin, data} -> collect(conn, ref, acc <> data, status)
      {:trailers, trailers} -> {acc, :proplists.get_value("grpc-status", trailers)}
    end
  end

  defp count_messages(<<>>, n), do: n

  defp count_messages(<<0, size::32, _::binary-size(size), rest::binary>>, n),
    do: count_messages(rest, n + 1)
end
