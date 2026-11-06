defmodule GRPC.Client.Adapters.GunFlowControlTest do
  # The Gun adapter used to buffer a response without limit when the caller read
  # slower than the server sent (#599). Streams are now opened with a finite
  # Gun `flow` and credited when the caller takes a message off the queue.
  use GRPC.Client.DataCase, async: true

  alias GRPC.Client.Adapters.Gun
  alias Routeguide.RouteGuide.Stub

  @message_size 64 * 1024
  @messages 128
  # Gun counts `flow` in DATA frames, and a frame holds at most 16 KiB by default.
  @frame_size 16_384

  defmodule Server do
    use GRPC.Server, service: Routeguide.RouteGuide.Service
    alias GRPC.Server

    def get_feature(%{latitude: size}, _stream) do
      %Routeguide.Feature{name: String.duplicate("a", size)}
    end

    def list_features(%{lo: %{latitude: count, longitude: size}}, stream) do
      name = String.duplicate("a", size)

      Enum.each(1..count, fn _ ->
        Server.send_reply(stream, %Routeguide.Feature{name: name})
      end)
    end

    def route_chat(req_enum, stream) do
      Enum.each(req_enum, fn note ->
        Server.send_reply(stream, %Routeguide.RouteNote{
          message: String.duplicate("a", note.location.longitude)
        })
      end)
    end
  end

  setup do
    {:ok, _, port} = GRPC.Server.start(Server, 0)
    on_exit(fn -> GRPC.Server.stop(Server) end)
    %{port: port}
  end

  defp connect(port, opts) do
    channel = build(:channel, port: port, host: "localhost")
    {:ok, channel} = Gun.connect(channel, opts)
    on_exit(fn -> Gun.disconnect(channel) end)
    channel
  end

  defp list_request(count, size) do
    %Routeguide.Rectangle{lo: %Routeguide.Point{latitude: count, longitude: size}}
  end

  describe "slow consumer" do
    test "the response queue stays within the response window", %{port: port} do
      window = 256 * 1024
      channel = connect(port, response_window: window)
      conn_pid = channel.adapter_payload.conn_pid
      parent = self()

      # The consumer reads one message and then stops reading.
      task =
        Task.async(fn ->
          {:ok, replies} = Stub.list_features(channel, list_request(@messages, @message_size))

          Enum.reduce(replies, 0, fn {:ok, _feature}, count ->
            if count == 0 do
              send(parent, {:first_message, self()})
              receive do: (:resume -> :ok)
            end

            count + 1
          end)
        end)

      assert_receive {:first_message, consumer}, 10_000
      {_stream_ref, response_pid} = wait_for_response_process(conn_pid)

      queued = settled_queue_bytes(response_pid)

      # Gun withholds the stream's WINDOW_UPDATE once `flow` is used up, so up to one
      # stream window (set to the response window) is still in flight.
      bound = div(window + @frame_size - 1, @frame_size) * @frame_size + window + @frame_size
      assert queued > 0, "the server sent nothing"
      assert queued <= bound, "queued #{queued} bytes, expected at most #{bound}"

      # Reading again drains the rest, and the server's remaining messages arrive.
      send(consumer, :resume)
      assert Task.await(task, 30_000) == @messages
    end
  end

  describe "consumers that keep up" do
    test "a fast consumer reads every message with the default window", %{port: port} do
      channel = connect(port, [])

      {:ok, replies} = Stub.list_features(channel, list_request(@messages, @message_size))
      features = Enum.to_list(replies)

      assert length(features) == @messages

      assert Enum.all?(
               features,
               &match?({:ok, %{name: name}} when byte_size(name) == @message_size, &1)
             )
    end

    test "a unary reply larger than the window completes", %{port: port} do
      size = 3 * 1024 * 1024
      channel = connect(port, response_window: 64 * 1024)

      assert {:ok, %Routeguide.Feature{name: name}} =
               Stub.get_feature(channel, %Routeguide.Point{latitude: size}, timeout: 30_000)

      assert byte_size(name) == size
    end

    test "a bidi stream with replies larger than the window completes", %{port: port} do
      channel = connect(port, response_window: 64 * 1024)
      size = 256 * 1024
      notes = 10

      stream = Stub.route_chat(channel)

      for _ <- 1..notes do
        GRPC.Stub.send_request(stream, %Routeguide.RouteNote{
          location: %Routeguide.Point{longitude: size}
        })
      end

      GRPC.Stub.end_stream(stream)
      {:ok, replies} = GRPC.Stub.recv(stream, timeout: 30_000)
      replies = Enum.to_list(replies)

      assert length(replies) == notes
      assert Enum.all?(replies, &match?({:ok, %{message: m}} when byte_size(m) == size, &1))
    end
  end

  describe ":response_window option" do
    test "defaults to 1 MiB, which is 64 frames of 16 KiB", %{port: port} do
      channel = connect(port, [])
      assert :sys.get_state(channel.adapter_payload.conn_pid).flow == 64
    end

    test "is rounded up to whole frames", %{port: port} do
      channel = connect(port, response_window: 1)
      assert :sys.get_state(channel.adapter_payload.conn_pid).flow == 1

      channel = connect(port, response_window: 16_385)
      assert :sys.get_state(channel.adapter_payload.conn_pid).flow == 2
    end

    test ":infinity turns the limit off", %{port: port} do
      channel = connect(port, response_window: :infinity)
      assert :sys.get_state(channel.adapter_payload.conn_pid).flow == :infinity
    end

    test "a bad value is rejected by GRPC.Stub.connect/2", %{port: port} do
      assert_raise ArgumentError, ~r/:response_window/, fn ->
        GRPC.Stub.connect("localhost:#{port}", adapter_opts: [response_window: 0])
      end
    end

    test "is validated" do
      assert :ok = Gun.validate_opts(response_window: 1)
      assert :ok = Gun.validate_opts(response_window: :infinity)
      assert :ok = Gun.validate_opts([])

      for bad <- [0, -1, 1.5, "1", nil] do
        assert {:error, message} = Gun.validate_opts(response_window: bad)
        assert message =~ ":response_window"
      end
    end
  end

  # Samples the queue until its size stops changing. A stalled consumer ends in a
  # steady state, so ten equal samples in a row mean the sender has stopped.
  defp settled_queue_bytes(response_pid, previous \\ nil, repeats \\ 0)

  defp settled_queue_bytes(_response_pid, previous, 10), do: previous

  defp settled_queue_bytes(response_pid, previous, repeats) do
    Process.sleep(20)
    current = queue_bytes(response_pid)
    repeats = if current == previous, do: repeats + 1, else: 0
    settled_queue_bytes(response_pid, current, repeats)
  end

  defp queue_bytes(response_pid) do
    response_pid
    |> :sys.get_state()
    |> Map.fetch!(:messages)
    |> :queue.to_list()
    |> Enum.reduce(0, fn
      {:data, _fin, data}, total -> total + byte_size(data)
      _other, total -> total
    end)
  end

  defp wait_for_response_process(conn_pid, attempts \\ 500)

  defp wait_for_response_process(_conn_pid, 0), do: flunk("request was not dispatched")

  defp wait_for_response_process(conn_pid, attempts) do
    case :sys.get_state(conn_pid).response_processes do
      processes when map_size(processes) == 0 ->
        Process.sleep(10)
        wait_for_response_process(conn_pid, attempts - 1)

      processes ->
        [{stream_ref, %{response_pid: response_pid}}] = Map.to_list(processes)
        {stream_ref, response_pid}
    end
  end
end
