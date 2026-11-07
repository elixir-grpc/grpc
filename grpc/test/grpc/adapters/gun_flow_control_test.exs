defmodule GRPC.Client.Adapters.GunFlowControlTest do
  # The Gun adapter used to buffer a response without limit when the caller read
  # slower than the server sent (#599). Streams are now opened with a finite
  # Gun `flow` and credited when the caller takes a message off the queue.
  use GRPC.Client.DataCase, async: true

  alias GRPC.Client.Adapters.Gun
  alias Routeguide.RouteGuide.Stub

  @message_size 64 * 1024
  # 96 messages of 64 KiB stay under cowboy's 8,000,000 byte stream buffer, so the
  # server handler finishes sending while the client reads nothing.
  @messages 96
  # Gun counts `flow` in DATA frames, and a frame holds at most 16 KiB by default.
  @frame_size 16_384
  # `cow_http2_machine:ensure_window/3` adds this margin to each stream window update.
  @cowlib_margin 65_535
  # One frame of 8 bytes: a 5 byte gRPC prefix and a 3 byte `Feature` with a 1 byte name.
  @small_message_bytes 8

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

      notify(stream, {:sent_all, count})
    end

    # The note's latitude is its index, and its longitude is the size of the reply.
    def route_chat(req_enum, stream) do
      Enum.each(req_enum, fn note ->
        Server.send_reply(stream, %Routeguide.RouteNote{
          message: String.duplicate("a", note.location.longitude)
        })

        notify(stream, {:replied, note.location.latitude})
      end)
    end

    # Tells the test process, which sends its pid in the `x-notify` metadata, that
    # `send_reply` returned. It returns once cowboy has taken the data.
    defp notify(stream, message) do
      case GRPC.Stream.get_headers(stream) do
        %{"x-notify" => encoded} ->
          send(encoded |> Base.decode64!() |> :erlang.binary_to_term(), message)

        _ ->
          :ok
      end

      :ok
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

  defp notify_metadata, do: %{"x-notify" => Base.encode64(:erlang.term_to_binary(self()))}

  # Opens a bidi stream and sends `notes` requests that each ask for a reply of
  # `size` bytes. Returns after the server handler has sent every reply, with the
  # response queue settled.
  defp open_stalled_chat(channel, window, notes, size) do
    stream = Stub.route_chat(channel, metadata: notify_metadata())

    for index <- 1..notes do
      GRPC.Stub.send_request(stream, %Routeguide.RouteNote{
        location: %Routeguide.Point{latitude: index, longitude: size}
      })
    end

    assert_receive {:replied, ^notes}, 10_000

    {_stream_ref, response_pid} = wait_for_response_process(channel.adapter_payload.conn_pid)
    {entries, bytes} = settled_queue(channel.adapter_payload.conn_pid, response_pid)

    assert entries > 0, "the server sent nothing"
    assert bytes <= queue_bound(window)
    assert bytes < notes * size, "the whole reply is queued, so the window did not fill"

    {stream, response_pid}
  end

  describe "slow consumer" do
    test "the response queue stays within the response window", %{port: port} do
      window = 256 * 1024
      channel = connect(port, response_window: window)
      conn_pid = channel.adapter_payload.conn_pid
      metadata = notify_metadata()
      parent = self()

      # The consumer reads one message and then stops reading.
      task =
        Task.async(fn ->
          {:ok, replies} =
            Stub.list_features(channel, list_request(@messages, @message_size),
              metadata: metadata
            )

          Enum.reduce(replies, 0, fn {:ok, _feature}, count ->
            if count == 0 do
              send(parent, {:first_message, self()})
              receive do: (:resume -> :ok)
            end

            count + 1
          end)
        end)

      assert_receive {:first_message, consumer}, 10_000
      assert_receive {:sent_all, @messages}, 10_000
      {_stream_ref, response_pid} = wait_for_response_process(conn_pid)

      {entries, queued} = settled_queue(conn_pid, response_pid)

      assert entries > 0, "the server sent nothing"
      bound = queue_bound(window)
      assert queued <= bound, "queued #{queued} bytes, expected at most #{bound}"

      assert queued < @messages * @message_size,
             "the whole reply is queued, so the window did not fill"

      # Reading again drains the rest, and the server's remaining messages arrive.
      send(consumer, :resume)
      assert Task.await(task, 30_000) == @messages
    end

    test "many small messages queue a bounded number of entries", %{port: port} do
      window = @frame_size
      messages = 5_000

      # A zero margin makes the bound exact: with one frame of credit, Gun never
      # reopens the stream window, so the peer sends `window` bytes and stops.
      channel =
        connect(port,
          response_window: window,
          http2_opts: %{stream_window_margin_size: 0}
        )

      conn_pid = channel.adapter_payload.conn_pid

      # The caller takes headers only, so no frame is credited.
      {:ok, replies} =
        Stub.list_features(channel, list_request(messages, 1), metadata: notify_metadata())

      assert_receive {:sent_all, ^messages}, 10_000
      {_stream_ref, response_pid} = wait_for_response_process(conn_pid)

      {entries, _bytes} = settled_queue(conn_pid, response_pid)

      bound = div(window, @frame_size) + div(window, @small_message_bytes)
      assert entries > 0, "the server sent nothing"
      assert entries <= bound, "queued #{entries} entries, expected at most #{bound}"
      assert entries < messages, "every message is queued, so the window did not fill"

      # The messages not queued yet arrive when the caller reads.
      assert replies |> Enum.to_list() |> length() == messages
    end
  end

  describe "stalled stream" do
    test "cancelling it stops the response process and leaves the channel usable", %{port: port} do
      window = 4 * @frame_size
      channel = connect(port, response_window: window)
      conn_pid = channel.adapter_payload.conn_pid
      {stream, response_pid} = open_stalled_chat(channel, window, 40, @message_size)

      monitor_ref = Process.monitor(response_pid)
      canceled = GRPC.Stub.cancel(stream)

      assert canceled.canceled
      assert_receive {:DOWN, ^monitor_ref, :process, ^response_pid, :normal}, 5_000
      assert :sys.get_state(conn_pid).response_processes == %{}
      assert Process.alive?(conn_pid)

      assert {:error, %GRPC.RPCError{status: status}} = GRPC.Stub.recv(canceled)
      assert status == GRPC.Status.cancelled()

      assert_channel_usable(channel)
    end

    test "a deadline that expires stops the response process and leaves the channel usable", %{
      port: port
    } do
      window = 4 * @frame_size
      channel = connect(port, response_window: window)
      conn_pid = channel.adapter_payload.conn_pid
      {stream, response_pid} = open_stalled_chat(channel, window, 40, @message_size)

      # The deadline starts here, after the window is full. `recv/2` takes the headers
      # only, so nothing is credited and the stream stays stalled until it expires.
      monitor_ref = Process.monitor(response_pid)
      assert {:ok, _replies} = GRPC.Stub.recv(stream, timeout: 200)

      assert_receive {:DOWN, ^monitor_ref, :process, ^response_pid, :normal}, 5_000
      assert :sys.get_state(conn_pid).response_processes == %{}
      assert Process.alive?(conn_pid)

      assert_channel_usable(channel)
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

  describe "the least windows" do
    # A cowlib sender holds data back while the stream window is under 16,384
    # bytes and the data does not fit. Smaller windows are not accepted, because
    # a reply larger than the window never arrives from such a peer.
    for window <- [16_384, 16_385, 20_000] do
      @window window

      test "a window of #{window} bytes delivers small and large replies", %{port: port} do
        channel = connect(port, response_window: @window)

        {:ok, replies} =
          Stub.list_features(channel, list_request(20, 1_000), timeout: 10_000)

        assert [{:ok, %{name: name}} | _] = replies = Enum.to_list(replies)
        assert length(replies) == 20
        assert byte_size(name) == 1_000

        {:ok, replies} =
          Stub.list_features(channel, list_request(3, 100_000), timeout: 10_000)

        replies = Enum.to_list(replies)
        assert length(replies) == 3
        assert Enum.all?(replies, &match?({:ok, %{name: n}} when byte_size(n) == 100_000, &1))

        size = 1024 * 1024

        assert {:ok, %Routeguide.Feature{name: big}} =
                 Stub.get_feature(channel, %Routeguide.Point{latitude: size}, timeout: 30_000)

        assert byte_size(big) == size
      end
    end
  end

  describe ":response_window option" do
    test "defaults to 1 MiB, which is 64 frames of 16 KiB", %{port: port} do
      channel = connect(port, [])
      assert :sys.get_state(channel.adapter_payload.conn_pid).flow == 64
    end

    test "is rounded up to whole frames", %{port: port} do
      channel = connect(port, response_window: 16_384)
      assert :sys.get_state(channel.adapter_payload.conn_pid).flow == 1

      channel = connect(port, response_window: 16_385)
      assert :sys.get_state(channel.adapter_payload.conn_pid).flow == 2
    end

    test ":infinity turns the limit off", %{port: port} do
      channel = connect(port, response_window: :infinity)
      assert :sys.get_state(channel.adapter_payload.conn_pid).flow == :infinity
    end

    test "a window under one frame is rejected by GRPC.Stub.connect/2", %{port: port} do
      for bad <- [100, 16_383, 0] do
        assert_raise ArgumentError, ~r/:response_window.*16384/, fn ->
          GRPC.Stub.connect("localhost:#{port}", adapter_opts: [response_window: bad])
        end
      end
    end

    test "is validated" do
      assert :ok = Gun.validate_opts(response_window: 16_384)
      assert :ok = Gun.validate_opts(response_window: :infinity)
      assert :ok = Gun.validate_opts([])

      for bad <- [0, -1, 1, 100, 16_383, 1.5, "1", nil] do
        assert {:error, message} = Gun.validate_opts(response_window: bad)
        assert message =~ ":response_window"
        assert message =~ "16384"
      end
    end
  end

  describe "http2_opts initial_stream_window_size" do
    test "a value at the floor delivers small and large replies", %{port: port} do
      channel = connect(port, http2_opts: %{initial_stream_window_size: 16_384})

      {:ok, replies} = Stub.list_features(channel, list_request(20, 1_000), timeout: 10_000)
      replies = Enum.to_list(replies)
      assert length(replies) == 20
      assert Enum.all?(replies, &match?({:ok, %{name: n}} when byte_size(n) == 1_000, &1))

      size = 1024 * 1024

      assert {:ok, %Routeguide.Feature{name: big}} =
               Stub.get_feature(channel, %Routeguide.Point{latitude: size}, timeout: 30_000)

      assert byte_size(big) == size
    end

    test "is validated" do
      for ok <- [16_384, 16_385, 8_000_000] do
        assert :ok = Gun.validate_opts(http2_opts: %{initial_stream_window_size: ok})
      end

      assert :ok = Gun.validate_opts(http2_opts: %{})
      assert :ok = Gun.validate_opts(http2_opts: %{stream_window_margin_size: 0})

      for bad <- [0, 1, 16_383, -1, 1.5, "16384", nil] do
        assert {:error, message} =
                 Gun.validate_opts(http2_opts: %{initial_stream_window_size: bad})

        assert message =~ ":initial_stream_window_size"
        assert message =~ "16384"
        assert message =~ inspect(bad)
      end
    end

    test "is checked when :response_window is valid and when it is not" do
      assert {:error, message} =
               Gun.validate_opts(
                 response_window: :infinity,
                 http2_opts: %{initial_stream_window_size: 100}
               )

      assert message =~ ":initial_stream_window_size"

      assert {:error, message} =
               Gun.validate_opts(
                 response_window: 100,
                 http2_opts: %{initial_stream_window_size: 100}
               )

      assert message =~ ":response_window"
    end

    test "a value under the floor is rejected by GRPC.Stub.connect/2", %{port: port} do
      assert_raise ArgumentError, ~r/:initial_stream_window_size.*16384/, fn ->
        GRPC.Stub.connect("localhost:#{port}",
          adapter_opts: [http2_opts: %{initial_stream_window_size: 16_383}]
        )
      end
    end
  end

  describe "http2_opts initial_connection_window_size" do
    # A cowlib sender holds data back only when the stream window is small, so
    # the connection window has no floor here.
    test "is not limited by validate_opts" do
      for size <- [0, 1_000, 16_383, 65_535, 8_000_000] do
        assert :ok = Gun.validate_opts(http2_opts: %{initial_connection_window_size: size})
      end
    end
  end

  # What the response queue can hold: the frames Gun credits, plus the HTTP/2 stream
  # window (the same size as the response window) and the margin cowlib adds to
  # each stream window update. One frame of slack covers a partly read message.
  defp queue_bound(window) do
    frames = div(window + @frame_size - 1, @frame_size)
    frames * @frame_size + window + @cowlib_margin + @frame_size
  end

  # Returns the queue size once the sender has stopped. A PING round trip is the
  # barrier: Gun reads the peer's frames in order, so every DATA frame the peer
  # sent before it answered is in the queue when the ack arrives. When two
  # barriers in a row see the same queue, nothing more is coming. The test calls
  # this after the server handler has sent everything, so the peer has no more to add.
  defp settled_queue(conn_pid, response_pid) do
    gun_pid = :sys.get_state(conn_pid).gun_pid
    settled_queue(gun_pid, response_pid, nil, 200)
  end

  defp settled_queue(_gun_pid, _response_pid, _previous, 0), do: flunk("the queue did not settle")

  defp settled_queue(gun_pid, response_pid, previous, attempts) do
    ref = :gun.ping(gun_pid)
    assert_receive {:gun_notify, ^gun_pid, :ping_ack, ^ref}, 5_000
    current = queue_stats(response_pid)

    if current == previous do
      current
    else
      settled_queue(gun_pid, response_pid, current, attempts - 1)
    end
  end

  # `:sys.get_state/1` also makes sure the process handled the messages Gun sent before.
  defp queue_stats(response_pid) do
    response_pid
    |> :sys.get_state()
    |> Map.fetch!(:messages)
    |> :queue.to_list()
    |> Enum.reduce({0, 0}, fn
      {:data, _fin, data}, {entries, bytes} -> {entries + 1, bytes + byte_size(data)}
      _other, acc -> acc
    end)
  end

  defp assert_channel_usable(channel) do
    size = 200 * 1024

    assert {:ok, %Routeguide.Feature{name: name}} =
             Stub.get_feature(channel, %Routeguide.Point{latitude: size}, timeout: 10_000)

    assert byte_size(name) == size
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
