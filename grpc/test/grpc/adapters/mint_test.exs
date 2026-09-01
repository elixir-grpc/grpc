defmodule GRPC.Client.Adapters.MintTest do
  use GRPC.Client.DataCase, async: false

  alias GRPC.Client.Adapters.Mint

  setup do
    {:ok, _, port} = GRPC.Server.start(FeatureServer, 0)

    on_exit(fn ->
      :ok = GRPC.Server.stop(FeatureServer)
    end)

    %{port: port}
  end

  describe "connect/2" do
    test "connects insecurely (default options)", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      assert {:ok, result} = Mint.connect(channel, [])
      assert %{channel | adapter_payload: %{conn_pid: result.adapter_payload.conn_pid}} == result
    end

    test "connects insecurely (custom options)", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      assert {:ok, result} = Mint.connect(channel, transport_opts: [ip: :loopback])
      assert %{channel | adapter_payload: %{conn_pid: result.adapter_payload.conn_pid}} == result

      # Ensure that changing one of the options breaks things
      assert {:error, :badarg} = Mint.connect(channel, transport_opts: [ip: "256.0.0.0"])
    end

    test "accepts config_options for application specific configuration", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      assert {:ok, result} =
               Mint.connect(channel, config_options: [transport_opts: [ip: :loopback]])

      assert %{channel | adapter_payload: %{conn_pid: result.adapter_payload.conn_pid}} == result

      # Ensure that changing one of the options via config_options also breaks things
      assert {:error, :badarg} =
               Mint.connect(channel, config_options: [transport_opts: [ip: "256.0.0.0"]])
    end

    test "defaults client settings when none is passed", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      assert {:ok, result} = Mint.connect(channel, [])
      state = :sys.get_state(result.adapter_payload.conn_pid)

      # Mint mirrors advertised client_settings onto conn at connect time.
      assert state.connect_opts[:client_settings] == [
               initial_window_size: 8_000_000,
               max_frame_size: 8_000_000
             ]

      assert %{initial_window_size: 8_000_000, max_frame_size: 8_000_000} =
               Map.take(state.conn.client_settings, [:initial_window_size, :max_frame_size])
    end

    test "allow client settings to be passed", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      assert {:ok, result} =
               Mint.connect(channel,
                 client_settings: [
                   initial_window_size: 50_000,
                   max_frame_size: 50_000
                 ]
               )

      state = :sys.get_state(result.adapter_payload.conn_pid)

      assert state.connect_opts[:client_settings] == [
               initial_window_size: 50_000,
               max_frame_size: 50_000
             ]

      assert %{initial_window_size: 50_000, max_frame_size: 50_000} =
               Map.take(state.conn.client_settings, [:initial_window_size, :max_frame_size])
    end
  end

  describe "disconnect/1" do
    test "keeps adapter_payload as a map with conn_pid set to nil", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      {:ok, connected} = Mint.connect(channel, [])
      assert %{conn_pid: pid} = connected.adapter_payload
      assert is_pid(pid)

      {:ok, disconnected} = Mint.disconnect(connected)

      assert %{conn_pid: nil} = disconnected.adapter_payload
    end

    test "disconnect is idempotent — calling it twice succeeds", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      {:ok, connected} = Mint.connect(channel, [])
      {:ok, disconnected} = Mint.disconnect(connected)
      {:ok, disconnected_again} = Mint.disconnect(disconnected)

      assert %{conn_pid: nil} = disconnected_again.adapter_payload
    end

    test "send_request/3 raises ArgumentError when conn_pid is nil", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      {:ok, connected} = Mint.connect(channel, [])
      {:ok, disconnected} = Mint.disconnect(connected)

      stream = build(:client_stream, channel: disconnected)

      assert_raise ArgumentError, ~r/Can't perform a request without a connection process/, fn ->
        Mint.send_request(stream, %Helloworld.HelloRequest{name: "test"}, [])
      end
    end
  end

  describe "handle_errors_receive_data/2" do
    test "returns a GRPC.RPCError with unknown status" do
      response = {:error, :closed}
      stream = build(:client_stream, payload: %{response: response})

      assert {:error, %GRPC.RPCError{status: status, message: message}} =
               Mint.handle_errors_receive_data(stream, [])

      assert status == GRPC.Status.unknown()
      assert message == "error occurred while receiving data: #{inspect(response)}"
    end
  end

  describe "receive_data/2 - deadline" do
    setup do
      {:ok, stream_response_pid} =
        GRPC.Client.Adapters.Mint.StreamResponseProcess.start_link(build(:client_stream), true)

      # A connection process that is already gone: nothing will ever notify the
      # stream response process, which is what used to block the caller forever.
      dead_conn_pid = spawn(fn -> :ok end)
      ref = Process.monitor(dead_conn_pid)
      assert_receive {:DOWN, ^ref, :process, ^dead_conn_pid, _reason}

      stream =
        build(:client_stream,
          channel: build(:channel, adapter: Mint, adapter_payload: %{conn_pid: dead_conn_pid}),
          payload: %{
            stream_response_pid: stream_response_pid,
            response: {:ok, %{request_ref: make_ref()}}
          }
        )

      %{stream: stream, stream_response_pid: stream_response_pid}
    end

    # Without a deadline these would block until the ExUnit timeout, so keep that
    # wait short enough to read as a failure rather than as a stuck suite.
    @describetag timeout: 5_000

    test "returns DEADLINE_EXCEEDED when no response arrives in time", %{stream: stream} do
      assert {:error, %GRPC.RPCError{status: status, message: message}} =
               Mint.receive_data(stream, timeout: 10)

      assert status == GRPC.Status.deadline_exceeded()
      assert message == "deadline exceeded"
    end

    test "stops the stream response process it gave up on", %{
      stream: stream,
      stream_response_pid: stream_response_pid
    } do
      assert {:error, %GRPC.RPCError{}} = Mint.receive_data(stream, timeout: 10)

      refute Process.alive?(stream_response_pid)
    end

    test "accepts the milliseconds a :deadline is resolved into", %{stream: stream} do
      timeout = GRPC.TimeUtils.to_relative(DateTime.add(DateTime.utc_now(), 20, :millisecond))

      assert is_number(timeout)

      assert {:error, %GRPC.RPCError{status: status}} =
               Mint.receive_data(stream, timeout: timeout)

      assert status == GRPC.Status.deadline_exceeded()
    end

    test "treats a deadline that has already passed as an immediate one", %{stream: stream} do
      timeout = GRPC.TimeUtils.to_relative(DateTime.add(DateTime.utc_now(), -5, :second))

      assert timeout < 0

      assert {:error, %GRPC.RPCError{status: status}} =
               Mint.receive_data(stream, timeout: timeout)

      assert status == GRPC.Status.deadline_exceeded()
    end

    test "lets an explicit :deadline override the timeout GRPC.Stub fills in", %{stream: stream} do
      assert {:error, %GRPC.RPCError{status: status}} =
               Mint.receive_data(stream, timeout: :timer.minutes(1), deadline: 10)

      assert status == GRPC.Status.deadline_exceeded()
    end
  end

  describe "receive_data/2 - deadline through GRPC.Stub" do
    test "a :deadline on a unary call reaches the server instead of raising", %{port: port} do
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter: Mint)
      on_exit(fn -> GRPC.Stub.disconnect(channel) end)

      point = %Routeguide.Point{latitude: 409_146_138, longitude: -746_188_906}

      assert {:ok, feature} =
               Routeguide.RouteGuide.Stub.get_feature(channel, point,
                 deadline: DateTime.add(DateTime.utc_now(), 30, :second)
               )

      assert feature == %Routeguide.Feature{location: point, name: "409146138,-746188906"}
    end
  end

  describe "connect/2 with retry option" do
    test "passes retry option to ConnectionProcess state", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      {:ok, connected} = Mint.connect(channel, retry: 5)
      state = :sys.get_state(connected.adapter_payload.conn_pid)

      assert state.retry == 5
      assert state.retry_attempt == 0
      assert state.scheme == :http
      assert state.host == "localhost"
      assert state.port == port
    end

    test "defaults retry to 0 when not specified", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      {:ok, connected} = Mint.connect(channel, [])
      state = :sys.get_state(connected.adapter_payload.conn_pid)

      assert state.retry == 0
    end
  end
end
