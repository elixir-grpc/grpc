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
      assert {:error, message} = Mint.connect(channel, transport_opts: [ip: "256.0.0.0"])

      assert message == "Error while opening connection: {:error, :badarg}"
    end

    test "accepts config_options for application specific configuration", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      assert {:ok, result} =
               Mint.connect(channel, config_options: [transport_opts: [ip: :loopback]])

      assert %{channel | adapter_payload: %{conn_pid: result.adapter_payload.conn_pid}} == result

      # Ensure that changing one of the options via config_options also breaks things
      assert {:error, message} =
               Mint.connect(channel, config_options: [transport_opts: [ip: "256.0.0.0"]])

      assert message == "Error while opening connection: {:error, :badarg}"
    end

    test "defaults client settings when none is passed", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      assert {:ok, result} = Mint.connect(channel, [])
      # wait for settings to be pushed
      Process.sleep(50)
      state = :sys.get_state(result.adapter_payload.conn_pid)

      assert %{initial_window_size: 8_000_000, max_frame_size: 8_000_000} =
               Map.get(state.conn, :client_settings)
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

      # wait for settings to be pushed
      Process.sleep(50)
      state = :sys.get_state(result.adapter_payload.conn_pid)

      assert %{initial_window_size: 50_000, max_frame_size: 50_000} =
               Map.get(state.conn, :client_settings)
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
