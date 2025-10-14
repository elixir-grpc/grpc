defmodule GRPC.Client.Adapters.MintTest do
  use GRPC.DataCase

  alias GRPC.Client.Adapters.Mint

  describe "connect/2" do
    setup do
      {:ok, _, port} = GRPC.Server.start(FeatureServer, 0)

      on_exit(fn ->
        :ok = GRPC.Server.stop(FeatureServer)
      end)

      %{port: port}
    end

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
end
