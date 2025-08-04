defmodule GRPC.Client.Adapters.FinchTest do
  use GRPC.DataCase

  alias GRPC.Client.Adapters.Finch

  describe "connect/2" do
    setup do
      {:ok, _, port} = GRPC.Server.start(FeatureServer, 0)

      on_exit(fn ->
        :ok = GRPC.Server.stop(FeatureServer)
      end)

      %{port: port}
    end

    test "connects insecurely (default options)", %{port: port} do
      channel = build(:channel, adapter: Finch, port: port, host: "localhost")

      assert {:ok, result} = Finch.connect(channel, [])
      assert %{channel | adapter_payload: %{conn_pid: result.adapter_payload.conn_pid}} == result
    end

    test "connects insecurely (custom options)", %{port: port} do
      channel = build(:channel, adapter: Finch, port: port, host: "localhost")

      assert {:ok, result} = Finch.connect(channel, conn_opts: [transport_opts: [ip: :loopback]])
      assert %{channel | adapter_payload: %{conn_pid: result.adapter_payload.conn_pid}} == result
    end

    test "accepts config_options for application specific configuration", %{port: port} do
      channel = build(:channel, adapter: Finch, port: port, host: "localhost")

      assert {:ok, result} =
               Finch.connect(channel,
                 config_options: [conn_opts: [transport_opts: [ip: :loopback]]]
               )

      assert %{channel | adapter_payload: %{conn_pid: result.adapter_payload.conn_pid}} == result
    end

    test "defaults client settings when none is passed", %{port: port} do
      channel = build(:channel, adapter: Finch, port: port, host: "localhost")

      assert {:ok, result} = Finch.connect(channel, [])
      # wait for settings to be pushed
      Process.sleep(50)

      state =
        get_sys_state(result.adapter_payload.conn_pid)

      assert [initial_window_size: 8_000_000, max_frame_size: 8_000_000] =
               get_in(state, [:pools, {:http, "localhost", port}, :conn_opts, :client_settings])
    end

    test "allow client settings to be passed", %{port: port} do
      channel = build(:channel, adapter: Finch, port: port, host: "localhost")

      assert {:ok, result} =
               Finch.connect(channel,
                 conn_opts: [
                   client_settings: [
                     initial_window_size: 50_000,
                     max_frame_size: 50_000
                   ]
                 ]
               )

      # wait for settings to be pushed
      Process.sleep(50)

      state =
        get_sys_state(result.adapter_payload.conn_pid)

      assert [initial_window_size: 50_000, max_frame_size: 50_000] =
               get_in(state, [:pools, {:http, "localhost", port}, :conn_opts, :client_settings])
    end
  end

  defp get_sys_state(conn_pid) do
    :sys.get_state(conn_pid)
    |> Tuple.to_list()
    |> List.last()
  end
end
