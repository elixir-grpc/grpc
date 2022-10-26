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
      channel = build(:channel, port: port, host: "localhost")

      assert {:ok, result} = Mint.connect(channel, [])
      assert %{channel | adapter_payload: %{conn_pid: result.adapter_payload.conn_pid}} == result
    end

    test "connects insecurely (custom options)", %{port: port} do
      channel = build(:channel, port: port, host: "localhost")

      assert {:ok, result} = Mint.connect(channel, transport_opts: [ip: :loopback])
      assert %{channel | adapter_payload: %{conn_pid: result.adapter_payload.conn_pid}} == result

      # Ensure that changing one of the options breaks things
      assert {:error, message} = Mint.connect(channel, transport_opts: [ip: "256.0.0.0"])

      assert message ==
               "An error happened while trying to opening the connection: {:error, :badarg}"
    end
  end
end
