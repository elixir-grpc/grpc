defmodule GRPC.Client.Adapters.GunTest do
  use GRPC.DataCase, async: true

  alias GRPC.Client.Adapters.Gun

  describe "connect/2" do
    setup do
      server_credential = build(:credential)
      {:ok, _, port} = GRPC.Server.start(FeatureServer, 0, cred: server_credential)

      on_exit(fn ->
        :ok = GRPC.Server.stop(FeatureServer)
      end)

      %{
        port: port,
        credential:
          build(:credential,
            ssl: Keyword.take(server_credential.ssl, [:certfile, :keyfile, :versions])
          )
      }
    end

    test "connects insecurely (default options)", %{port: port, credential: credential} do
      channel = build(:channel, port: port, host: "localhost", cred: credential)

      assert {:ok, result} = Gun.connect(channel, [])

      assert %{channel | adapter_payload: %{conn_pid: result.adapter_payload.conn_pid}} == result
    end

    test "connects insecurely (custom options)", %{port: port, credential: credential} do
      channel = build(:channel, port: port, host: "localhost", cred: credential)

      # Ensure that it works
      assert {:ok, result} = Gun.connect(channel, transport_opts: [ip: :loopback])
      assert %{channel | adapter_payload: %{conn_pid: result.adapter_payload.conn_pid}} == result

      # Ensure that changing one of the options breaks things
      assert {:error, {:down, :badarg}} ==
               Gun.connect(channel, transport_opts: [ip: "256.0.0.0"])
    end

    test "connects securely (default options)", %{port: port, credential: credential} do
      channel =
        build(:channel,
          port: port,
          scheme: "https",
          host: "localhost",
          cred: credential
        )

      assert {:ok, result} = Gun.connect(channel, tls_opts: channel.cred.ssl)

      assert %{channel | adapter_payload: %{conn_pid: result.adapter_payload.conn_pid}} == result
    end

    test "connects securely (custom options)", %{port: port, credential: credential} do
      channel =
        build(:channel,
          port: port,
          scheme: "https",
          host: "localhost",
          cred: credential
        )

      # Ensure that it works
      assert {:ok, result} =
               Gun.connect(channel,
                 transport_opts: [certfile: credential.ssl[:certfile], ip: :loopback]
               )

      assert %{channel | adapter_payload: %{conn_pid: result.adapter_payload.conn_pid}} == result

      # Ensure that changing one of the options breaks things
      assert {:error, :timeout} ==
               Gun.connect(channel,
                 transport_opts: [
                   certfile: credential.ssl[:certfile] <> "invalidsuffix",
                   ip: :loopback
                 ]
               )
    end
  end
end
