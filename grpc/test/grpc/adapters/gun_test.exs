defmodule GRPC.Client.Adapters.GunTest do
  use GRPC.Client.DataCase, async: true

  alias GRPC.Client.Adapters.Gun

  defmodule Endpoint do
    use GRPC.Endpoint
    run(FeatureServer)
  end

  setup do
    server_credential = build(:credential)

    {:ok, _, port} =
      GRPC.Server.start_endpoint(Endpoint, 0, adapter_opts: [cred: server_credential])

    on_exit(fn ->
      :ok = GRPC.Server.stop_endpoint(Endpoint)
    end)

    %{
      port: port,
      credential: server_credential
    }
  end

  describe "connect/2" do
    test "connects insecurely (default options)", %{port: port, credential: credential} do
      channel = build(:channel, port: port, host: "localhost", cred: credential)

      assert {:ok, result} = Gun.connect(channel, [])
      assert %{conn_pid: conn_pid} = result.adapter_payload
      assert is_pid(conn_pid)
      assert %{channel | adapter_payload: %{conn_pid: conn_pid}} == result
    end

    test "connects insecurely (custom options)", %{port: port, credential: credential} do
      channel = build(:channel, port: port, host: "localhost", cred: credential)

      # Ensure that it works
      assert {:ok, result} = Gun.connect(channel, transport_opts: [ip: :loopback])
      assert %{conn_pid: conn_pid} = result.adapter_payload
      assert is_pid(conn_pid)
      assert %{channel | adapter_payload: %{conn_pid: conn_pid}} == result

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
      assert %{conn_pid: conn_pid} = result.adapter_payload
      assert is_pid(conn_pid)
      assert %{channel | adapter_payload: %{conn_pid: conn_pid}} == result
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
                 transport_opts: [
                   verify: :verify_none,
                   certfile: credential.ssl[:certfile],
                   ip: :loopback
                 ]
               )

      assert %{conn_pid: conn_pid} = result.adapter_payload
      assert is_pid(conn_pid)
      assert %{channel | adapter_payload: %{conn_pid: conn_pid}} == result

      # Ensure that changing one of the options breaks things
      assert {:error, :timeout} ==
               Gun.connect(channel,
                 await_timeout: 100,
                 transport_opts: [
                   certfile: credential.ssl[:certfile] <> "invalidsuffix",
                   verify: :verify_peer,
                   ip: :loopback
                 ]
               )
    end
  end

  describe "disconnect/1" do
    test "keeps adapter_payload as a map with conn_pid set to nil", %{
      port: port,
      credential: credential
    } do
      channel = build(:channel, port: port, host: "localhost", cred: credential)

      {:ok, connected} = Gun.connect(channel, [])
      assert %{conn_pid: conn_pid} = connected.adapter_payload
      assert is_pid(conn_pid)

      {:ok, disconnected} = Gun.disconnect(connected)

      assert %{conn_pid: nil} = disconnected.adapter_payload
    end

    test "disconnect is idempotent — calling it twice succeeds", %{
      port: port,
      credential: credential
    } do
      channel = build(:channel, port: port, host: "localhost", cred: credential)

      {:ok, connected} = Gun.connect(channel, [])
      {:ok, disconnected} = Gun.disconnect(connected)
      {:ok, disconnected_again} = Gun.disconnect(disconnected)

      assert %{conn_pid: nil} = disconnected_again.adapter_payload
    end
  end

  describe "receive_data/2" do
    test "maps connection-level gun errors to internal RPC errors" do
      {:ok, response_pid} = GRPC.Client.Adapters.Gun.StreamResponseProcess.start_link()

      reason = {:protocol_error, :"The preface was not received in a reasonable amount of time."}
      send(response_pid, {:gun_error, self(), reason})

      stream = %GRPC.Client.Stream{payload: %{response_pid: response_pid}, server_stream: false}
      internal = GRPC.Status.internal()

      assert {:error, %GRPC.RPCError{status: ^internal, message: message}} =
               Gun.receive_data(stream, timeout: 100)

      assert message =~ ":connection_error"
      assert message =~ "preface"
    end
  end
end
