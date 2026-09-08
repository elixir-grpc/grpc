defmodule GRPC.Client.Adapters.GunTest do
  use GRPC.Client.DataCase, async: true

  alias GRPC.Client.Adapters.Gun
  alias GRPC.Client.Adapters.Gun.ConnectionProcess
  alias GRPC.Client.Adapters.Gun.ConnectionProcess

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

    test "reuses the connection process for a named channel", %{
      port: port,
      credential: credential
    } do
      channel =
        build(:channel, port: port, host: "localhost", cred: credential, ref: make_ref())

      assert {:ok, %{adapter_payload: %{conn_pid: conn_pid}} = connected} =
               Gun.connect(channel, [])

      on_exit(fn -> Gun.disconnect(connected) end)

      assert {:ok, %{adapter_payload: %{conn_pid: ^conn_pid}}} = Gun.connect(channel, [])

      assert [{^conn_pid, _}] =
               Registry.lookup(
                 GRPC.Client.Registry,
                 {ConnectionProcess, {channel.ref, channel.host, channel.port}}
               )
    end

    test "does not reuse the connection process for an unnamed channel", %{
      port: port,
      credential: credential
    } do
      channel = build(:channel, port: port, host: "localhost", cred: credential)

      assert {:ok, %{adapter_payload: %{conn_pid: first_pid}} = first} = Gun.connect(channel, [])

      assert {:ok, %{adapter_payload: %{conn_pid: second_pid}} = second} =
               Gun.connect(channel, [])

      on_exit(fn ->
        Gun.disconnect(first)
        Gun.disconnect(second)
      end)

      refute first_pid == second_pid
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

  describe "transport telemetry" do
    test "reports lifecycle, settings, occupancy, loss, recovery, and stop", %{
      port: port,
      credential: credential
    } do
      for event <- [
            :started,
            :connected,
            :settings,
            :streams,
            :stream_rejected,
            :reset,
            :down,
            :stopped
          ] do
        attach_telemetry([:grpc, :client, :transport, event])
      end

      logical_ref = make_ref()
      channel = build(:channel, ref: logical_ref, port: port, host: "localhost", cred: credential)
      assert {:ok, connected} = Gun.connect(channel, [])
      conn_pid = connected.adapter_payload.conn_pid

      assert_receive {:telemetry, [:grpc, :client, :transport, :started],
                      %{generation: 0, active_streams: 0},
                      %{logical_connection_ref: ^logical_ref, transport_ref: ^conn_pid}}

      assert_receive {:telemetry, [:grpc, :client, :transport, :connected], %{generation: 1},
                      %{reconnect: false}}

      gun_pid = :sys.get_state(conn_pid).gun_pid
      send(conn_pid, {:gun_notify, gun_pid, :settings_changed, %{max_concurrent_streams: 1}})

      assert_receive {:telemetry, [:grpc, :client, :transport, :settings], %{},
                      %{max_concurrent_streams: 1}}

      assert {:ok, %{stream_ref: stream_ref}} =
               ConnectionProcess.open_stream(conn_pid, "/pending", [])

      assert_receive {:telemetry, [:grpc, :client, :transport, :streams], %{active: 1}, _}

      assert {:error, %GRPC.RPCError{status: status}} =
               ConnectionProcess.open_stream(conn_pid, "/rejected", [])

      assert status == GRPC.Status.resource_exhausted()

      assert_receive {:telemetry, [:grpc, :client, :transport, :stream_rejected], %{count: 1},
                      %{reason: :max_concurrent_streams}}

      send(conn_pid, {:gun_down, gun_pid, :http2, :econnreset, [stream_ref]})

      assert_receive {:telemetry, [:grpc, :client, :transport, :streams], %{active: 0}, _}

      assert_receive {:telemetry, [:grpc, :client, :transport, :reset], %{streams_terminated: 0},
                      _}

      assert_receive {:telemetry, [:grpc, :client, :transport, :down], %{streams_terminated: 1},
                      _}

      send(conn_pid, {:gun_up, gun_pid, :http2})

      assert_receive {:telemetry, [:grpc, :client, :transport, :connected], %{generation: 2},
                      %{reconnect: true}}

      assert {:ok, %{stream_ref: recovered_stream_ref}} =
               ConnectionProcess.open_stream(conn_pid, "/pending", [])

      assert_receive {:telemetry, [:grpc, :client, :transport, :streams], %{active: 1}, _}
      assert :ok = ConnectionProcess.cancel(conn_pid, recovered_stream_ref)
      assert_receive {:telemetry, [:grpc, :client, :transport, :streams], %{active: 0}, _}

      assert {:ok, _} = Gun.disconnect(connected)

      assert_receive {:telemetry, [:grpc, :client, :transport, :stopped],
                      %{streams_terminated: 0}, _}
    end

    test "does not infer GOAWAY from a generic connection-down signal", %{
      port: port,
      credential: credential
    } do
      attach_telemetry([:grpc, :client, :transport, :goaway])
      channel = build(:channel, ref: make_ref(), port: port, host: "localhost", cred: credential)
      assert {:ok, connected} = Gun.connect(channel, [])
      conn_pid = connected.adapter_payload.conn_pid
      gun_pid = :sys.get_state(conn_pid).gun_pid

      send(conn_pid, {:gun_down, gun_pid, :http2, {:goaway, :no_error}, []})
      refute_receive {:telemetry, [:grpc, :client, :transport, :goaway], _, _}, 100
      assert {:ok, _} = Gun.disconnect(connected)
    end
  end

  describe "receive_data/2" do
    test "maps connection-level gun errors to unavailable RPC errors" do
      {:ok, response_pid} = GRPC.Client.Adapters.Gun.StreamResponseProcess.start_link()

      reason = {:protocol_error, :"The preface was not received in a reasonable amount of time."}
      send(response_pid, {:gun_error, self(), reason})

      stream = %GRPC.Client.Stream{payload: %{response_pid: response_pid}, server_stream: false}
      unavailable = GRPC.Status.unavailable()

      assert {:error, %GRPC.RPCError{status: ^unavailable, message: message}} =
               Gun.receive_data(stream, timeout: 100)

      assert message =~ "connection_error"
      assert message =~ "preface"
    end
  end
end
