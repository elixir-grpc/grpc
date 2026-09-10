defmodule GRPC.Client.Adapters.GunTest do
  use GRPC.Client.DataCase, async: true

  alias GRPC.Client.Adapters.Gun
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

  describe "receive_data/2" do
    test "a unary call cancels and cleans up when the peer never returns headers" do
      connected = connect_quiet()
      conn_pid = connected.adapter_payload.conn_pid
      request = %Helloworld.HelloRequest{name: "timeout"}

      task =
        Task.async(fn ->
          Helloworld.Greeter.Stub.say_hello(connected, request, timeout: 500)
        end)

      {stream_ref, response_pid} = wait_for_response_process(conn_pid)
      monitor_ref = Process.monitor(response_pid)

      assert {:error, %GRPC.RPCError{status: status}} = Task.await(task, 1_000)
      assert status == GRPC.Status.deadline_exceeded()
      assert_receive {:DOWN, ^monitor_ref, :process, ^response_pid, :normal}
      refute Map.has_key?(:sys.get_state(conn_pid).response_processes, stream_ref)
    end

    test "cancels and cleans up a stream when response headers time out" do
      connected = connect_quiet()
      conn_pid = connected.adapter_payload.conn_pid

      assert {:ok, %{stream_ref: stream_ref, response_pid: response_pid}} =
               ConnectionProcess.open_stream(conn_pid, "/pending", [])

      monitor_ref = Process.monitor(response_pid)

      stream = %GRPC.Client.Stream{
        channel: connected,
        payload: %{stream_ref: stream_ref, response_pid: response_pid},
        server_stream: false
      }

      assert {:error, %GRPC.RPCError{status: status}} = Gun.receive_data(stream, timeout: 0)
      assert status == GRPC.Status.deadline_exceeded()
      assert_receive {:DOWN, ^monitor_ref, :process, ^response_pid, :normal}
      refute Map.has_key?(:sys.get_state(conn_pid).response_processes, stream_ref)
    end

    test "a finite call deadline cleans up before receive is called" do
      connected = connect_quiet()

      %{stream_ref: stream_ref, response_pid: response_pid} =
        open_pending_stream(connected, System.monotonic_time(:millisecond))

      monitor_ref = Process.monitor(response_pid)

      assert_receive {:DOWN, ^monitor_ref, :process, ^response_pid, :normal}

      refute Map.has_key?(
               :sys.get_state(connected.adapter_payload.conn_pid).response_processes,
               stream_ref
             )
    end

    test "cancels and cleans up a stream when the response stalls after headers" do
      connected = connect_quiet()

      %{stream: stream, stream_ref: stream_ref, response_pid: response_pid} =
        open_pending_stream(connected)

      monitor_ref = Process.monitor(response_pid)
      send(response_pid, {:gun_response, self(), stream_ref, :nofin, 200, []})

      assert {:error, %GRPC.RPCError{status: status}} = Gun.receive_data(stream, timeout: 0)
      assert status == GRPC.Status.deadline_exceeded()
      assert_receive {:DOWN, ^monitor_ref, :process, ^response_pid, :normal}

      refute Map.has_key?(
               :sys.get_state(stream.channel.adapter_payload.conn_pid).response_processes,
               stream_ref
             )
    end

    test "repeated timeouts leave no response processes or bookkeeping" do
      connected = connect_quiet()
      conn_pid = connected.adapter_payload.conn_pid

      for _ <- 1..3 do
        %{stream_ref: stream_ref, response_pid: response_pid} =
          open_pending_stream(connected, System.monotonic_time(:millisecond))

        monitor_ref = Process.monitor(response_pid)

        assert_receive {:DOWN, ^monitor_ref, :process, ^response_pid, :normal}
        refute Map.has_key?(:sys.get_state(conn_pid).response_processes, stream_ref)
      end

      assert :sys.get_state(conn_pid).response_processes == %{}
      assert Process.alive?(conn_pid)
    end

    test "explicit cancellation keeps its existing cleanup behavior" do
      connected = connect_quiet()

      %{stream: stream, stream_ref: stream_ref, response_pid: response_pid} =
        open_pending_stream(connected)

      monitor_ref = Process.monitor(response_pid)
      canceled = GRPC.Stub.cancel(stream)

      assert canceled.canceled
      assert :ok = ConnectionProcess.cancel(connected.adapter_payload.conn_pid, stream_ref)
      assert Process.alive?(connected.adapter_payload.conn_pid)
      assert_receive {:DOWN, ^monitor_ref, :process, ^response_pid, :normal}

      refute Map.has_key?(
               :sys.get_state(connected.adapter_payload.conn_pid).response_processes,
               stream_ref
             )

      assert {:error, %GRPC.RPCError{status: status}} = GRPC.Stub.recv(canceled)
      assert status == GRPC.Status.cancelled()
    end

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

  defp connect_quiet do
    {:ok, listen_socket} = :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true])
    {:ok, port} = :inet.port(listen_socket)
    test_pid = self()

    peer =
      spawn_link(fn ->
        {:ok, socket} = :gen_tcp.accept(listen_socket)
        :ok = :gen_tcp.send(socket, <<0::24, 0x04, 0, 0::32>>)
        :ok = :gen_tcp.send(socket, <<0::24, 0x04, 0x01, 0::32>>)
        send(test_pid, {:peer_ready, self()})

        receive do
          :close -> :gen_tcp.close(socket)
        end
      end)

    channel = build(:channel, port: port, host: "localhost")
    assert {:ok, connected} = Gun.connect(channel, [])
    assert_receive {:peer_ready, ^peer}

    on_exit(fn ->
      Gun.disconnect(connected)
      send(peer, :close)
      :gen_tcp.close(listen_socket)
    end)

    connected
  end

  defp wait_for_response_process(conn_pid, attempts \\ 100)

  defp wait_for_response_process(_conn_pid, 0), do: flunk("request was not dispatched")

  defp wait_for_response_process(conn_pid, attempts) do
    case :sys.get_state(conn_pid).response_processes do
      processes when map_size(processes) == 0 ->
        Process.sleep(1)
        wait_for_response_process(conn_pid, attempts - 1)

      processes ->
        [{stream_ref, {response_pid, _monitor_ref}}] = Map.to_list(processes)
        {stream_ref, response_pid}
    end
  end

  defp open_pending_stream(connected, deadline \\ :infinity) do
    conn_pid = connected.adapter_payload.conn_pid
    stream = build(:client_stream, channel: connected)
    headers = GRPC.Transport.HTTP2.client_headers_without_reserved(stream, timeout: :infinity)

    assert {:ok, %{stream_ref: stream_ref, response_pid: response_pid}} =
             ConnectionProcess.open_stream(conn_pid, stream.path, headers, deadline)

    %{
      stream: %GRPC.Client.Stream{
        channel: connected,
        payload: %{stream_ref: stream_ref, response_pid: response_pid, deadline: deadline},
        server_stream: false
      },
      stream_ref: stream_ref,
      response_pid: response_pid
    }
  end
end
