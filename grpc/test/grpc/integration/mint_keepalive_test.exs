defmodule GRPC.Integration.MintKeepaliveTest do
  # async: false — toggles global Application env
  use ExUnit.Case, async: false

  import GRPC.Integration.TestCase

  @adapter_opts [keepalive: 20, keepalive_tolerance: 1]

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream) do
      %Helloworld.HelloReply{message: "Hello, #{req.name}"}
    end
  end

  describe "pool mode" do
    test "pings keep a pooled connection open and RPCs keep working" do
      run_server(HelloServer, fn port ->
        {:ok, channel} =
          GRPC.Stub.connect("127.0.0.1:#{port}",
            adapter: GRPC.Client.Adapters.Mint,
            adapter_opts: [keepalive: 20, keepalive_tolerance: 10]
          )

        [conn_pid] = pool_conn_pids(channel)
        Process.sleep(500)

        assert [^conn_pid] = pool_conn_pids(channel)

        assert {:ok, %Helloworld.HelloReply{message: "Hello, World"}} =
                 Helloworld.Greeter.Stub.say_hello(channel, %Helloworld.HelloRequest{
                   name: "World"
                 })
      end)
    end

    @tag :capture_log
    test "the pool replaces a connection closed by missed pings" do
      port = silent_server()

      {:ok, channel} =
        GRPC.Stub.connect("127.0.0.1:#{port}",
          adapter: GRPC.Client.Adapters.Mint,
          adapter_opts: @adapter_opts
        )

      on_exit(fn -> GRPC.Stub.disconnect(channel) end)
      assert_receive :accepted
      [conn_pid] = pool_conn_pids(channel)
      ref = Process.monitor(conn_pid)

      assert_receive {:DOWN, ^ref, :process, ^conn_pid, :normal}, 1_000
      assert_receive :accepted, 1_000

      assert [new_pid] = pool_conn_pids(channel)
      assert new_pid != conn_pid
    end

    @tag :capture_log
    test "disconnecting stops pooled connections" do
      port = silent_server()

      {:ok, channel} =
        GRPC.Stub.connect("127.0.0.1:#{port}",
          adapter: GRPC.Client.Adapters.Mint,
          adapter_opts: [keepalive: 20, keepalive_tolerance: 100]
        )

      assert_receive :accepted
      [conn_pid] = pool_conn_pids(channel)
      ref = Process.monitor(conn_pid)

      {:ok, _channel} = GRPC.Stub.disconnect(channel)

      assert_receive {:DOWN, ^ref, :process, ^conn_pid, _}, 1_000
      refute_receive :accepted, 200
    end
  end

  describe "legacy mode" do
    setup do
      previous = Application.get_env(:grpc, :pool_enabled)
      Application.put_env(:grpc, :pool_enabled, false)
      on_exit(fn -> Application.put_env(:grpc, :pool_enabled, previous) end)
    end

    @tag :capture_log
    test "connection recovery redials after missed pings close the connection" do
      port = silent_server()

      {:ok, channel} =
        GRPC.Stub.connect("127.0.0.1:#{port}",
          adapter: GRPC.Client.Adapters.Mint,
          adapter_opts: @adapter_opts
        )

      on_exit(fn -> GRPC.Stub.disconnect(channel) end)
      assert_receive :accepted
      %{adapter_payload: %{conn_pid: conn_pid}} = channel
      ref = Process.monitor(conn_pid)

      assert_receive {:DOWN, ^ref, :process, ^conn_pid, :normal}, 1_000
      assert_receive :accepted, 5_000
    end
  end

  defp pool_conn_pids(%GRPC.Channel{pool: pool_ref}) do
    [{pool_pid, _}] =
      Registry.lookup(GRPC.Client.Pool.Registry, {GRPC.Client.Pool.Server, pool_ref})

    pool_pid
    |> :sys.get_state()
    |> Map.fetch!(:channels)
    |> Enum.map(fn {_id, %{channel: %{adapter_payload: %{conn_pid: pid}}}} -> pid end)
  end

  # Completes the HTTP/2 handshake but never acknowledges PING frames. The test
  # process receives :accepted for every connection.
  defp silent_server do
    test_pid = self()
    {:ok, listen_socket} = :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true])
    {:ok, port} = :inet.port(listen_socket)

    acceptor =
      spawn_link(fn -> accept_loop(listen_socket, test_pid) end)

    on_exit(fn ->
      Process.unlink(acceptor)
      Process.exit(acceptor, :kill)
      :gen_tcp.close(listen_socket)
    end)

    port
  end

  defp accept_loop(listen_socket, test_pid) do
    case :gen_tcp.accept(listen_socket) do
      {:ok, socket} ->
        handler =
          spawn(fn ->
            receive do
              :go -> :ok
            end

            :ok = :gen_tcp.send(socket, <<0::24, 0x04, 0, 0::32>>)
            :ok = :gen_tcp.send(socket, <<0::24, 0x04, 0x01, 0::32>>)
            Process.sleep(:infinity)
          end)

        :ok = :gen_tcp.controlling_process(socket, handler)
        send(handler, :go)
        send(test_pid, :accepted)
        accept_loop(listen_socket, test_pid)

      {:error, _} ->
        :ok
    end
  end
end
