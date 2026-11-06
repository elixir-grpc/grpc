defmodule GRPC.Integration.DeadlineCleanupTest do
  # async: false — toggles global Application env
  use ExUnit.Case, async: false

  setup do
    previous = Application.get_env(:grpc, :pool_enabled)
    on_exit(fn -> Application.put_env(:grpc, :pool_enabled, previous) end)
  end

  # Peer that completes the HTTP/2 handshake and never answers a request.
  defp start_quiet_peer do
    {:ok, listen_socket} = :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true])
    {:ok, port} = :inet.port(listen_socket)

    peer =
      spawn_link(fn ->
        {:ok, socket} = :gen_tcp.accept(listen_socket)
        :ok = :gen_tcp.send(socket, <<0::24, 0x04, 0, 0::32>>)
        :ok = :gen_tcp.send(socket, <<0::24, 0x04, 0x01, 0::32>>)

        receive do
          :close -> :gen_tcp.close(socket)
        end
      end)

    on_exit(fn ->
      send(peer, :close)
      :gen_tcp.close(listen_socket)
    end)

    port
  end

  defp call_with_deadline(channel) do
    Helloworld.Greeter.Stub.say_hello(channel, %Helloworld.HelloRequest{name: "quiet"},
      timeout: 50
    )
  end

  # The response process replies to the caller before it notifies the connection
  # process, so the bookkeeping entry can outlive the call by a moment.
  defp assert_no_response_processes(conn_pid, attempts \\ 100)

  defp assert_no_response_processes(conn_pid, 0) do
    assert :sys.get_state(conn_pid).response_processes == %{}
  end

  defp assert_no_response_processes(conn_pid, attempts) do
    if :sys.get_state(conn_pid).response_processes == %{} do
      :ok
    else
      Process.sleep(5)
      assert_no_response_processes(conn_pid, attempts - 1)
    end
  end

  defp assert_deadline_exceeded(result) do
    assert {:error, %GRPC.RPCError{status: status}} = result
    assert status == GRPC.Status.deadline_exceeded()
  end

  test "pooled call that expires cancels its stream and releases its lease once" do
    Application.put_env(:grpc, :pool_enabled, true)
    port = start_quiet_peer()

    {:ok, channel} =
      GRPC.Stub.connect("localhost:#{port}", pool: %{size: 1, max_overflow: 0, max_streams: 1})

    [{pool_pid, _}] =
      Registry.lookup(GRPC.Client.Pool.Registry, {GRPC.Client.Pool.Server, channel.pool})

    %{channels: channels} = :sys.get_state(pool_pid)
    [%{channel: %{adapter_payload: %{conn_pid: conn_pid}}}] = Map.values(channels)

    # With max_streams: 1 a leaked lease would turn the second call into resource_exhausted.
    for _ <- 1..2 do
      assert_deadline_exceeded(call_with_deadline(channel))
    end

    state = :sys.get_state(pool_pid)
    assert Enum.all?(state.channels, fn {_, ch} -> ch.open_streams == 0 end)
    assert map_size(state.leases_by_monitor_ref) == 0

    assert_no_response_processes(conn_pid)
    assert Process.alive?(conn_pid)
  end

  test "legacy call that expires cancels its stream and keeps the connection usable" do
    Application.put_env(:grpc, :pool_enabled, false)
    port = start_quiet_peer()

    {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
    %{adapter_payload: %{conn_pid: conn_pid}} = channel

    for _ <- 1..2 do
      assert_deadline_exceeded(call_with_deadline(channel))
    end

    assert_no_response_processes(conn_pid)
    assert Process.alive?(conn_pid)
  end
end
