defmodule GRPC.Client.ConnectionSupervisedTest do
  use GRPC.Client.DataCase, async: false

  alias GRPC.Client.Connection

  defmodule ConfiguredConnection do
    use GRPC.Client.Connection, otp_app: :grpc
  end

  defmodule TrackingResolver do
    def resolve(_target) do
      {:ok, %{addresses: [%{address: "127.0.0.1", port: 50051}], service_config: nil}}
    end

    def init(_target, _opts) do
      worker =
        spawn_link(fn ->
          receive do
            :stop -> :ok
          end
        end)

      send(test_pid(), {:resolver_init, worker})
      {:ok, %{worker_pid: worker}}
    end

    def update(state, _event), do: {:ok, state}

    def shutdown(%{worker_pid: pid}) do
      send(pid, :stop)
      :ok
    end

    defp test_pid, do: Application.get_env(:grpc, :tracking_resolver_test_pid)
  end

  describe "child_spec/1 and start_link/1" do
    test "starts a named connection from an inline child spec" do
      name = unique_name("inline")

      start_supervised!(
        {Connection, name: name, target: "ipv4:127.0.0.1:50051", adapter: GRPC.Test.ClientAdapter}
      )

      assert :ok = Connection.await_ready(name, 2_000)
      assert {:ok, %GRPC.Channel{ref: ^name}} = Connection.get_channel(name)

      assert {:ok, %GRPC.Channel{host: "127.0.0.1", port: 50051}} =
               Connection.pick_channel(%GRPC.Channel{ref: name})
    end

    test "requires :target" do
      assert_raise ArgumentError, ~r/:target/, fn ->
        Connection.child_spec(name: :missing_target)
      end
    end

    test "requires :name" do
      assert_raise ArgumentError, ~r/:name/, fn ->
        Connection.child_spec(target: "ipv4:127.0.0.1:50051")
      end
    end
  end

  describe "get_channel/1 and get_channel!/1" do
    test "returns {:error, :not_started} for unknown names" do
      assert {:error, :not_started} = Connection.get_channel(:unknown_connection_name)
    end

    test "get_channel!/1 raises for unknown names" do
      assert_raise ArgumentError, ~r/no gRPC connection named/, fn ->
        Connection.get_channel!(:unknown_connection_name)
      end
    end

    test "returns the handle even while the connection is still establishing" do
      name = unique_name("connecting")
      Application.put_env(:grpc, :grpc_test_failing_hosts, ["127.0.0.1"])
      on_exit(fn -> Application.delete_env(:grpc, :grpc_test_failing_hosts) end)

      start_supervised!(
        {Connection,
         name: name, target: "ipv4:127.0.0.1:50051", adapter: GRPC.Test.FailingClientAdapter}
      )

      assert {:ok, %GRPC.Channel{ref: ^name}} = Connection.get_channel(name)
      assert {:error, :no_connection} = Connection.pick_channel(%GRPC.Channel{ref: name})
    end
  end

  describe "use GRPC.Client.Connection" do
    test "reads configuration from the application environment" do
      Application.put_env(:grpc, ConfiguredConnection,
        target: "ipv4:127.0.0.1:50052",
        adapter: GRPC.Test.ClientAdapter
      )

      on_exit(fn -> Application.delete_env(:grpc, ConfiguredConnection) end)

      start_supervised!(ConfiguredConnection)

      assert :ok = Connection.await_ready(ConfiguredConnection, 2_000)

      assert %GRPC.Channel{ref: ConfiguredConnection} =
               Connection.get_channel!(ConfiguredConnection)

      assert {:ok, %GRPC.Channel{port: 50052}} =
               Connection.pick_channel(%GRPC.Channel{ref: ConfiguredConnection})
    end

    test "start_link options override the application environment" do
      Application.put_env(:grpc, ConfiguredConnection,
        target: "ipv4:127.0.0.1:50052",
        adapter: GRPC.Test.ClientAdapter
      )

      on_exit(fn -> Application.delete_env(:grpc, ConfiguredConnection) end)

      start_supervised!({ConfiguredConnection, target: "ipv4:127.0.0.1:50053"})

      assert :ok = Connection.await_ready(ConfiguredConnection, 2_000)

      assert {:ok, %GRPC.Channel{port: 50053}} =
               Connection.pick_channel(%GRPC.Channel{ref: ConfiguredConnection})
    end
  end

  describe "resiliency" do
    test "stays alive and retries when the backend is down at boot" do
      name = unique_name("flaky")
      attach_telemetry([:grpc, :client, :connection, :connect_error])
      attach_telemetry([:grpc, :client, :connection, :connected])
      Application.put_env(:grpc, :grpc_test_failing_hosts, ["127.0.0.1"])
      on_exit(fn -> Application.delete_env(:grpc, :grpc_test_failing_hosts) end)

      start_supervised!(
        {Connection,
         name: name, target: "ipv4:127.0.0.1:50051", adapter: GRPC.Test.FailingClientAdapter}
      )

      assert_receive {:telemetry, [:grpc, :client, :connection, :connect_error],
                      %{retry_delay: _}, %{name: ^name, reason: :connection_refused}},
                     1_000

      assert {:error, :timeout} = Connection.await_ready(name, 100)

      Application.put_env(:grpc, :grpc_test_failing_hosts, [])
      send(whereis_connection(name), :retry_establish)

      assert_receive {:telemetry, [:grpc, :client, :connection, :connected],
                      %{retry_attempt: attempt}, %{name: ^name}},
                     2_000

      assert attempt >= 1
      assert :ok = Connection.await_ready(name, 2_000)

      assert {:ok, %GRPC.Channel{host: "127.0.0.1"}} =
               Connection.pick_channel(%GRPC.Channel{ref: name})
    end

    test "re-establishes after the connection process is killed" do
      name = unique_name("restart")
      attach_telemetry([:grpc, :client, :connection, :connected])

      start_supervised!(
        {Connection, name: name, target: "ipv4:127.0.0.1:50051", adapter: GRPC.Test.ClientAdapter}
      )

      assert :ok = Connection.await_ready(name, 2_000)
      assert_receive {:telemetry, [:grpc, :client, :connection, :connected], _, %{name: ^name}}

      pid = whereis_connection(name)
      Process.exit(pid, :kill)

      # A second :connected event can only come from the restarted process
      # re-establishing from scratch.
      assert_receive {:telemetry, [:grpc, :client, :connection, :connected], _, %{name: ^name}},
                     2_000

      assert whereis_connection(name) != pid
      assert :ok = Connection.await_ready(name, 2_000)
      assert {:ok, %GRPC.Channel{}} = Connection.pick_channel(%GRPC.Channel{ref: name})
    end
  end

  describe "await_ready/2 waiter lifecycle" do
    setup do
      Application.put_env(:grpc, :grpc_test_failing_hosts, ["127.0.0.1"])
      on_exit(fn -> Application.delete_env(:grpc, :grpc_test_failing_hosts) end)
      :ok
    end

    test "waiters are pruned when the caller dies" do
      name = unique_name("waiter_down")

      start_supervised!(
        {Connection,
         name: name, target: "ipv4:127.0.0.1:50051", adapter: GRPC.Test.FailingClientAdapter}
      )

      pid = whereis_connection(name)
      waiter = spawn(fn -> Connection.await_ready(name, 30_000) end)

      assert eventually(fn -> length(:sys.get_state(pid).waiters) == 1 end)

      Process.exit(waiter, :kill)

      assert eventually(fn -> :sys.get_state(pid).waiters == [] end)
    end

    test "repeated timed-out calls from the same caller do not accumulate" do
      name = unique_name("waiter_dedup")

      start_supervised!(
        {Connection,
         name: name, target: "ipv4:127.0.0.1:50051", adapter: GRPC.Test.FailingClientAdapter}
      )

      pid = whereis_connection(name)

      for _ <- 1..5 do
        assert {:error, :timeout} = Connection.await_ready(name, 50)
      end

      assert length(:sys.get_state(pid).waiters) == 1
    end

    test "pending waiters get {:error, :not_started} when the connection is disconnected" do
      name = unique_name("waiter_disconnect")

      start_supervised!(
        {Connection,
         name: name, target: "ipv4:127.0.0.1:50051", adapter: GRPC.Test.FailingClientAdapter}
      )

      pid = whereis_connection(name)
      task = Task.async(fn -> Connection.await_ready(name, 30_000) end)

      assert eventually(fn -> length(:sys.get_state(pid).waiters) == 1 end)

      assert {:ok, %GRPC.Channel{}} = Connection.disconnect(name)
      assert {:error, :not_started} = Task.await(task)
    end
  end

  describe "abnormal termination" do
    @tag capture_log: true
    test "the channel handle survives a crash so it can span the restart window" do
      name = unique_name("crash_handle")
      Process.flag(:trap_exit, true)

      {:ok, pid} =
        Connection.start_link("ipv4:127.0.0.1:50051",
          name: name,
          adapter: GRPC.Test.ClientAdapter
        )

      on_exit(fn -> :persistent_term.erase({Connection, :channel, name}) end)

      assert :ok = Connection.await_ready(name, 2_000)

      :sys.terminate(pid, :boom)
      assert_receive {:EXIT, ^pid, :boom}, 1_000

      assert {:ok, %GRPC.Channel{ref: ^name}} = Connection.get_channel(name)
      assert {:error, :no_connection} = Connection.pick_channel(%GRPC.Channel{ref: name})
    end
  end

  describe "resolver worker exits" do
    @tag capture_log: true
    test "re-init only happens for the resolver worker's own pid" do
      name = unique_name("exit_gate")
      Application.put_env(:grpc, :tracking_resolver_test_pid, self())
      on_exit(fn -> Application.delete_env(:grpc, :tracking_resolver_test_pid) end)

      start_supervised!(
        {Connection,
         name: name,
         target: "ipv4:127.0.0.1:50051",
         resolver: TrackingResolver,
         adapter: GRPC.Test.ClientAdapter}
      )

      assert :ok = Connection.await_ready(name, 2_000)
      assert_receive {:resolver_init, worker}

      conn = whereis_connection(name)

      other = spawn(fn -> :ok end)
      send(conn, {:EXIT, other, :some_crash})
      refute_receive {:resolver_init, _}, 200

      Process.exit(worker, :kill)
      assert_receive {:resolver_init, _new_worker}, 1_000
    end
  end

  describe "disconnect/1 by name" do
    test "disconnects a named connection" do
      name = unique_name("disconnect")
      attach_telemetry([:grpc, :client, :connection, :disconnected])

      start_supervised!(
        {Connection, name: name, target: "ipv4:127.0.0.1:50051", adapter: GRPC.Test.ClientAdapter}
      )

      assert :ok = Connection.await_ready(name, 2_000)
      assert {:ok, %GRPC.Channel{}} = Connection.disconnect(name)
      assert {:error, :not_started} = Connection.get_channel(name)

      assert_receive {:telemetry, [:grpc, :client, :connection, :disconnected], _,
                      %{name: ^name, reason: :normal}},
                     1_000
    end
  end

  defp unique_name(prefix), do: :"#{prefix}_#{System.unique_integer([:positive])}"

  defp whereis_connection(name) do
    case Registry.lookup(GRPC.Client.Registry, {Connection, name}) do
      [{pid, _value}] -> pid
      [] -> nil
    end
  end

  defp eventually(fun, retries \\ 50)

  defp eventually(fun, 0), do: fun.()

  defp eventually(fun, retries) do
    if fun.() do
      true
    else
      Process.sleep(50)
      eventually(fun, retries - 1)
    end
  end
end
