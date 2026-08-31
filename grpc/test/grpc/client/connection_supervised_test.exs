defmodule GRPC.Client.ConnectionSupervisedTest do
  use GRPC.Client.DataCase, async: true

  alias GRPC.Client.Connection

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

  defmodule TransportProcessAdapter do
    @moduledoc false
    # Mirrors the Gun adapter's process shape: the "transport" process is NOT
    # linked to the connection process (Gun's ConnectionProcess lives under
    # the adapter's DynamicSupervisor), so its death is only observable via
    # the monitor the connection sets in connect_real_channel/5. Its pid is
    # exposed as adapter_payload.conn_pid; killing it simulates the transport
    # dying mid-flight (e.g. gun giving up after internal retries). Accepts
    # FailingClientAdapter-style :failing_hosts adapter options so tests can
    # flip reachability while a connection is down.
    @behaviour GRPC.Client.Adapter

    def connect(%{host: host} = channel, opts) do
      if host in failing_hosts(opts) do
        {:error, :connection_refused}
      else
        pid =
          spawn(fn ->
            receive do
              :stop -> :ok
            end
          end)

        {:ok, %{channel | adapter_payload: %{conn_pid: pid}}}
      end
    end

    def disconnect(%{adapter_payload: %{conn_pid: pid}} = channel) when is_pid(pid) do
      if Process.alive?(pid), do: send(pid, :stop)

      {:ok, %{channel | adapter_payload: %{conn_pid: nil}}}
    end

    def disconnect(channel), do: {:ok, channel}

    def send_request(stream, _message, _opts), do: stream
    def receive_data(_stream, _opts), do: {:ok, nil}
    def send_data(stream, _message, _opts), do: stream
    def send_headers(stream, _opts), do: stream
    def end_stream(stream), do: stream
    def cancel(stream), do: stream

    defp failing_hosts(opts) do
      case Keyword.get(opts || [], :failing_hosts, []) do
        fun when is_function(fun, 0) -> fun.()
        hosts when is_list(hosts) -> hosts
      end
    end
  end

  defmodule TwoAddressResolver do
    @moduledoc false
    def resolve(_target) do
      {:ok,
       %{
         addresses: [
           %{address: "127.0.0.1", port: 50051},
           %{address: "127.0.0.2", port: 50052}
         ],
         service_config: nil
       }}
    end
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

      start_supervised!(
        {Connection,
         name: name,
         target: "ipv4:127.0.0.1:50051",
         adapter: GRPC.Test.FailingClientAdapter,
         adapter_opts: [failing_hosts: ["127.0.0.1"]]}
      )

      assert {:ok, %GRPC.Channel{ref: ^name}} = Connection.get_channel(name)
      assert {:error, :no_connection} = Connection.pick_channel(%GRPC.Channel{ref: name})
    end
  end

  describe "resiliency" do
    test "stays alive and retries when the backend is down at boot" do
      name = unique_name("flaky")
      attach_telemetry([:grpc, :client, :connection, :connect_error])
      attach_telemetry([:grpc, :client, :connection, :connected])
      hosts = start_supervised!({Agent, fn -> ["127.0.0.1"] end})

      start_supervised!(
        {Connection,
         name: name,
         target: "ipv4:127.0.0.1:50051",
         adapter: GRPC.Test.FailingClientAdapter,
         adapter_opts: [failing_hosts: fn -> Agent.get(hosts, & &1) end]}
      )

      assert_receive {:telemetry, [:grpc, :client, :connection, :connect_error],
                      %{retry_delay: _}, %{name: ^name, reason: :connection_refused}},
                     1_000

      assert {:error, :timeout} = Connection.await_ready(name, 100)

      Agent.update(hosts, fn _ -> [] end)
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

  describe "underlying connection process death" do
    @tag capture_log: true
    test "re-establishes in place when the last connection process dies" do
      name = unique_name("conn_death")
      attach_telemetry([:grpc, :client, :connection, :connected])

      start_supervised!(
        {Connection, name: name, target: "ipv4:127.0.0.1:50051", adapter: TransportProcessAdapter}
      )

      assert :ok = Connection.await_ready(name, 2_000)
      assert_receive {:telemetry, [:grpc, :client, :connection, :connected], _, %{name: ^name}}

      conn = whereis_connection(name)

      assert {:ok, %GRPC.Channel{adapter_payload: %{conn_pid: pid1}}} =
               Connection.pick_channel(%GRPC.Channel{ref: name})

      Process.exit(pid1, :kill)

      # Recovery must come from the same orchestrator process reconnecting,
      # not from a supervisor restart.
      assert_receive {:telemetry, [:grpc, :client, :connection, :connected], _, %{name: ^name}},
                     2_000

      assert whereis_connection(name) == conn
      assert :ok = Connection.await_ready(name, 2_000)

      assert {:ok, %GRPC.Channel{adapter_payload: %{conn_pid: pid2}}} =
               Connection.pick_channel(%GRPC.Channel{ref: name})

      assert pid2 != pid1
      assert Process.alive?(pid2)
    end

    @tag capture_log: true
    test "RPCs fail with UNAVAILABLE while down and succeed after recovery" do
      name = unique_name("conn_death_rpc")
      hosts = start_supervised!({Agent, fn -> [] end})

      start_supervised!(
        {Connection,
         name: name,
         target: "ipv4:127.0.0.1:50051",
         adapter: TransportProcessAdapter,
         adapter_opts: [failing_hosts: fn -> Agent.get(hosts, & &1) end]}
      )

      assert :ok = Connection.await_ready(name, 2_000)
      conn = whereis_connection(name)
      {:ok, handle} = Connection.get_channel(name)
      request = %Helloworld.HelloRequest{name: "ping"}

      assert {:ok, _} = Helloworld.Greeter.Stub.say_hello(handle, request)

      assert {:ok, %GRPC.Channel{adapter_payload: %{conn_pid: pid1}}} =
               Connection.pick_channel(handle)

      # Make redials fail, then kill the transport: the connection enters the
      # retry loop and RPCs must fail clean instead of crashing in the adapter
      # (previously a FunctionClauseError on the payload-less virtual handle).
      Agent.update(hosts, fn _ -> ["127.0.0.1"] end)
      kill_and_await(pid1)
      wait_until(fn -> not :sys.get_state(conn).established? end)

      unavailable = GRPC.Status.unavailable()

      assert {:error, %GRPC.RPCError{status: ^unavailable}} =
               Helloworld.Greeter.Stub.say_hello(handle, request)

      Agent.update(hosts, fn _ -> [] end)
      send(conn, :retry_establish)

      assert :ok = Connection.await_ready(name, 2_000)
      assert {:ok, _} = Helloworld.Greeter.Stub.say_hello(handle, request)
    end

    @tag capture_log: true
    test "request-streaming calls raise UNAVAILABLE instead of returning an error tuple" do
      name = unique_name("conn_death_stream")
      hosts = start_supervised!({Agent, fn -> ["127.0.0.1"] end})

      start_supervised!(
        {Connection,
         name: name,
         target: "ipv4:127.0.0.1:50051",
         adapter: TransportProcessAdapter,
         adapter_opts: [failing_hosts: fn -> Agent.get(hosts, & &1) end]}
      )

      {:ok, handle} = Connection.get_channel(name)

      # A stream return value cannot express failure, so the stub must raise
      # rather than hand back an error tuple that send_request/3 would crash on.
      assert_raise GRPC.RPCError, ~r/no healthy connection/, fn ->
        Routeguide.RouteGuide.Stub.record_route(handle)
      end
    end

    @tag capture_log: true
    test "keeps serving from the remaining channels and redials the dead one" do
      name = unique_name("partial_death")

      start_supervised!(
        {Connection,
         name: name,
         target: "dns://multi.test:50051",
         resolver: TwoAddressResolver,
         lb_policy: :round_robin,
         adapter: TransportProcessAdapter}
      )

      assert :ok = Connection.await_ready(name, 2_000)
      conn = whereis_connection(name)
      handle = %GRPC.Channel{ref: name}

      pids =
        for _ <- 1..4, uniq: true do
          {:ok, %GRPC.Channel{adapter_payload: %{conn_pid: pid}}} =
            Connection.pick_channel(handle)

          pid
        end

      assert length(pids) == 2
      [dead, _survivor] = pids

      kill_and_await(dead)
      wait_until(fn -> is_nil(connected_key_for(conn, dead)) end)

      # Still established and no full re-establish: picks never see the dead
      # channel again.
      assert :ok = Connection.await_ready(name, 100)
      assert whereis_connection(name) == conn

      for _ <- 1..4 do
        assert {:ok, %GRPC.Channel{adapter_payload: %{conn_pid: pid}}} =
                 Connection.pick_channel(handle)

        assert pid != dead
        assert Process.alive?(pid)
      end

      # The repair loop redials the dead endpoint even though this resolver
      # has no background worker to trigger a re-resolution.
      wait_until(fn ->
        pids =
          for _ <- 1..4, uniq: true do
            {:ok, %GRPC.Channel{adapter_payload: %{conn_pid: pid}}} =
              Connection.pick_channel(handle)

            pid
          end

        length(pids) == 2 and Enum.all?(pids, &Process.alive?/1)
      end)
    end

    test "stub calls through an unresolvable virtual handle return UNAVAILABLE" do
      handle = %GRPC.Channel{ref: :no_such_connection_name}
      unavailable = GRPC.Status.unavailable()

      assert {:error, %GRPC.RPCError{status: ^unavailable}} =
               Helloworld.Greeter.Stub.say_hello(handle, %Helloworld.HelloRequest{name: "x"})
    end
  end

  describe "await_ready/2 waiter lifecycle" do
    setup do
      %{
        connection_opts: [
          target: "ipv4:127.0.0.1:50051",
          adapter: GRPC.Test.FailingClientAdapter,
          adapter_opts: [failing_hosts: ["127.0.0.1"]]
        ]
      }
    end

    test "waiters are pruned when the caller dies", %{connection_opts: connection_opts} do
      name = unique_name("waiter_down")
      attach_telemetry([:grpc, :client, :connection, :await_ready, :start])
      attach_telemetry([:grpc, :client, :connection, :await_ready, :stop])

      start_supervised!({Connection, [name: name] ++ connection_opts})

      pid = whereis_connection(name)
      waiter = spawn(fn -> Connection.await_ready(name, 30_000) end)

      assert_receive {:telemetry, [:grpc, :client, :connection, :await_ready, :start], _,
                      %{name: ^name, caller: ^waiter}},
                     1_000

      assert length(:sys.get_state(pid).waiters) == 1

      Process.exit(waiter, :kill)

      assert_receive {:telemetry, [:grpc, :client, :connection, :await_ready, :stop],
                      %{duration: _}, %{name: ^name, caller: ^waiter, result: :abandoned}},
                     1_000

      assert :sys.get_state(pid).waiters == []
    end

    test "repeated timed-out calls from the same caller do not accumulate", %{
      connection_opts: connection_opts
    } do
      name = unique_name("waiter_dedup")
      attach_telemetry([:grpc, :client, :connection, :await_ready, :start])
      attach_telemetry([:grpc, :client, :connection, :await_ready, :stop])

      start_supervised!({Connection, [name: name] ++ connection_opts})

      pid = whereis_connection(name)
      caller = self()

      for _ <- 1..5 do
        assert {:error, :timeout} = Connection.await_ready(name, 10)
      end

      # Five starts prove the connection registered every call; each re-entry
      # replaces the caller's stale entry, closing its span as :abandoned.
      for _ <- 1..5 do
        assert_receive {:telemetry, [:grpc, :client, :connection, :await_ready, :start], _,
                        %{name: ^name, caller: ^caller}},
                       1_000
      end

      for _ <- 1..4 do
        assert_receive {:telemetry, [:grpc, :client, :connection, :await_ready, :stop], _,
                        %{name: ^name, caller: ^caller, result: :abandoned}},
                       1_000
      end

      assert length(:sys.get_state(pid).waiters) == 1
    end

    test "pending waiters get {:error, :not_started} when the connection is disconnected", %{
      connection_opts: connection_opts
    } do
      name = unique_name("waiter_disconnect")
      attach_telemetry([:grpc, :client, :connection, :await_ready, :start])
      attach_telemetry([:grpc, :client, :connection, :await_ready, :stop])

      start_supervised!({Connection, [name: name] ++ connection_opts})

      task = Task.async(fn -> Connection.await_ready(name, 30_000) end)
      task_pid = task.pid

      assert_receive {:telemetry, [:grpc, :client, :connection, :await_ready, :start], _,
                      %{name: ^name, caller: ^task_pid}},
                     1_000

      assert {:ok, %GRPC.Channel{}} = Connection.disconnect(name)

      assert_receive {:telemetry, [:grpc, :client, :connection, :await_ready, :stop], _,
                      %{name: ^name, caller: ^task_pid, result: :disconnected}},
                     1_000

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
      :sys.get_state(conn)
      refute_received {:resolver_init, _}

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

  # Kills a transport and blocks until it is actually gone, so its monitor
  # signal to the connection process has been dispatched.
  defp kill_and_await(pid) do
    ref = Process.monitor(pid)
    Process.exit(pid, :kill)
    assert_receive {:DOWN, ^ref, :process, ^pid, _}, 1_000
  end

  defp wait_until(fun, tries \\ 200) do
    cond do
      fun.() ->
        :ok

      tries == 0 ->
        flunk("condition not met within the wait budget")

      true ->
        Process.sleep(10)
        wait_until(fun, tries - 1)
    end
  end

  defp connected_key_for(conn, pid) do
    Enum.find_value(:sys.get_state(conn).real_channels, fn
      {key, {:connected, %{adapter_payload: %{conn_pid: ^pid}}}} -> key
      _ -> nil
    end)
  end

  defp whereis_connection(name) do
    case Registry.lookup(GRPC.Client.Registry, {Connection, name}) do
      [{pid, _value}] -> pid
      [] -> nil
    end
  end
end
