defmodule GRPC.Client.ConnectionSupervisedTest do
  use GRPC.Client.DataCase, async: false

  alias GRPC.Client.Connection

  defmodule ConfiguredConnection do
    use GRPC.Client.Connection, otp_app: :grpc
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

      assert :ok = ConfiguredConnection.await_ready(2_000)
      assert %GRPC.Channel{ref: ConfiguredConnection} = ConfiguredConnection.get_channel!()

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

      assert :ok = ConfiguredConnection.await_ready(2_000)

      assert {:ok, %GRPC.Channel{port: 50053}} =
               Connection.pick_channel(%GRPC.Channel{ref: ConfiguredConnection})
    end
  end

  describe "resiliency" do
    test "stays alive and retries when the backend is down at boot" do
      name = unique_name("flaky")
      Application.put_env(:grpc, :grpc_test_failing_hosts, ["127.0.0.1"])
      on_exit(fn -> Application.delete_env(:grpc, :grpc_test_failing_hosts) end)

      start_supervised!(
        {Connection,
         name: name, target: "ipv4:127.0.0.1:50051", adapter: GRPC.Test.FailingClientAdapter}
      )

      assert {:error, :timeout} = Connection.await_ready(name, 100)

      Application.put_env(:grpc, :grpc_test_failing_hosts, [])
      send(whereis_connection(name), :retry_establish)

      assert :ok = Connection.await_ready(name, 2_000)

      assert {:ok, %GRPC.Channel{host: "127.0.0.1"}} =
               Connection.pick_channel(%GRPC.Channel{ref: name})
    end

    test "re-establishes after the connection process is killed" do
      name = unique_name("restart")

      start_supervised!(
        {Connection, name: name, target: "ipv4:127.0.0.1:50051", adapter: GRPC.Test.ClientAdapter}
      )

      assert :ok = Connection.await_ready(name, 2_000)

      pid = whereis_connection(name)
      Process.exit(pid, :kill)

      assert eventually(fn ->
               case whereis_connection(name) do
                 nil -> false
                 new_pid -> new_pid != pid and Connection.await_ready(name, 500) == :ok
               end
             end)

      assert {:ok, %GRPC.Channel{}} = Connection.pick_channel(%GRPC.Channel{ref: name})
    end
  end

  describe "disconnect/1 by name" do
    test "disconnects a named connection" do
      name = unique_name("disconnect")

      start_supervised!(
        {Connection, name: name, target: "ipv4:127.0.0.1:50051", adapter: GRPC.Test.ClientAdapter}
      )

      assert :ok = Connection.await_ready(name, 2_000)
      assert {:ok, %GRPC.Channel{}} = Connection.disconnect(name)
      assert eventually(fn -> Connection.get_channel(name) == {:error, :not_started} end)
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
