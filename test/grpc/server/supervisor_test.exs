defmodule GRPC.Server.SupervisorTest do
  use ExUnit.Case, async: false

  alias GRPC.Server.Supervisor

  defmodule MockEndpoint do
    def __meta__(_), do: [FeatureServer]
  end

  setup_all do
    start_server = Application.get_env(:grpc, :start_server)

    on_exit(fn -> Application.put_env(:grpc, :start_server, start_server) end)

    :ok
  end

  describe "init/1" do
    test "does not start children if no opts and global env is false" do
      Application.put_env(:grpc, :start_server, false)
      assert {:ok, {%{strategy: :one_for_one}, []}} = Supervisor.init({MockEndpoint, 1234})
    end

    test "does not start children if opts sets false and global env is true" do
      Application.put_env(:grpc, :start_server, true)

      assert {:ok, {%{strategy: :one_for_one}, []}} =
               Supervisor.init({MockEndpoint, 1234, start_server: false})
    end

    test "does not start children if opts sets true and global env is false" do
      Application.put_env(:grpc, :start_server, false)

      endpoint_str = "#{Macro.to_string(MockEndpoint)}"

      assert {:ok,
              {%{strategy: :one_for_one},
               [
                 {{:ranch_listener_sup, ^endpoint_str}, _, :permanent, :infinity, :supervisor,
                  [:ranch_listener_sup]}
               ]}} = Supervisor.init({MockEndpoint, 1234, start_server: true})
    end

    test "does not start children if opts sets true and global env is true" do
      Application.put_env(:grpc, :start_server, true)

      endpoint_str = "#{Macro.to_string(MockEndpoint)}"

      assert {:ok,
              {%{strategy: :one_for_one},
               [
                 {{:ranch_listener_sup, ^endpoint_str}, _, :permanent, :infinity, :supervisor,
                  [:ranch_listener_sup]}
               ]}} = Supervisor.init({MockEndpoint, 1234, start_server: true})
    end
  end
end
