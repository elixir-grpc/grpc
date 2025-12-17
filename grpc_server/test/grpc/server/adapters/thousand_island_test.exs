defmodule GRPC.Server.Adapters.ThousandIslandTest do
  use ExUnit.Case, async: true

  alias GRPC.Server.Adapters.ThousandIsland, as: Adapter

  describe "child_spec/2" do
    test "returns valid child spec" do
      spec = Adapter.child_spec(:test_endpoint, [], 50051, [])

      assert is_map(spec)
      assert spec.id == Adapter.Supervisor
      assert spec.type == :supervisor
      assert {Adapter.Supervisor, :start_link, [opts_list]} = spec.start
      assert is_list(opts_list)
      assert Keyword.get(opts_list, :endpoint) == :test_endpoint
      assert Keyword.get(opts_list, :servers) == []
      assert Keyword.get(opts_list, :port) == 50051
      assert Keyword.get(opts_list, :adapter_opts) == []
      assert Keyword.get(opts_list, :cred) == nil
    end

    test "includes adapter options in child spec" do
      adapter_opts = [num_acceptors: 5, num_connections: 50]
      spec = Adapter.child_spec(:test_endpoint, [], 50051, adapter_opts: adapter_opts)

      {Adapter.Supervisor, :start_link, [opts_list]} = spec.start
      adapter_opts_kw = Keyword.get(opts_list, :adapter_opts)
      assert Keyword.get(adapter_opts_kw, :num_acceptors) == 5
      assert Keyword.get(adapter_opts_kw, :num_connections) == 50
    end
  end

  describe "start/4" do
    test "can start and stop server" do
      {:ok, pid, port} = Adapter.start(:test_server_unique_a, [], 0, [])

      assert is_pid(pid)
      assert is_integer(port)
      # Note: Port 0 means "choose any available port"
      # ThousandIsland returns the actual assigned port  
      assert Process.alive?(pid)

      # Stop server
      Supervisor.stop(pid)
      refute Process.alive?(pid)
    end

    test "accepts custom options" do
      opts = [
        num_acceptors: 2,
        num_connections: 10
      ]

      {:ok, pid, _port} = Adapter.start(:test_server_unique_b, [], 0, opts)
      assert Process.alive?(pid)

      Supervisor.stop(pid)
    end
  end
end
