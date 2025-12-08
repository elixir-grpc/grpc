defmodule GRPC.Server.Adapters.ThousandIslandTest do
  use ExUnit.Case, async: true

  alias GRPC.Server.Adapters.ThousandIsland, as: Adapter

  describe "child_spec/2" do
    test "returns valid child spec" do
      spec = Adapter.child_spec(:test_endpoint, [], 50051, [])

      # The spec should be a supervisor child spec that wraps ThousandIsland + Task.Supervisor
      assert is_map(spec)
      assert spec.type == :supervisor
      assert {Supervisor, :start_link, [children, _opts]} = spec.start
      assert is_list(children)

      # Should have Task.Supervisor and ThousandIsland children
      assert length(children) == 2
      [task_sup, ti_child] = children

      # First child is Task.Supervisor
      assert {Task.Supervisor, [name: GRPC.Server.StreamTaskSupervisor]} = task_sup

      # Second child is ThousandIsland
      assert ti_child.id == :thousand_island
      assert {ThousandIsland, :start_link, [args]} = ti_child.start
      assert is_list(args)
      assert args[:port] == 50051
    end

    test "includes adapter options in child spec" do
      opts = [num_acceptors: 5, num_connections: 50]
      spec = Adapter.child_spec(:test_endpoint, [], 50051, opts)

      {Supervisor, :start_link, [children, _opts]} = spec.start
      [_task_sup, ti_child] = children
      {ThousandIsland, :start_link, [args]} = ti_child.start

      assert args[:num_acceptors] == 5
      assert args[:num_connections] == 50
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
