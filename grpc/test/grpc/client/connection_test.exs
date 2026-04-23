defmodule GRPC.Client.ConnectionTest do
  use GRPC.Client.DataCase, async: false

  alias GRPC.Channel
  alias GRPC.Client.Connection
  alias GRPC.Client.LoadBalancing.Registry

  setup do
    %{
      ref: make_ref(),
      ip: "127.0.0.1",
      target: "ipv4:127.0.0.1:50051",
      adapter: GRPC.Test.ClientAdapter
    }
  end

  describe "pick_channel/2" do
    test "returns {:error, :no_connection} when the ref is not registered", %{ref: ref} do
      channel = %Channel{ref: ref}

      assert {:error, :no_connection} = Connection.pick_channel(channel)
    end

    test "returns {:ok, channel} once a connection has registered its LB state", %{
      ref: ref,
      target: target,
      adapter: adapter
    } do
      {:ok, channel} = Connection.connect(target, adapter: adapter, name: ref)

      assert {:ok, ^channel} = Connection.pick_channel(%Channel{ref: ref})
    end
  end

  describe "connect/2 - already_started branch" do
    test "returns {:ok, channel} when the GenServer for the same ref is already running", %{
      ref: ref,
      ip: ip,
      target: target,
      adapter: adapter
    } do
      {:ok, first_channel} = Connection.connect(target, adapter: adapter, name: ref)

      # Connecting again with the same ref triggers the :already_started path
      {:ok, second_channel} = Connection.connect(target, adapter: adapter, name: ref)

      assert first_channel.ref == second_channel.ref
      assert second_channel.host == ip
      assert second_channel.port == 50051

      Connection.disconnect(first_channel)
    end

    test "returns {:error, :no_connection} when already_started but the registry entry is missing",
         %{ref: ref, target: target, adapter: adapter} do
      {:ok, channel} = Connection.connect(target, adapter: adapter, name: ref)

      {:ok, entry} = Registry.lookup(ref)
      Registry.delete(ref)

      # Calling connect again will hit already_started → pick_channel → :no_connection
      assert {:error, :no_connection} = Connection.connect(target, adapter: adapter, name: ref)

      Registry.put(ref, entry)
      Connection.disconnect(channel)
    end
  end

  describe "disconnect/1" do
    test "GenServer process is no longer alive after disconnect", %{
      ref: ref,
      target: target,
      adapter: adapter
    } do
      {:ok, channel} = Connection.connect(target, adapter: adapter, name: ref)

      [{pid, _}] = :global.whereis_name({Connection, ref}) |> List.wrap() |> Enum.map(&{&1, nil})

      {:ok, _} = Connection.disconnect(channel)

      ref_mon = Process.monitor(pid)
      assert_receive {:DOWN, ^ref_mon, :process, ^pid, _reason}, 500
    end

    test "pick_channel returns {:error, :no_connection} after disconnect (registry entry is deleted)",
         %{ref: ref, target: target, adapter: adapter} do
      {:ok, channel} = Connection.connect(target, adapter: adapter, name: ref)

      {:ok, _} = Connection.disconnect(channel)

      assert {:error, :no_connection} = Connection.pick_channel(channel)
    end
  end

  describe "terminate/2 - registry cleanup on process kill" do
    test "registry entry is deleted when process is killed without disconnect", %{
      ref: ref,
      target: target,
      adapter: adapter
    } do
      {:ok, channel} = Connection.connect(target, adapter: adapter, name: ref)

      pid = :global.whereis_name({Connection, ref})
      ref_mon = Process.monitor(pid)
      GenServer.stop(pid, :shutdown)
      assert_receive {:DOWN, ^ref_mon, :process, ^pid, :shutdown}, 500

      assert {:error, :no_connection} = Connection.pick_channel(channel)
    end
  end

  describe "LB ETS table lifecycle" do
    test "disconnect/1 calls lb_mod.shutdown so the ETS table is freed", %{
      ref: ref,
      target: target,
      adapter: adapter
    } do
      {:ok, channel} =
        Connection.connect(target, adapter: adapter, name: ref, lb_policy: :round_robin)

      tid = lb_tid(ref)
      assert :ets.info(tid) != :undefined

      {:ok, _} = Connection.disconnect(channel)

      assert :ets.info(tid) == :undefined
    end

    test "terminate/2 frees the ETS table when the process is killed", %{
      ref: ref,
      target: target,
      adapter: adapter
    } do
      {:ok, _channel} =
        Connection.connect(target, adapter: adapter, name: ref, lb_policy: :round_robin)

      tid = lb_tid(ref)
      pid = :global.whereis_name({Connection, ref})
      ref_mon = Process.monitor(pid)

      GenServer.stop(pid, :shutdown)
      assert_receive {:DOWN, ^ref_mon, :process, ^pid, :shutdown}, 500

      assert :ets.info(tid) == :undefined
    end
  end

  describe "pick_channel/2 races with disconnect/1" do
    # Concurrent RPCs must never crash when a disconnect is tearing down the
    # LB state. This exercises both the Registry.lookup ArgumentError rescue
    # (table gone) and the per-LB pick rescue (per-connection table gone).
    test "many concurrent picks complete without crashing while disconnect runs", %{
      ref: ref,
      target: target,
      adapter: adapter
    } do
      {:ok, channel} =
        Connection.connect(target, adapter: adapter, name: ref, lb_policy: :round_robin)

      parent = self()
      picker_count = 50
      picks_per_proc = 100

      pickers =
        for i <- 1..picker_count do
          spawn_link(fn ->
            results =
              for _ <- 1..picks_per_proc do
                Connection.pick_channel(channel)
              end

            send(parent, {:done, i, results})
          end)
        end

      # Give pickers a head start so some land before, during, and after.
      Process.sleep(2)
      {:ok, _} = Connection.disconnect(channel)

      for _ <- 1..picker_count do
        assert_receive {:done, _, results}, 2_000

        # Every result must be well-shaped — either a valid channel or the
        # documented error. Anything else means we crashed a caller.
        for r <- results do
          assert match?({:ok, %Channel{}}, r) or r == {:error, :no_connection}
        end
      end

      # And confirm the pickers actually ran to completion.
      for pid <- pickers do
        refute Process.alive?(pid)
      end
    end
  end

  defp lb_tid(ref) do
    pid = :global.whereis_name({Connection, ref})
    %{lb_state: %{tid: tid}} = :sys.get_state(pid)
    tid
  end
end
