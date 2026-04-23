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
    # The Connection GenServer owns the LB's ETS table (lb_mod.init runs in
    # the GenServer), so BEAM reclaims the table automatically when the
    # process exits — no explicit shutdown callback needed.
    test "disconnect/1 exits the GenServer and the ETS table is freed", %{
      ref: ref,
      target: target,
      adapter: adapter
    } do
      {:ok, channel} =
        Connection.connect(target, adapter: adapter, name: ref, lb_policy: :round_robin)

      tid = lb_tid(ref)
      assert :ets.info(tid) != :undefined

      pid = :global.whereis_name({Connection, ref})
      ref_mon = Process.monitor(pid)

      {:ok, _} = Connection.disconnect(channel)

      # disconnect replies before the GenServer terminates (via {:continue, :stop}),
      # so wait for the process to actually exit before asserting BEAM has reclaimed
      # the table.
      assert_receive {:DOWN, ^ref_mon, :process, ^pid, _reason}, 500

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

  describe "resource leaks over repeated connect/disconnect" do
    # Mirrors the regression style used in #509 for the persistent_term leak:
    # cycle connect/disconnect many times and assert zero growth in the
    # long-lived tables we own (the registry + the count of ETS tables).
    test "500 cycles leave the registry empty and no per-LB tables leak", %{
      target: target,
      adapter: adapter
    } do
      before_table_count = length(:ets.all())
      before_registry_size = :ets.info(:grpc_client_lb_registry, :size)

      for _ <- 1..500 do
        ref = make_ref()
        {:ok, channel} = Connection.connect(target, adapter: adapter, name: ref)
        {:ok, _} = Connection.disconnect(channel)
      end

      after_registry_size = :ets.info(:grpc_client_lb_registry, :size)
      after_table_count = length(:ets.all())

      assert after_registry_size == before_registry_size,
             "registry leaked: before=#{before_registry_size} after=#{after_registry_size}"

      # Allow small slop for tables the VM may have created incidentally.
      assert after_table_count - before_table_count <= 5,
             "ETS tables leaked: before=#{before_table_count} after=#{after_table_count}"
    end
  end

  defp lb_tid(ref) do
    pid = :global.whereis_name({Connection, ref})
    %{lb_state: %{tid: tid}} = :sys.get_state(pid)
    tid
  end
end
