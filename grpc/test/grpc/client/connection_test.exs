defmodule GRPC.Client.ConnectionTest do
  use GRPC.Client.DataCase, async: false

  alias GRPC.Channel
  alias GRPC.Client.Connection

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

    test "returns {:ok, channel} once a connection has published its LB state", %{
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
      {:ok, second_channel} = Connection.connect(target, adapter: adapter, name: ref)

      assert first_channel.ref == second_channel.ref
      assert second_channel.host == ip
      assert second_channel.port == 50051

      Connection.disconnect(first_channel)
    end

    test "returns {:error, :no_connection} when already_started but the persistent_term entry is missing",
         %{ref: ref, target: target, adapter: adapter} do
      {:ok, channel} = Connection.connect(target, adapter: adapter, name: ref)

      key = {Connection, ref}
      entry = :persistent_term.get(key)
      :persistent_term.erase(key)

      assert {:error, :no_connection} = Connection.connect(target, adapter: adapter, name: ref)

      :persistent_term.put(key, entry)
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

    test "pick_channel returns {:error, :no_connection} after disconnect (persistent_term entry is erased)",
         %{ref: ref, target: target, adapter: adapter} do
      {:ok, channel} = Connection.connect(target, adapter: adapter, name: ref)

      {:ok, _} = Connection.disconnect(channel)

      assert {:error, :no_connection} = Connection.pick_channel(channel)
    end
  end

  describe "terminate/2 - persistent_term cleanup on process kill" do
    test "persistent_term entry is erased when process is killed without disconnect", %{
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

      Process.sleep(2)
      {:ok, _} = Connection.disconnect(channel)

      for _ <- 1..picker_count do
        assert_receive {:done, _, results}, 2_000

        for r <- results do
          assert match?({:ok, %Channel{}}, r) or r == {:error, :no_connection}
        end
      end

      for pid <- pickers do
        refute Process.alive?(pid)
      end
    end
  end

  describe "resource leaks over repeated connect/disconnect" do
    test "500 cycles leave persistent_term clean and no per-LB tables leak", %{
      target: target,
      adapter: adapter
    } do
      before_table_count = length(:ets.all())
      before_pt_count = connection_pt_count()

      for _ <- 1..500 do
        ref = make_ref()
        {:ok, channel} = Connection.connect(target, adapter: adapter, name: ref)
        {:ok, _} = Connection.disconnect(channel)
      end

      after_pt_count = connection_pt_count()
      after_table_count = length(:ets.all())

      assert after_pt_count == before_pt_count,
             "persistent_term leaked: before=#{before_pt_count} after=#{after_pt_count}"

      assert after_table_count - before_table_count <= 5,
             "ETS tables leaked: before=#{before_table_count} after=#{after_table_count}"
    end
  end

  defp connection_pt_count do
    Enum.count(:persistent_term.get(), &match?({{Connection, _}, _}, &1))
  end

  defp lb_tid(ref) do
    pid = :global.whereis_name({Connection, ref})
    %{lb_state: %{tid: tid}} = :sys.get_state(pid)
    tid
  end
end
