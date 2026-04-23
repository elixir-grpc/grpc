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
    test "returns {:error, :no_connection} when no persistent_term entry exists", %{ref: ref} do
      channel = %Channel{ref: ref}

      assert {:error, :no_connection} = Connection.pick_channel(channel)
    end

    test "returns {:ok, channel} when a channel is stored in persistent_term", %{
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

    test "returns {:error, :no_connection} when already_started but no persistent_term entry", %{
      ref: ref,
      target: target,
      adapter: adapter
    } do
      {:ok, channel} = Connection.connect(target, adapter: adapter, name: ref)

      # Capture the real entry so we can restore its shape for disconnect.
      entry = :persistent_term.get({Connection, :lb_state, ref})
      :persistent_term.erase({Connection, :lb_state, ref})

      # Calling connect again will hit already_started → pick_channel → :no_connection
      assert {:error, :no_connection} = Connection.connect(target, adapter: adapter, name: ref)

      :persistent_term.put({Connection, :lb_state, ref}, entry)
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

    test "pick_channel returns {:error, :no_connection} after disconnect (persistent_term is erased)",
         %{ref: ref, target: target, adapter: adapter} do
      {:ok, channel} = Connection.connect(target, adapter: adapter, name: ref)

      {:ok, _} = Connection.disconnect(channel)

      assert {:error, :no_connection} = Connection.pick_channel(channel)
    end
  end

  describe "terminate/2 - persistent_term cleanup on process kill" do
    test "persistent_term is erased when process is killed without disconnect", %{
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

  defp lb_tid(ref) do
    pid = :global.whereis_name({Connection, ref})
    %{lb_state: %{tid: tid}} = :sys.get_state(pid)
    tid
  end
end
