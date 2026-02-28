defmodule GRPC.Client.ConnectionTest do
  use GRPC.Client.DataCase, async: false

  alias GRPC.Channel
  alias GRPC.Client.Connection

  setup do
    %{
      ref: make_ref(),
      ip: "127.0.0.1",
      adapter: GRPC.Test.ClientAdapter
    }
  end

  describe "pick_channel/2" do
    test "returns {:error, :no_connection} when no persistent_term entry exists", %{ref: ref} do
      channel = %Channel{ref: ref}

      assert {:error, :no_connection} = Connection.pick_channel(channel)
    end

    test "returns {:ok, channel} when a channel is stored in persistent_term", %{ref: ref} do
      stored_channel = %Channel{ref: ref, host: "127.0.0.1", port: 50051}

      :persistent_term.put({Connection, :lb_state, ref}, stored_channel)

      assert {:ok, ^stored_channel} = Connection.pick_channel(%Channel{ref: ref})

      :persistent_term.erase({Connection, :lb_state, ref})
    end

    test "ignores the opts argument and still resolves by channel ref", %{ref: ref} do
      stored_channel = %Channel{ref: ref, host: "10.0.0.1", port: 9090}

      :persistent_term.put({Connection, :lb_state, ref}, stored_channel)

      assert {:ok, ^stored_channel} =
               Connection.pick_channel(%Channel{ref: ref}, timeout: 5_000)

      :persistent_term.erase({Connection, :lb_state, ref})
    end

    test "returns the channel stored after a successful connect/2", %{
      ref: ref,
      ip: ip,
      adapter: adapter
    } do
      {:ok, channel} = Connection.connect("ipv4:#{ip}:50051", adapter: adapter, name: ref)

      assert {:ok, picked} = Connection.pick_channel(channel)
      assert picked.host == ip
      assert picked.port == 50051

      Connection.disconnect(channel)
    end
  end

  describe "connect/2 - already_started branch" do
    test "returns {:ok, channel} when the GenServer for the same ref is already running", %{
      ref: ref,
      ip: ip,
      adapter: adapter
    } do
      {:ok, first_channel} = Connection.connect("ipv4:#{ip}:50051", adapter: adapter, name: ref)

      # Connecting again with the same ref triggers the :already_started path
      {:ok, second_channel} = Connection.connect("ipv4:#{ip}:50051", adapter: adapter, name: ref)

      # Both channels share the same ref and point to the same host/port
      assert first_channel.ref == second_channel.ref
      assert second_channel.host == ip
      assert second_channel.port == 50051

      Connection.disconnect(first_channel)
    end

    test "returns {:error, :no_connection} when already_started but no persistent_term entry", %{
      ref: ref,
      ip: ip,
      adapter: adapter
    } do
      {:ok, channel} = Connection.connect("ipv4:#{ip}:50051", adapter: adapter, name: ref)

      # Remove the persistent_term entry to simulate a missing LB state
      :persistent_term.erase({Connection, :lb_state, ref})

      # Calling connect again will hit already_started → pick_channel → :no_connection
      assert {:error, :no_connection} =
               Connection.connect("ipv4:#{ip}:50051", adapter: adapter, name: ref)

      # Restore entry so disconnect works cleanly
      :persistent_term.put({Connection, :lb_state, ref}, channel)
      Connection.disconnect(channel)
    end
  end

  describe "connect/2 - disconnect/1" do
    test "connect succeeds and disconnect returns {:ok, channel}", %{
      ref: ref,
      ip: ip,
      adapter: adapter
    } do
      {:ok, channel} = Connection.connect("ipv4:#{ip}:50051", adapter: adapter, name: ref)

      assert {:ok, _disconnected} = Connection.disconnect(channel)
    end

    test "GenServer process is no longer alive after disconnect", %{
      ref: ref,
      ip: ip,
      adapter: adapter
    } do
      {:ok, channel} = Connection.connect("ipv4:#{ip}:50051", adapter: adapter, name: ref)

      # Grab the PID of the running Connection GenServer before disconnecting
      [{pid, _}] = :global.whereis_name({Connection, ref}) |> List.wrap() |> Enum.map(&{&1, nil})

      {:ok, _} = Connection.disconnect(channel)

      # Give the process a moment to finish stopping
      ref_mon = Process.monitor(pid)
      assert_receive {:DOWN, ^ref_mon, :process, ^pid, _reason}, 500
    end

    test "pick_channel still returns a stale channel after disconnect (persistent_term is not erased)",
         %{ref: ref, ip: ip, adapter: adapter} do
      {:ok, channel} = Connection.connect("ipv4:#{ip}:50051", adapter: adapter, name: ref)

      {:ok, _} = Connection.disconnect(channel)

      # persistent_term is intentionally NOT erased on disconnect —
      # the caller (GRPC.Stub.call) is responsible for detecting stale connections
      # via Process.alive? on the adapter_payload conn_pid.
      assert {:ok, _stale_channel} = Connection.pick_channel(channel)
    end
  end
end
