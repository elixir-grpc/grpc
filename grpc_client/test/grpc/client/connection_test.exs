defmodule GRPC.Client.ConnectionTest do
  use GRPC.Client.DataCase, async: false

  alias GRPC.Channel
  alias GRPC.Client.Connection

  @adapter GRPC.Test.ClientAdapter

  # Helper to build a unique ref so tests don't share persistent_term state
  defp unique_ref, do: make_ref()

  defp connect_with_ref(ref) do
    Connection.connect("ipv4:127.0.0.1:50051", adapter: @adapter, name: ref)
  end

  defp disconnect_channel(channel) do
    Connection.disconnect(channel)
  end

  describe "pick_channel/2" do
    test "returns {:error, :no_connection} when no persistent_term entry exists" do
      channel = %Channel{ref: unique_ref()}

      assert {:error, :no_connection} = Connection.pick_channel(channel)
    end

    test "returns {:ok, channel} when a channel is stored in persistent_term" do
      ref = unique_ref()
      stored_channel = %Channel{ref: ref, host: "127.0.0.1", port: 50051}

      :persistent_term.put({Connection, :lb_state, ref}, stored_channel)

      assert {:ok, ^stored_channel} = Connection.pick_channel(%Channel{ref: ref})

      :persistent_term.erase({Connection, :lb_state, ref})
    end

    test "ignores the opts argument and still resolves by channel ref" do
      ref = unique_ref()
      stored_channel = %Channel{ref: ref, host: "10.0.0.1", port: 9090}

      :persistent_term.put({Connection, :lb_state, ref}, stored_channel)

      assert {:ok, ^stored_channel} =
               Connection.pick_channel(%Channel{ref: ref}, timeout: 5_000)

      :persistent_term.erase({Connection, :lb_state, ref})
    end

    test "returns the channel stored after a successful connect/2" do
      ref = unique_ref()
      {:ok, channel} = connect_with_ref(ref)

      assert {:ok, picked} = Connection.pick_channel(channel)
      assert picked.host == "127.0.0.1"
      assert picked.port == 50051

      disconnect_channel(channel)
    end
  end

  describe "connect/2 - already_started branch" do
    test "returns {:ok, channel} when the GenServer for the same ref is already running" do
      ref = unique_ref()
      {:ok, first_channel} = connect_with_ref(ref)

      # Connecting again with the same ref triggers the :already_started path
      {:ok, second_channel} = connect_with_ref(ref)

      # Both channels share the same ref and point to the same host/port
      assert first_channel.ref == second_channel.ref
      assert second_channel.host == "127.0.0.1"
      assert second_channel.port == 50051

      disconnect_channel(first_channel)
    end

    test "returns {:error, :no_connection} when already_started but no persistent_term entry" do
      ref = unique_ref()
      {:ok, channel} = connect_with_ref(ref)

      # Remove the persistent_term entry to simulate a missing LB state
      :persistent_term.erase({Connection, :lb_state, ref})

      # Calling connect again will hit already_started → pick_channel → :no_connection
      assert {:error, :no_connection} = connect_with_ref(ref)

      # Restore entry so disconnect works cleanly
      :persistent_term.put({Connection, :lb_state, ref}, channel)
      disconnect_channel(channel)
    end
  end

  describe "connect/2 - disconnect/1" do
    test "connect succeeds and disconnect returns {:ok, channel}" do
      ref = unique_ref()
      {:ok, channel} = connect_with_ref(ref)

      assert {:ok, _disconnected} = disconnect_channel(channel)
    end

    test "GenServer process is no longer alive after disconnect" do
      ref = unique_ref()
      {:ok, channel} = connect_with_ref(ref)

      # Grab the PID of the running Connection GenServer before disconnecting
      [{pid, _}] = :global.whereis_name({Connection, ref}) |> List.wrap() |> Enum.map(&{&1, nil})

      {:ok, _} = disconnect_channel(channel)

      # Give the process a moment to finish stopping
      ref_mon = Process.monitor(pid)
      assert_receive {:DOWN, ^ref_mon, :process, ^pid, _reason}, 500
    end

    test "pick_channel still returns a stale channel after disconnect (persistent_term is not erased)" do
      ref = unique_ref()
      {:ok, channel} = connect_with_ref(ref)

      {:ok, _} = disconnect_channel(channel)

      # persistent_term is intentionally NOT erased on disconnect —
      # the caller (GRPC.Stub.call) is responsible for detecting stale connections
      # via Process.alive? on the adapter_payload conn_pid.
      assert {:ok, _stale_channel} = Connection.pick_channel(channel)
    end
  end
end
