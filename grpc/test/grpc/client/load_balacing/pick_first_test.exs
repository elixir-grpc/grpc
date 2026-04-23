defmodule GRPC.Client.LoadBalancing.PickFirstTest do
  use ExUnit.Case, async: true

  alias GRPC.Channel
  alias GRPC.Client.LoadBalancing.PickFirst

  defp channels(pairs),
    do: Enum.map(pairs, fn {h, p} -> %Channel{host: h, port: p, ref: {h, p}} end)

  describe "init/1" do
    test "creates an ETS table and returns the tid in state" do
      {:ok, state} = PickFirst.init(channels: channels([{"a", 1}, {"b", 2}]))
      assert %{tid: tid} = state
      assert is_reference(tid)
      assert :ets.info(tid) != :undefined
      PickFirst.shutdown(state)
    end

    test "seeds the table with the first channel" do
      {:ok, state} = PickFirst.init(channels: channels([{"a", 1}, {"b", 2}]))
      assert {:ok, %Channel{host: "a", port: 1}, ^state} = PickFirst.pick(state)
      PickFirst.shutdown(state)
    end

    test "rejects empty channel lists" do
      assert {:error, :no_channels} = PickFirst.init(channels: [])
    end

    test "rejects missing :channels option" do
      assert {:error, :no_channels} = PickFirst.init([])
    end
  end

  describe "pick/1" do
    test "always returns the current channel" do
      {:ok, state} = PickFirst.init(channels: channels([{"a", 1}, {"b", 2}]))

      for _ <- 1..3 do
        assert {:ok, %Channel{host: "a", port: 1}, ^state} = PickFirst.pick(state)
      end

      PickFirst.shutdown(state)
    end

    test "returns :no_channels when current is nil" do
      {:ok, state} = PickFirst.init(channels: channels([{"a", 1}]))
      {:ok, _} = PickFirst.update(state, [])
      assert {:error, :no_channels} = PickFirst.pick(state)
      PickFirst.shutdown(state)
    end

    test "returns :no_channels instead of raising when the table was deleted" do
      {:ok, state} = PickFirst.init(channels: channels([{"a", 1}]))
      :ok = PickFirst.shutdown(state)
      assert {:error, :no_channels} = PickFirst.pick(state)
    end
  end

  describe "update/2" do
    test "swaps current in place without changing the tid" do
      {:ok, state} = PickFirst.init(channels: channels([{"a", 1}]))
      original_tid = state.tid

      {:ok, new_state} = PickFirst.update(state, channels([{"x", 9}, {"y", 8}]))
      assert new_state.tid == original_tid

      assert {:ok, %Channel{host: "x", port: 9}, _} = PickFirst.pick(new_state)
      PickFirst.shutdown(new_state)
    end

    test "clears current to nil on empty list" do
      {:ok, state} = PickFirst.init(channels: channels([{"a", 1}]))
      {:ok, state} = PickFirst.update(state, [])
      assert {:error, :no_channels} = PickFirst.pick(state)
      PickFirst.shutdown(state)
    end
  end

  describe "shutdown/1" do
    test "deletes the ETS table" do
      {:ok, state} = PickFirst.init(channels: channels([{"a", 1}]))
      tid = state.tid
      assert :ets.info(tid) != :undefined

      :ok = PickFirst.shutdown(state)
      assert :ets.info(tid) == :undefined
    end

    test "is idempotent on already-deleted tables" do
      {:ok, state} = PickFirst.init(channels: channels([{"a", 1}]))
      :ok = PickFirst.shutdown(state)
      assert :ok = PickFirst.shutdown(state)
    end
  end
end
