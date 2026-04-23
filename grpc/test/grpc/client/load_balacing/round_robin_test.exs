defmodule GRPC.Client.LoadBalancing.RoundRobinTest do
  @moduledoc """
  Tests for the ETS-backed round-robin load balancer.

  Covers:
    1. init/1 creates an ETS table and returns it in state
    2. init/1 rejects empty channel lists
    3. pick/1 rotates through channels in order
    4. pick/1 wraps around after the last channel
    5. update/2 replaces channels in place without creating a new table
    6. update/2 rejects empty channel lists
    7. update/2 resets the cursor so the first pick is the first channel
    8. shutdown/1 deletes the ETS table
    9. shutdown/1 is idempotent (safe on already-deleted tables)
   10. pick/1 is safe under concurrent access (many processes, one table)
  """
  use ExUnit.Case, async: true

  alias GRPC.Channel
  alias GRPC.Client.LoadBalancing.RoundRobin

  defp channels(pairs),
    do: Enum.map(pairs, fn {h, p} -> %Channel{host: h, port: p, ref: {h, p}} end)

  describe "init/1" do
    test "creates an ETS table and returns the tid in state" do
      {:ok, state} = RoundRobin.init(channels: channels([{"a", 1}]))
      assert %{tid: tid} = state
      assert is_reference(tid)
      assert :ets.info(tid) != :undefined
      RoundRobin.shutdown(state)
    end

    test "rejects empty channel lists" do
      assert {:error, :no_channels} = RoundRobin.init(channels: [])
    end

    test "rejects missing :channels option" do
      assert {:error, :no_channels} = RoundRobin.init([])
    end
  end

  describe "pick/1" do
    test "rotates through channels in order" do
      {:ok, state} = RoundRobin.init(channels: channels([{"a", 1}, {"b", 2}, {"c", 3}]))

      assert {:ok, %Channel{host: "a", port: 1}, _} = RoundRobin.pick(state)
      assert {:ok, %Channel{host: "b", port: 2}, _} = RoundRobin.pick(state)
      assert {:ok, %Channel{host: "c", port: 3}, _} = RoundRobin.pick(state)
      assert {:ok, %Channel{host: "a", port: 1}, _} = RoundRobin.pick(state)

      RoundRobin.shutdown(state)
    end

    test "wraps around with a single channel" do
      {:ok, state} = RoundRobin.init(channels: channels([{"only", 1}]))

      for _ <- 1..5 do
        assert {:ok, %Channel{host: "only"}, _} = RoundRobin.pick(state)
      end

      RoundRobin.shutdown(state)
    end
  end

  describe "update/2" do
    test "replaces channels in place without changing the tid" do
      {:ok, state} = RoundRobin.init(channels: channels([{"a", 1}, {"b", 2}]))
      original_tid = state.tid

      {:ok, new_state} = RoundRobin.update(state, channels([{"x", 9}, {"y", 8}, {"z", 7}]))
      assert new_state.tid == original_tid

      assert {:ok, %Channel{host: "x", port: 9}, _} = RoundRobin.pick(new_state)
      assert {:ok, %Channel{host: "y", port: 8}, _} = RoundRobin.pick(new_state)
      assert {:ok, %Channel{host: "z", port: 7}, _} = RoundRobin.pick(new_state)

      RoundRobin.shutdown(new_state)
    end

    test "accepts empty channel lists; pick then returns :no_channels" do
      {:ok, state} = RoundRobin.init(channels: channels([{"a", 1}]))
      assert {:ok, ^state} = RoundRobin.update(state, [])
      assert {:error, :no_channels} = RoundRobin.pick(state)
      RoundRobin.shutdown(state)
    end

    test "resets cursor so the first pick after update starts at the first channel" do
      {:ok, state} = RoundRobin.init(channels: channels([{"a", 1}, {"b", 2}]))
      {:ok, _, _} = RoundRobin.pick(state)
      {:ok, _, _} = RoundRobin.pick(state)

      {:ok, state} = RoundRobin.update(state, channels([{"new-first", 1}, {"new-second", 2}]))
      assert {:ok, %Channel{host: "new-first"}, _} = RoundRobin.pick(state)

      RoundRobin.shutdown(state)
    end
  end

  describe "pick/1 race with shutdown" do
    test "returns :no_channels instead of raising when the table was deleted" do
      {:ok, state} = RoundRobin.init(channels: channels([{"a", 1}]))
      :ok = RoundRobin.shutdown(state)

      assert {:error, :no_channels} = RoundRobin.pick(state)
    end
  end

  describe "shutdown/1" do
    test "deletes the ETS table" do
      {:ok, state} = RoundRobin.init(channels: channels([{"a", 1}]))
      tid = state.tid
      assert :ets.info(tid) != :undefined

      :ok = RoundRobin.shutdown(state)
      assert :ets.info(tid) == :undefined
    end

    test "is idempotent on already-deleted tables" do
      {:ok, state} = RoundRobin.init(channels: channels([{"a", 1}]))
      :ok = RoundRobin.shutdown(state)
      assert :ok = RoundRobin.shutdown(state)
    end
  end

  describe "concurrency" do
    test "pick/1 is safe under many concurrent processes" do
      chs = channels(for i <- 1..4, do: {"host#{i}", 1000 + i})
      {:ok, state} = RoundRobin.init(channels: chs)

      parent = self()
      picks_per_proc = 250
      procs = 16

      for _ <- 1..procs do
        spawn_link(fn ->
          picks =
            for _ <- 1..picks_per_proc do
              {:ok, %Channel{host: host}, _} = RoundRobin.pick(state)
              host
            end

          send(parent, {:picks, picks})
        end)
      end

      all_picks =
        for _ <- 1..procs, reduce: [] do
          acc ->
            receive do
              {:picks, picks} -> picks ++ acc
            end
        end

      assert length(all_picks) == procs * picks_per_proc

      counts = Enum.frequencies(all_picks)
      avg = div(length(all_picks), length(chs))

      for {_, c} <- counts do
        assert abs(c - avg) <= 1,
               "uneven pick distribution: #{inspect(counts)}"
      end

      RoundRobin.shutdown(state)
    end
  end
end
