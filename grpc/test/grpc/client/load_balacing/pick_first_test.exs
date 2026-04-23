defmodule GRPC.Client.LoadBalancing.PickFirstTest do
  use ExUnit.Case, async: true

  alias GRPC.Channel
  alias GRPC.Client.LoadBalancing.PickFirst

  defp channels(pairs),
    do: Enum.map(pairs, fn {h, p} -> %Channel{host: h, port: p, ref: {h, p}} end)

  describe "init/1" do
    test "returns the first channel as current" do
      {:ok, state} = PickFirst.init(channels: channels([{"a", 1}, {"b", 2}]))
      assert state == %{current: %Channel{host: "a", port: 1, ref: {"a", 1}}}
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
    end

    test "returns :no_channels when current is nil" do
      assert {:error, :no_channels} = PickFirst.pick(%{current: nil})
    end
  end

  describe "update/2" do
    test "swaps current to the first of the new channels" do
      {:ok, state} = PickFirst.init(channels: channels([{"a", 1}]))
      {:ok, new_state} = PickFirst.update(state, channels([{"x", 9}, {"y", 8}]))

      assert {:ok, %Channel{host: "x", port: 9}, _} = PickFirst.pick(new_state)
    end

    test "sets current to nil on empty list" do
      {:ok, state} = PickFirst.init(channels: channels([{"a", 1}]))
      assert {:ok, %{current: nil}} = PickFirst.update(state, [])
    end
  end

  describe "shutdown/1" do
    test "returns :ok (no-op, no external resources)" do
      {:ok, state} = PickFirst.init(channels: channels([{"a", 1}]))
      assert :ok = PickFirst.shutdown(state)
    end
  end
end
