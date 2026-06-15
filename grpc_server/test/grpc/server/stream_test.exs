defmodule GRPC.Server.StreamTest do
  use ExUnit.Case, async: true

  alias GRPC.Server.Stream

  describe "remaining_ms/1" do
    test "returns :infinity when no deadline" do
      assert Stream.remaining_ms(%Stream{deadline: nil}) == :infinity
    end

    test "returns time left and never goes negative" do
      future = System.monotonic_time(:millisecond) + 1_000
      assert Stream.remaining_ms(%Stream{deadline: future}) in 900..1_000

      past = System.monotonic_time(:millisecond) - 1_000
      assert Stream.remaining_ms(%Stream{deadline: past}) == 0
    end
  end
end
