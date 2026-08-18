defmodule GRPC.TimeUtilsTest do
  use ExUnit.Case, async: true

  doctest GRPC.TimeUtils

  describe "to_relative/2" do
    test "returns an integer" do
      # A float is dropped by append_timeout/2 rather than sent.
      from = DateTime.utc_now()

      for offset_us <- [1_000, 5_005, 2_000_000, 999] do
        result = GRPC.TimeUtils.to_relative(DateTime.add(from, offset_us, :microsecond), from)
        assert is_integer(result), "got #{inspect(result)} for #{offset_us}us"
      end
    end

    test "truncates rather than rounds, so a deadline is never extended" do
      from = DateTime.utc_now()
      almost_6ms = DateTime.add(from, 5_999, :microsecond)

      assert GRPC.TimeUtils.to_relative(almost_6ms, from) == 5
    end

    test "an already-expired deadline is non-positive" do
      from = DateTime.utc_now()
      past = DateTime.add(from, -1_500, :millisecond)

      assert GRPC.TimeUtils.to_relative(past, from) <= 0
    end
  end
end
