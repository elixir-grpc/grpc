defmodule GRPC.Transport.UtilsTest do
  use ExUnit.Case, async: true
  alias GRPC.Transport.Utils

  @max_val 99_999_999
  @us_ceiling 100_000_000
  @ms_ceiling @us_ceiling * 1000
  @second_ceiling @ms_ceiling * 1000
  @minute_ceiling @second_ceiling * 60
  @hour_ceiling @minute_ceiling * 60

  @now_us 1432000000000000
  @now DateTime.from_unix!(@now_us, :microseconds)

  test "normalize_timeout/2 returns 0" do
    deadline = DateTime.from_unix!(@now_us - 1, :microseconds)
    assert Utils.encode_timeout(deadline, @now) == "0u"
    deadline = DateTime.from_unix!(@now_us, :microseconds)
    assert Utils.encode_timeout(deadline, @now) == "0u"
  end

  test "normalize_timeout/2 returns microsecond" do
    deadline = DateTime.from_unix!(@now_us + 1, :microseconds)
    assert Utils.encode_timeout(deadline, @now) == "1u"
    deadline = DateTime.from_unix!(@now_us + @us_ceiling - 1, :microseconds)
    assert Utils.encode_timeout(deadline, @now) == "#{@max_val}u"
  end

  test "normalize_timeout/2 returns millisecond" do
    deadline = DateTime.from_unix!(@now_us + @us_ceiling, :microseconds)
    assert Utils.encode_timeout(deadline, @now) == "100000m"
    deadline = DateTime.from_unix!(@now_us + @ms_ceiling - 1, :microseconds)
    assert Utils.encode_timeout(deadline, @now) == "#{@max_val}m"
  end

  test "normalize_timeout/2 returns second" do
    deadline = DateTime.from_unix!(@now_us + @ms_ceiling, :microseconds)
    assert Utils.encode_timeout(deadline, @now) == "100000S"
    deadline = DateTime.from_unix!(@now_us + @second_ceiling - 1, :microseconds)
    assert Utils.encode_timeout(deadline, @now) == "#{@max_val}S"
  end

  test "normalize_timeout/2 returns minute" do
    deadline = DateTime.from_unix!(@now_us + @second_ceiling, :microseconds)
    assert Utils.encode_timeout(deadline, @now) == "1666666M"
    deadline = DateTime.from_unix!(@now_us + @minute_ceiling - 1, :microseconds)
    assert Utils.encode_timeout(deadline, @now) == "#{@max_val}M"
  end

  test "normalize_timeout/2 returns hour" do
    deadline = DateTime.from_unix!(@now_us + @minute_ceiling, :microseconds)
    assert Utils.encode_timeout(deadline, @now) == "1666666H"
    deadline = DateTime.from_unix!(@now_us + @hour_ceiling - 1, :microseconds)
    assert Utils.encode_timeout(deadline, @now) == "#{@max_val}H"
    deadline = DateTime.from_unix!(@now_us + @hour_ceiling, :microseconds)
    assert Utils.encode_timeout(deadline, @now) == "#{@max_val}H"
  end
end
