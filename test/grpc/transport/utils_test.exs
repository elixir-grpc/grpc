defmodule GRPC.Transport.UtilsTest do
  use ExUnit.Case, async: true
  alias GRPC.Transport.Utils

  @max_val 99_999_999
  @us_ceiling 100_000_000
  @ms_ceiling @us_ceiling * 1000
  @second_ceiling @ms_ceiling * 1000
  @minute_ceiling @second_ceiling * 60
  @hour_ceiling @minute_ceiling * 60

  test "normalize_timeout/1 returns 0" do
    assert Utils.encode_timeout(-1) == "0u"
    assert Utils.encode_timeout(0) == "0u"
  end

  test "normalize_timeout/1 returns microsecond" do
    assert Utils.encode_timeout(1) == "1u"
    assert Utils.encode_timeout(@us_ceiling - 1) == "#{@max_val}u"
  end

  test "normalize_timeout/1 returns millisecond" do
    assert Utils.encode_timeout(@us_ceiling) == "100000m"
    assert Utils.encode_timeout(@ms_ceiling - 1) == "#{@max_val}m"
  end

  test "normalize_timeout/1 returns second" do
    assert Utils.encode_timeout(@ms_ceiling) == "100000S"
    assert Utils.encode_timeout(@second_ceiling - 1) == "#{@max_val}S"
  end

  test "normalize_timeout/1 returns minute" do
    assert Utils.encode_timeout(@second_ceiling) == "1666666M"
    assert Utils.encode_timeout(@minute_ceiling - 1) == "#{@max_val}M"
  end

  test "normalize_timeout/1 returns hour" do
    assert Utils.encode_timeout(@minute_ceiling) == "1666666H"
    assert Utils.encode_timeout(@hour_ceiling - 1) == "#{@max_val}H"
    assert Utils.encode_timeout(@hour_ceiling) == "#{@max_val}H"
  end

  test "normalize_timeout/1 accepts deadline" do
    deadline = DateTime.utc_now
    assert Utils.encode_timeout(deadline) == "0u"
  end

  test "normalize_timeout/2 accepts deadline" do
    now = DateTime.utc_now
    now_us = DateTime.to_unix(now, :microseconds)
    deadline = DateTime.from_unix!(now_us + 5, :microseconds)
    assert Utils.encode_timeout(deadline, now) == "5u"
  end
end
