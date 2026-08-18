defmodule GRPC.Transport.UtilsTest do
  use ExUnit.Case, async: true

  import GRPC.Transport.Utils

  # unit: ns
  @ns_ceiling 1000
  @us_ceiling 1000_000

  # unit: ms, must track GRPC.Transport.Utils.
  @ms_ceiling 100_000_000
  @second_ceiling @ms_ceiling * 1000
  @minute_ceiling @second_ceiling * 60

  test "encode_ns/1 returns 0" do
    assert encode_ns(-1) == "0u"
    assert encode_ns(0) == "0u"
  end

  test "encode_ns/1 returns nanoseconds" do
    assert encode_ns(1) == "1n"
    assert encode_ns(@ns_ceiling - 1) == "999n"
  end

  test "encode_ns/1 returns microseconds" do
    assert encode_ns(@ns_ceiling) == "1u"
    assert encode_ns(@us_ceiling - 1) == "999u"
  end

  test "encode_timeout/1 returns 0" do
    assert encode_timeout(-1) == "0u"
    assert encode_timeout(0) == "0u"
  end

  test "encode_timeout/1 returns millisecond" do
    assert encode_timeout(1) == "1m"
    assert encode_timeout(1500) == "1500m"
    assert encode_timeout(@ms_ceiling - 1) == "99999999m"
  end

  test "encode_timeout/1 returns second" do
    assert encode_timeout(@ms_ceiling) == "100000S"
    assert encode_timeout(@second_ceiling - 1) == "99999999S"
  end

  test "encode_timeout/1 returns minute" do
    assert encode_timeout(@second_ceiling) == "1666666M"
    assert encode_timeout(@minute_ceiling - 1) == "99999999M"
  end

  test "encode_timeout/1 returns hour" do
    assert encode_timeout(@minute_ceiling) == "1666666H"
  end

  describe "encode_timeout/1 fidelity" do
    test "millisecond values survive a round-trip exactly" do
      # Regression guard: with a 1000 ms ceiling, 2500 ms encoded as "2S" and decoded as 2000.
      for ms <- [1, 999, 1000, 1500, 2000, 2500, 3847, 5000, 59_999, 60_000, 3_600_000] do
        assert decode_timeout(encode_timeout(ms)) == ms,
               "#{ms} ms did not survive encode/decode: " <>
                 "#{inspect(encode_timeout(ms))} -> #{decode_timeout(encode_timeout(ms))} ms"
      end
    end

    test "values above the millisecond ceiling lose less than one second" do
      # Past 8 digits of ms a coarser unit is forced, so bound the loss instead.
      ms = @ms_ceiling + 1
      decoded = decode_timeout(encode_timeout(ms))

      assert decoded <= ms
      assert ms - decoded < 1000
    end

    test "the encoded value stays within the 8-digit wire limit" do
      # TimeoutValue is "a positive integer as ASCII string of at most 8 digits".
      for ms <- [
            1,
            @ms_ceiling - 1,
            @ms_ceiling,
            @second_ceiling - 1,
            @second_ceiling,
            @minute_ceiling - 1,
            @minute_ceiling
          ] do
        {digits, _unit} = String.split_at(encode_timeout(ms), -1)

        assert String.length(digits) <= 8,
               "#{ms} ms encoded to #{digits} (#{String.length(digits)} digits)"
      end
    end
  end

  test "decode_timeout/1 returns 0" do
    assert decode_timeout("0u") == 0
  end

  test "decode_timeout/1 returns 0.123" do
    assert decode_timeout("123u") == 0
  end

  test "decode_timeout/1 returns 123 ms" do
    assert decode_timeout("123m") == 123
  end

  test "decode_timeout/1 returns seconds" do
    assert decode_timeout("123S") == 123_000
  end

  test "decode_timeout/1 returns minutes" do
    assert decode_timeout("123M") == 123 * 60_000
  end

  test "decode_timeout/1 returns hour" do
    assert decode_timeout("123H") == 123 * 3_600_000
  end
end
