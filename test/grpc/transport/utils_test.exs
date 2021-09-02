defmodule GRPC.Transport.UtilsTest do
  use ExUnit.Case, async: true

  import GRPC.Transport.Utils

  # unit: ns
  @ns_ceiling 1000
  @us_ceiling 1000_000

  # unit: ms
  @ms_ceiling 1000
  @second_ceiling @ms_ceiling * 60
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
    assert encode_timeout(@ms_ceiling - 1) == "999m"
  end

  test "encode_timeout/1 returns second" do
    assert encode_timeout(@ms_ceiling) == "1S"
    assert encode_timeout(@second_ceiling - 1) == "59S"
  end

  test "encode_timeout/1 returns minute" do
    assert encode_timeout(@second_ceiling) == "1M"
    assert encode_timeout(@minute_ceiling - 1) == "59M"
  end

  test "encode_timeout/1 returns hour" do
    assert encode_timeout(@minute_ceiling) == "1H"
    assert encode_timeout(@minute_ceiling * 24) == "24H"
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

  test "encode_status_details/2 returns empty string" do
    assert encode_status_details(GRPC.Status.invalid_argument(), nil) == ""
  end

  test "encode_status_details/2 returns status with no details" do
    status_details = encode_status_details(GRPC.Status.invalid_argument(), [])

    assert Google.Rpc.Status.decode(status_details) == %Google.Rpc.Status{
             code: 3,
             details: [],
             message: ""
           }
  end

  test "encode_status_details/2 returns status with correct type_url" do
    status_details =
      encode_status_details(GRPC.Status.invalid_argument(), [
        Google.Rpc.QuotaFailure.new(
          violations: [%{subject: "test", description: "Limit one greeting per person"}]
        )
      ])

    %Google.Rpc.Status{code: 3, details: [%Google.Protobuf.Any{type_url: type_url}]} =
      Google.Rpc.Status.decode(status_details)

    assert type_url == "type.googleapis.com/google.rpc.QuotaFailure"
  end

  test "decode_status_details/1 decodes status details succesfully" do
    details = [
      Google.Rpc.QuotaFailure.new(
        violations: [%{subject: "test", description: "Limit one greeting per person"}]
      ),
      Google.Rpc.BadRequest.new(
        violations: [%{description: "some description", field: "some field"}]
      )
    ]

    encoded_status_details = encode_status_details(GRPC.Status.invalid_argument(), details)
    decoded_status_details = decode_status_details(encoded_status_details)

    assert details == decoded_status_details
  end
end
