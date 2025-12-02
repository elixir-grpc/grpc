defmodule GRPC.Google.RPCTest do
  use ExUnit.Case, async: true

  describe "encode_status/1" do
    test "encodes a valid status" do
      status = %Google.Rpc.Status{
        code: 3,
        message: "Test error",
        details: []
      }

      encoded = GRPC.Google.RPC.encode_status(status)
      assert is_binary(encoded)

      assert {:ok, decoded_status} = GRPC.Google.RPC.decode_status(encoded)
      assert decoded_status.code == status.code
      assert decoded_status.message == status.message
      assert decoded_status.details == status.details
    end

    test "encodes status with details" do
      error_info = %Google.Rpc.ErrorInfo{
        reason: "TEST_REASON",
        domain: "example.com",
        metadata: %{"key" => "value"}
      }

      detail = %Google.Protobuf.Any{
        type_url: "type.googleapis.com/google.rpc.ErrorInfo",
        value: Google.Rpc.ErrorInfo.encode(error_info)
      }

      status = %Google.Rpc.Status{
        code: 3,
        message: "Test error with details",
        details: [detail]
      }

      encoded = GRPC.Google.RPC.encode_status(status)
      assert is_binary(encoded)

      assert {:ok, decoded_status} = GRPC.Google.RPC.decode_status(encoded)
      assert decoded_status.code == status.code
      assert decoded_status.message == status.message
      assert length(decoded_status.details) == 1
    end
  end

  describe "decode_status/1" do
    test "handles padding" do
      valid_status = %Google.Rpc.Status{
        code: 3,
        message: "Test error",
        details: []
      }

      encoded = GRPC.Google.RPC.encode_status(valid_status)

      assert {:ok, status} = GRPC.Google.RPC.decode_status(encoded)
      assert status == valid_status
    end

    test "handles invalid base64" do
      invalid_base64 = "this!!!is!!!not!!!valid!!!base64"

      assert {:error, %ArgumentError{}} = GRPC.Google.RPC.decode_status(invalid_base64)
    end

    test "handles corrupted protobuf data" do
      corrupted_data = Base.encode64("this is not valid protobuf data", padding: true)

      assert {:error, _} = GRPC.Google.RPC.decode_status(corrupted_data)
    end

    test "handles truncated protobuf data" do
      valid_status = %Google.Rpc.Status{
        code: 3,
        message: "Test error",
        details: []
      }

      encoded = GRPC.Google.RPC.encode_status(valid_status)
      decoded_bytes = Base.decode64!(encoded, padding: true)
      truncated = binary_part(decoded_bytes, 0, byte_size(decoded_bytes) - 5)
      truncated_base64 = Base.encode64(truncated, padding: true)

      assert {:error, reason} = GRPC.Google.RPC.decode_status(truncated_base64)

      assert reason == %Protobuf.DecodeError{
               message:
                 ~s(insufficient data decoding field message, expected "Test " to be at least 10 bytes)
             }
    end

    test "handles empty string" do
      assert {:ok, status} = GRPC.Google.RPC.decode_status("")
      assert status.code == 0
      assert status.message == ""
      assert status.details == []
    end

    test "handles valid status with details" do
      error_info = %Google.Rpc.ErrorInfo{
        reason: "TEST_REASON",
        domain: "example.com",
        metadata: %{"key" => "value"}
      }

      detail = %Google.Protobuf.Any{
        type_url: "type.googleapis.com/google.rpc.ErrorInfo",
        value: Google.Rpc.ErrorInfo.encode(error_info)
      }

      status = %Google.Rpc.Status{
        code: 3,
        message: "Test error",
        details: [detail]
      }

      encoded = GRPC.Google.RPC.encode_status(status)
      assert {:ok, decoded_status} = GRPC.Google.RPC.decode_status(encoded)

      assert decoded_status.code == status.code
      assert decoded_status.message == status.message
      assert length(decoded_status.details) == 1

      [decoded_detail] = decoded_status.details
      assert decoded_detail.type_url == detail.type_url
      assert decoded_detail.value == detail.value
    end
  end

  describe "status encoding and decode_status/1" do
    test "handles invalid Any.value content" do
      invalid_detail = %Google.Protobuf.Any{
        type_url: "type.googleapis.com/google.rpc.ErrorInfo",
        value: "this is not a valid encoded ErrorInfo message"
      }

      status = GRPC.Status.invalid_argument()
      message = "Test error"

      encoded =
        GRPC.Google.RPC.encode_status(%Google.Rpc.Status{
          code: status,
          message: message,
          details: [invalid_detail]
        })

      assert {:ok, decoded_status} = GRPC.Google.RPC.decode_status(encoded)
      assert decoded_status.code == status
      assert decoded_status.message == message
      assert length(decoded_status.details) == 1

      [detail] = decoded_status.details
      assert detail.type_url == "type.googleapis.com/google.rpc.ErrorInfo"
      assert detail.value == "this is not a valid encoded ErrorInfo message"

      assert_raise Protobuf.DecodeError, fn ->
        Google.Rpc.ErrorInfo.decode(detail.value)
      end
    end

    test "handles empty Any.value" do
      empty_detail = %Google.Protobuf.Any{
        type_url: "type.googleapis.com/google.rpc.ErrorInfo",
        value: ""
      }

      status = GRPC.Status.invalid_argument()
      message = "Empty detail test"

      encoded =
        GRPC.Google.RPC.encode_status(%Google.Rpc.Status{
          code: status,
          message: message,
          details: [empty_detail]
        })

      assert {:ok, decoded_status} = GRPC.Google.RPC.decode_status(encoded)
      assert decoded_status.code == status
      [detail] = decoded_status.details
      assert detail.value == ""

      decoded_error_info = Google.Rpc.ErrorInfo.decode(detail.value)

      assert decoded_error_info == %Google.Rpc.ErrorInfo{
               reason: "",
               domain: "",
               metadata: %{}
             }
    end

    test "handles missing type_url" do
      detail_no_type = %Google.Protobuf.Any{
        type_url: "",
        value: Google.Rpc.ErrorInfo.encode(%Google.Rpc.ErrorInfo{reason: "TEST"})
      }

      status = GRPC.Status.invalid_argument()
      message = "No type URL"

      encoded =
        GRPC.Google.RPC.encode_status(%Google.Rpc.Status{
          code: status,
          message: message,
          details: [detail_no_type]
        })

      assert {:ok, decoded_status} = GRPC.Google.RPC.decode_status(encoded)
      [detail] = decoded_status.details
      assert detail.type_url == ""
    end

    test "handles very large error details payload" do
      large_metadata = for i <- 1..100, into: %{}, do: {"key#{i}", "value#{i}"}

      details =
        for i <- 1..50 do
          %Google.Protobuf.Any{
            type_url: "type.googleapis.com/google.rpc.ErrorInfo",
            value:
              Google.Rpc.ErrorInfo.encode(%Google.Rpc.ErrorInfo{
                reason: "ERROR_#{i}",
                domain: "example.com",
                metadata: large_metadata
              })
          }
        end

      status = GRPC.Status.invalid_argument()
      message = "Large payload test"

      encoded =
        GRPC.Google.RPC.encode_status(%Google.Rpc.Status{
          code: status,
          message: message,
          details: details
        })

      assert {:ok, decoded_status} = GRPC.Google.RPC.decode_status(encoded)
      assert decoded_status.code == status
      assert decoded_status.message == message
      assert length(decoded_status.details) == 50
    end
  end
end
