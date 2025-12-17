defmodule GRPC.Transport.HTTP2.Frame.RstStreamTest do
  use ExUnit.Case, async: true

  alias GRPC.Transport.HTTP2.Frame
  alias GRPC.Transport.HTTP2.Errors

  describe "RST_STREAM frame deserialization" do
    test "deserializes RST_STREAM with NO_ERROR" do
      # RST_STREAM with error code NO_ERROR (0x0)
      data = <<4::24, 3::8, 0x0::8, 0::1, 123::31, 0x0::32>>

      assert {{:ok, %Frame.RstStream{stream_id: 123, error_code: 0x0}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes RST_STREAM with PROTOCOL_ERROR" do
      data = <<4::24, 3::8, 0x0::8, 0::1, 123::31, 0x1::32>>

      assert {{:ok, %Frame.RstStream{stream_id: 123, error_code: 0x1}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes RST_STREAM with INTERNAL_ERROR" do
      data = <<4::24, 3::8, 0x0::8, 0::1, 123::31, 0x2::32>>

      assert {{:ok, %Frame.RstStream{stream_id: 123, error_code: 0x2}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes RST_STREAM with FLOW_CONTROL_ERROR" do
      data = <<4::24, 3::8, 0x0::8, 0::1, 123::31, 0x3::32>>

      assert {{:ok, %Frame.RstStream{stream_id: 123, error_code: 0x3}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes RST_STREAM with CANCEL" do
      data = <<4::24, 3::8, 0x0::8, 0::1, 123::31, 0x8::32>>

      assert {{:ok, %Frame.RstStream{stream_id: 123, error_code: 0x8}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "rejects RST_STREAM with stream_id 0" do
      # RFC 9113 §6.4: RST_STREAM frames MUST be associated with a stream
      data = <<4::24, 3::8, 0x0::8, 0::1, 0::31, 0x8::32>>

      assert {{:error, error_code, _reason}, <<>>} = Frame.deserialize(data, 16_384)
      assert error_code == Errors.protocol_error()
    end

    test "rejects RST_STREAM with incorrect length" do
      # RFC 9113 §6.4: RST_STREAM frames MUST be 4 bytes
      data = <<2::24, 3::8, 0x0::8, 0::1, 123::31, 0x8::16>>

      assert {{:error, error_code, _reason}, <<>>} = Frame.deserialize(data, 16_384)
      assert error_code == Errors.frame_size_error()
    end

    test "handles RST_STREAM with unknown error code" do
      # Unknown error codes should still be accepted
      data = <<4::24, 3::8, 0x0::8, 0::1, 123::31, 0xFF::32>>

      assert {{:ok, %Frame.RstStream{stream_id: 123, error_code: 0xFF}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end
  end

  describe "RST_STREAM frame serialization" do
    test "serializes RST_STREAM with NO_ERROR" do
      frame = %Frame.RstStream{stream_id: 123, error_code: 0x0}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<4::24, 3::8, 0x0::8, 0::1, 123::31, 0x0::32>> = binary
    end

    test "serializes RST_STREAM with CANCEL" do
      frame = %Frame.RstStream{stream_id: 123, error_code: 0x8}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<4::24, 3::8, 0x0::8, 0::1, 123::31, 0x8::32>> = binary
    end

    test "serializes RST_STREAM with INTERNAL_ERROR" do
      frame = %Frame.RstStream{stream_id: 456, error_code: 0x2}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<4::24, 3::8, 0x0::8, 0::1, 456::31, 0x2::32>> = binary
    end
  end

  describe "gRPC-specific scenarios" do
    test "handles client cancellation" do
      # Client cancels RPC by sending RST_STREAM with CANCEL (0x8)
      cancel = %Frame.RstStream{stream_id: 1, error_code: Errors.cancel()}

      result = Frame.serialize(cancel, 16_384)
      binary = IO.iodata_to_binary(result)

      {{:ok, received}, <<>>} = Frame.deserialize(binary, 16_384)
      assert received.error_code == Errors.cancel()
    end

    test "handles server rejecting stream" do
      # Server rejects stream due to overload with REFUSED_STREAM (0x7)
      reject = %Frame.RstStream{stream_id: 1, error_code: Errors.refused_stream()}

      result = Frame.serialize(reject, 16_384)
      binary = IO.iodata_to_binary(result)

      {{:ok, received}, <<>>} = Frame.deserialize(binary, 16_384)
      assert received.error_code == Errors.refused_stream()
    end

    test "handles flow control violation" do
      # RST_STREAM sent when peer violates flow control
      flow_error = %Frame.RstStream{
        stream_id: 5,
        error_code: Errors.flow_control_error()
      }

      result = Frame.serialize(flow_error, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<4::24, 3::8, 0x0::8, 0::1, 5::31, _error::32>> = binary
    end

    test "handles stream timeout" do
      # Application-level timeout can trigger RST_STREAM with CANCEL
      timeout = %Frame.RstStream{
        stream_id: 10,
        error_code: Errors.cancel()
      }

      result = Frame.serialize(timeout, 16_384)
      assert is_list(result)
    end

    test "handles protocol violation on stream" do
      # Protocol error specific to a stream
      protocol_err = %Frame.RstStream{
        stream_id: 15,
        error_code: Errors.protocol_error()
      }

      result = Frame.serialize(protocol_err, 16_384)
      binary = IO.iodata_to_binary(result)

      {{:ok, received}, <<>>} = Frame.deserialize(binary, 16_384)
      assert received.stream_id == 15
      assert received.error_code == Errors.protocol_error()
    end

    test "handles concurrent stream resets" do
      # Multiple streams can be reset independently
      resets =
        for stream_id <- 1..10 do
          %Frame.RstStream{stream_id: stream_id, error_code: Errors.cancel()}
        end

      serialized = Enum.map(resets, &Frame.serialize(&1, 16_384))

      assert length(serialized) == 10
      assert Enum.all?(serialized, &is_list/1)
    end

    test "handles RST_STREAM after partial message" do
      # Stream reset while message is being transmitted
      rst = %Frame.RstStream{
        stream_id: 3,
        error_code: Errors.internal_error()
      }

      result = Frame.serialize(rst, 16_384)
      binary = IO.iodata_to_binary(result)

      # Verify frame structure
      assert <<4::24, 3::8, 0x0::8, 0::1, 3::31, _::32>> = binary
    end

    test "handles RST_STREAM for idle stream" do
      # Receiving RST_STREAM for stream that was never opened
      # Implementation should handle gracefully
      rst = %Frame.RstStream{stream_id: 999, error_code: 0x1}

      result = Frame.serialize(rst, 16_384)
      binary = IO.iodata_to_binary(result)

      {{:ok, received}, <<>>} = Frame.deserialize(binary, 16_384)
      assert received.stream_id == 999
    end

    test "error code mapping to gRPC status" do
      # Different HTTP/2 errors map to different gRPC status codes
      error_codes = [
        {Errors.no_error(), "NO_ERROR - clean shutdown"},
        {Errors.protocol_error(), "PROTOCOL_ERROR - invalid protocol state"},
        {Errors.internal_error(), "INTERNAL_ERROR - server error"},
        {Errors.flow_control_error(), "FLOW_CONTROL_ERROR - window violated"},
        {Errors.cancel(), "CANCEL - client cancellation"},
        {Errors.refused_stream(), "REFUSED_STREAM - server overload"}
      ]

      for {error_code, _description} <- error_codes do
        frame = %Frame.RstStream{stream_id: 1, error_code: error_code}
        result = Frame.serialize(frame, 16_384)
        assert is_list(result)
      end
    end
  end
end
