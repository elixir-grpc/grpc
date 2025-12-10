defmodule GRPC.Transport.HTTP2.Frame.GoawayTest do
  use ExUnit.Case, async: true

  alias GRPC.Transport.HTTP2.Frame
  alias GRPC.Transport.HTTP2.Errors

  describe "GOAWAY frame deserialization" do
    test "deserializes GOAWAY frame" do
      # GOAWAY: last_stream_id=123, error=NO_ERROR, debug=""
      payload = <<0::1, 123::31, 0x0::32>>
      data = <<byte_size(payload)::24, 7::8, 0x0::8, 0::1, 0::31, payload::binary>>

      assert {{:ok, %Frame.Goaway{last_stream_id: 123, error_code: 0x0, debug_data: <<>>}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes GOAWAY with debug data" do
      debug = "shutting down"
      payload = <<0::1, 123::31, 0x0::32, debug::binary>>
      data = <<byte_size(payload)::24, 7::8, 0x0::8, 0::1, 0::31, payload::binary>>

      assert {{:ok, %Frame.Goaway{last_stream_id: 123, error_code: 0x0, debug_data: ^debug}},
              <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes GOAWAY with PROTOCOL_ERROR" do
      payload = <<0::1, 50::31, 0x1::32>>
      data = <<byte_size(payload)::24, 7::8, 0x0::8, 0::1, 0::31, payload::binary>>

      assert {{:ok, %Frame.Goaway{last_stream_id: 50, error_code: 0x1, debug_data: <<>>}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes GOAWAY with ENHANCE_YOUR_CALM" do
      payload = <<0::1, 10::31, 0xB::32>>
      data = <<byte_size(payload)::24, 7::8, 0x0::8, 0::1, 0::31, payload::binary>>

      assert {{:ok, %Frame.Goaway{last_stream_id: 10, error_code: 0xB, debug_data: <<>>}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes GOAWAY with last_stream_id 0" do
      # No streams were processed
      payload = <<0::1, 0::31, 0x0::32>>
      data = <<byte_size(payload)::24, 7::8, 0x0::8, 0::1, 0::31, payload::binary>>

      assert {{:ok, %Frame.Goaway{last_stream_id: 0, error_code: 0x0, debug_data: <<>>}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "rejects GOAWAY with non-zero stream_id" do
      # RFC 9113 §6.8: GOAWAY frames MUST be associated with stream 0
      payload = <<0::1, 123::31, 0x0::32>>
      data = <<byte_size(payload)::24, 7::8, 0x0::8, 0::1, 1::31, payload::binary>>

      assert {{:error, error_code, _reason}, <<>>} = Frame.deserialize(data, 16_384)
      assert error_code == Errors.protocol_error()
    end

    # Note: GOAWAY deserialization uses pattern matching, so insufficient length
    # causes a function clause error rather than returning an error tuple
    # This is by design - the frame parser validates lengths before deserialization

    test "handles GOAWAY with large debug data" do
      debug = String.duplicate("debug info ", 100)
      payload = <<0::1, 123::31, 0x0::32, debug::binary>>
      data = <<byte_size(payload)::24, 7::8, 0x0::8, 0::1, 0::31, payload::binary>>

      assert {{:ok, %Frame.Goaway{debug_data: ^debug}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "handles reserved bit correctly" do
      # Reserved bit should be ignored
      payload = <<1::1, 123::31, 0x0::32>>
      data = <<byte_size(payload)::24, 7::8, 0x0::8, 0::1, 0::31, payload::binary>>

      assert {{:ok, %Frame.Goaway{last_stream_id: 123}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end
  end

  describe "GOAWAY frame serialization" do
    test "serializes GOAWAY with NO_ERROR" do
      frame = %Frame.Goaway{
        last_stream_id: 123,
        error_code: 0x0,
        debug_data: <<>>
      }

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<8::24, 7::8, 0x0::8, 0::1, 0::31, 0::1, 123::31, 0x0::32>> = binary
    end

    test "serializes GOAWAY with debug data" do
      debug = "shutting down"

      frame = %Frame.Goaway{
        last_stream_id: 123,
        error_code: 0x0,
        debug_data: debug
      }

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      payload_length = 8 + byte_size(debug)

      <<^payload_length::24, 7::8, 0x0::8, 0::1, 0::31, 0::1, 123::31, 0x0::32, ^debug::binary>> =
        binary
    end

    test "serializes GOAWAY with INTERNAL_ERROR" do
      frame = %Frame.Goaway{
        last_stream_id: 50,
        error_code: 0x2,
        debug_data: <<>>
      }

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<8::24, 7::8, 0x0::8, 0::1, 0::31, 0::1, 50::31, 0x2::32>> = binary
    end

    test "sets reserved bit to 0" do
      frame = %Frame.Goaway{
        last_stream_id: 123,
        error_code: 0x0,
        debug_data: <<>>
      }

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      <<_::9-bytes, reserved::1, _last_stream::31, _error::32>> = binary
      assert reserved == 0
    end
  end

  describe "gRPC-specific scenarios" do
    test "handles graceful server shutdown" do
      # Server initiates graceful shutdown with NO_ERROR
      # Sets last_stream_id to highest processed stream
      shutdown = %Frame.Goaway{
        last_stream_id: 99,
        error_code: Errors.no_error(),
        debug_data: "server shutting down gracefully"
      }

      result = Frame.serialize(shutdown, 16_384)
      binary = IO.iodata_to_binary(result)

      {{:ok, received}, <<>>} = Frame.deserialize(binary, 16_384)
      assert received.last_stream_id == 99
      assert received.error_code == Errors.no_error()
      assert received.debug_data =~ "graceful"
    end

    test "handles connection timeout" do
      # Connection idle timeout triggers GOAWAY
      timeout = %Frame.Goaway{
        last_stream_id: 50,
        error_code: Errors.no_error(),
        debug_data: "idle timeout"
      }

      result = Frame.serialize(timeout, 16_384)
      assert is_list(result)
    end

    test "handles protocol violation shutdown" do
      # Peer violates protocol, connection terminated
      violation = %Frame.Goaway{
        last_stream_id: 25,
        error_code: Errors.protocol_error(),
        debug_data: "invalid frame sequence"
      }

      result = Frame.serialize(violation, 16_384)
      binary = IO.iodata_to_binary(result)

      {{:ok, received}, <<>>} = Frame.deserialize(binary, 16_384)
      assert received.error_code == Errors.protocol_error()
    end

    test "handles connection overload" do
      # Server overloaded, sends GOAWAY with ENHANCE_YOUR_CALM
      overload = %Frame.Goaway{
        last_stream_id: 10,
        error_code: Errors.enhance_your_calm(),
        debug_data: "too many requests"
      }

      result = Frame.serialize(overload, 16_384)
      binary = IO.iodata_to_binary(result)

      {{:ok, received}, <<>>} = Frame.deserialize(binary, 16_384)
      assert received.error_code == Errors.enhance_your_calm()
    end

    test "handles client-initiated close" do
      # Client closes connection cleanly
      close = %Frame.Goaway{
        last_stream_id: 0,
        error_code: Errors.no_error(),
        debug_data: <<>>
      }

      result = Frame.serialize(close, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<8::24, 7::8, 0x0::8, 0::1, 0::31, 0::1, 0::31, _::32>> = binary
    end

    test "handles two-phase shutdown" do
      # Server sends GOAWAY with high last_stream_id
      # Waits for in-flight streams to complete
      # Sends final GOAWAY with actual last_stream_id

      phase1 = %Frame.Goaway{
        last_stream_id: 0x7FFFFFFF,
        error_code: Errors.no_error(),
        debug_data: "draining connections"
      }

      phase2 = %Frame.Goaway{
        last_stream_id: 42,
        error_code: Errors.no_error(),
        debug_data: "final shutdown"
      }

      phase1_binary = IO.iodata_to_binary(Frame.serialize(phase1, 16_384))
      phase2_binary = IO.iodata_to_binary(Frame.serialize(phase2, 16_384))

      {{:ok, p1}, <<>>} = Frame.deserialize(phase1_binary, 16_384)
      {{:ok, p2}, <<>>} = Frame.deserialize(phase2_binary, 16_384)

      assert p1.last_stream_id == 0x7FFFFFFF
      assert p2.last_stream_id == 42
    end

    test "handles internal error during processing" do
      # Unexpected internal error triggers immediate shutdown
      internal = %Frame.Goaway{
        last_stream_id: 15,
        error_code: Errors.internal_error(),
        debug_data: "unexpected exception in handler"
      }

      result = Frame.serialize(internal, 16_384)
      binary = IO.iodata_to_binary(result)

      {{:ok, received}, <<>>} = Frame.deserialize(binary, 16_384)
      assert received.error_code == Errors.internal_error()
    end

    test "handles flow control error shutdown" do
      # Global flow control violation requires connection close
      flow_error = %Frame.Goaway{
        last_stream_id: 20,
        error_code: Errors.flow_control_error(),
        debug_data: "connection window exceeded"
      }

      result = Frame.serialize(flow_error, 16_384)
      binary = IO.iodata_to_binary(result)

      {{:ok, received}, <<>>} = Frame.deserialize(binary, 16_384)
      assert received.error_code == Errors.flow_control_error()
    end

    test "includes diagnostic info in debug data" do
      # Useful debug information for troubleshooting
      diagnostic = %Frame.Goaway{
        last_stream_id: 30,
        error_code: Errors.protocol_error(),
        debug_data: "frame_type=1 stream_id=31 error=invalid_headers"
      }

      result = Frame.serialize(diagnostic, 16_384)
      binary = IO.iodata_to_binary(result)

      {{:ok, received}, <<>>} = Frame.deserialize(binary, 16_384)
      assert received.debug_data =~ "frame_type"
      assert received.debug_data =~ "invalid_headers"
    end

    test "handles connection state after GOAWAY" do
      # After sending GOAWAY, no new streams should be created
      # Existing streams can complete

      goaway = %Frame.Goaway{
        last_stream_id: 10,
        error_code: Errors.no_error(),
        debug_data: "no new streams"
      }

      result = Frame.serialize(goaway, 16_384)
      binary = IO.iodata_to_binary(result)

      # Any stream_id <= 10 can still send frames
      # Any stream_id > 10 should be rejected
      {{:ok, %{last_stream_id: last_stream}}, <<>>} = Frame.deserialize(binary, 16_384)
      assert last_stream == 10
    end
  end

  describe "error code mapping" do
    test "maps all standard HTTP/2 error codes" do
      error_scenarios = [
        {Errors.no_error(), "clean shutdown"},
        {Errors.protocol_error(), "protocol violation"},
        {Errors.internal_error(), "internal server error"},
        {Errors.flow_control_error(), "flow control violation"},
        {Errors.settings_timeout(), "settings timeout"},
        {Errors.stream_closed(), "frame on closed stream"},
        {Errors.frame_size_error(), "invalid frame size"},
        {Errors.refused_stream(), "stream refused"},
        {Errors.cancel(), "operation cancelled"},
        {Errors.compression_error(), "compression error"},
        {Errors.connect_error(), "connect error"},
        {Errors.enhance_your_calm(), "excessive load"},
        {Errors.inadequate_security(), "security requirements not met"},
        {Errors.http_1_1_requires(), "HTTP/1.1 required"}
      ]

      for {error_code, description} <- error_scenarios do
        frame = %Frame.Goaway{
          last_stream_id: 1,
          error_code: error_code,
          debug_data: description
        }

        result = Frame.serialize(frame, 16_384)
        binary = IO.iodata_to_binary(result)

        {{:ok, received}, <<>>} = Frame.deserialize(binary, 16_384)
        assert received.error_code == error_code
        assert received.debug_data == description
      end
    end
  end
end
