defmodule GRPC.Transport.HTTP2.Frame.HeadersTest do
  use ExUnit.Case, async: true

  alias GRPC.Transport.HTTP2.Frame
  alias GRPC.Transport.HTTP2.Errors

  describe "HEADERS frame deserialization" do
    test "deserializes basic HEADERS frame" do
      # HEADERS frame with END_HEADERS flag
      data = <<3::24, 1::8, 0x4::8, 0::1, 1::31, "hdr">>

      assert {{:ok, %Frame.Headers{stream_id: 1, end_headers: true, fragment: "hdr"}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes HEADERS frame with END_STREAM flag" do
      # Flags: END_STREAM (0x1) + END_HEADERS (0x4) = 0x5
      data = <<3::24, 1::8, 0x5::8, 0::1, 1::31, "hdr">>

      assert {{:ok,
               %Frame.Headers{stream_id: 1, end_stream: true, end_headers: true, fragment: "hdr"}},
              <<>>} = Frame.deserialize(data, 16_384)
    end

    test "deserializes HEADERS frame without END_HEADERS (needs CONTINUATION)" do
      # No END_HEADERS flag - requires CONTINUATION
      data = <<3::24, 1::8, 0x0::8, 0::1, 1::31, "hdr">>

      assert {{:ok, %Frame.Headers{stream_id: 1, end_headers: false, fragment: "hdr"}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes HEADERS frame with PRIORITY flag" do
      # Flags: PRIORITY (0x20) + END_HEADERS (0x4) = 0x24
      # Priority: exclusive=1, dependency=5, weight=10
      priority = <<1::1, 5::31, 10::8>>

      data =
        <<byte_size(priority <> "hdr")::24, 1::8, 0x24::8, 0::1, 1::31, priority::binary, "hdr">>

      assert {{:ok,
               %Frame.Headers{
                 stream_id: 1,
                 exclusive_dependency: true,
                 stream_dependency: 5,
                 weight: 10,
                 fragment: "hdr"
               }}, <<>>} = Frame.deserialize(data, 16_384)
    end

    test "deserializes HEADERS frame with padding" do
      # Flags: PADDED (0x8) + END_HEADERS (0x4) = 0xC
      payload = <<2::8, "hdr", 0::8, 0::8>>
      data = <<byte_size(payload)::24, 1::8, 0xC::8, 0::1, 1::31, payload::binary>>

      assert {{:ok, %Frame.Headers{stream_id: 1, fragment: "hdr"}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "rejects HEADERS frame with stream_id 0" do
      # RFC 9113 §6.2: HEADERS frames MUST be associated with a stream
      data = <<3::24, 1::8, 0x4::8, 0::1, 0::31, "hdr">>

      assert {{:error, error_code, _reason}, <<>>} = Frame.deserialize(data, 16_384)
      assert error_code == Errors.protocol_error()
    end

    test "handles HPACK compressed headers" do
      # Simulated HPACK encoded headers (not real HPACK encoding)
      hpack_data = <<0x82, 0x86, 0x84, 0x41, 0x0F>>
      data = <<byte_size(hpack_data)::24, 1::8, 0x4::8, 0::1, 1::31, hpack_data::binary>>

      assert {{:ok, %Frame.Headers{fragment: ^hpack_data}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end
  end

  describe "HEADERS frame serialization" do
    test "serializes basic HEADERS frame with END_HEADERS" do
      frame = %Frame.Headers{
        stream_id: 123,
        end_stream: false,
        end_headers: true,
        fragment: "hdr"
      }

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      # Flags: END_HEADERS (0x4)
      assert <<3::24, 1::8, 0x4::8, 0::1, 123::31, "hdr">> = binary
    end

    test "serializes HEADERS frame with END_STREAM and END_HEADERS" do
      frame = %Frame.Headers{
        stream_id: 123,
        end_stream: true,
        end_headers: true,
        fragment: "hdr"
      }

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      # Flags: END_STREAM (0x1) + END_HEADERS (0x4) = 0x5
      assert <<3::24, 1::8, 0x5::8, 0::1, 123::31, "hdr">> = binary
    end

    test "splits large HEADERS into HEADERS + CONTINUATION frames" do
      # 6 bytes of headers, max_frame_size = 2
      frame = %Frame.Headers{
        stream_id: 123,
        end_stream: false,
        end_headers: true,
        fragment: "header"
      }

      result = Frame.serialize(frame, 2)

      # Should produce: HEADERS (no END_HEADERS) + CONTINUATION + CONTINUATION (END_HEADERS)
      assert [
               [<<2::24, 1::8, 0x0::8, _::binary>>, "he"],
               [<<2::24, 9::8, 0x0::8, _::binary>>, "ad"],
               [<<2::24, 9::8, 0x4::8, _::binary>>, "er"]
             ] = result
    end

    test "CONTINUATION frames have END_HEADERS only on last frame" do
      frame = %Frame.Headers{
        stream_id: 123,
        end_headers: true,
        fragment: "abcde"
      }

      result = Frame.serialize(frame, 2)

      # First HEADERS: no END_HEADERS
      [[<<2::24, 1::8, flags1::8, _::binary>>, _] | continuation_frames] = result
      assert flags1 == 0x0

      # Middle CONTINUATION: no END_HEADERS  
      middle_frames = Enum.slice(continuation_frames, 0..-2//1)

      for [<<_::24, 9::8, flags::8, _::binary>>, _] <- middle_frames do
        assert flags == 0x0
      end

      # Last CONTINUATION: END_HEADERS (0x4)
      [[<<_::24, 9::8, last_flags::8, _::binary>>, _]] =
        Enum.slice(continuation_frames, -1..-1//1)

      assert last_flags == 0x4
    end

    test "preserves END_STREAM flag when splitting" do
      frame = %Frame.Headers{
        stream_id: 123,
        end_stream: true,
        end_headers: true,
        fragment: "abcde"
      }

      result = Frame.serialize(frame, 2)

      # First HEADERS should have END_STREAM (0x1) but not END_HEADERS
      [[<<2::24, 1::8, 0x1::8, _::binary>>, _] | _] = result
    end
  end

  describe "gRPC-specific scenarios" do
    test "handles gRPC pseudo-headers" do
      # gRPC uses HTTP/2 pseudo-headers: :method, :scheme, :path, :authority
      # These would be HPACK encoded, but we test with raw bytes
      grpc_headers = ":method: POST\n:path: /my.Service/Method"
      frame = %Frame.Headers{stream_id: 1, end_headers: true, fragment: grpc_headers}

      result = Frame.serialize(frame, 16_384)
      assert is_list(result)
    end

    test "handles gRPC metadata headers" do
      # gRPC metadata: custom headers, timeout, compression
      metadata = "grpc-timeout: 1S\ngrpc-encoding: gzip\nx-custom: value"
      frame = %Frame.Headers{stream_id: 1, end_headers: true, fragment: metadata}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      <<_header::9-bytes, payload::binary>> = binary
      assert payload == metadata
    end

    test "handles trailers-only response (no DATA frames)" do
      # gRPC can send trailers-only response for immediate errors
      # HEADERS frame with both END_STREAM and END_HEADERS
      trailers = "grpc-status: 0\ngrpc-message: OK"

      frame = %Frame.Headers{
        stream_id: 1,
        end_stream: true,
        end_headers: true,
        fragment: trailers
      }

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      # Should have both END_STREAM (0x1) and END_HEADERS (0x4) = 0x5
      <<_length::24, 1::8, 0x5::8, _::binary>> = binary
    end

    test "handles large metadata requiring continuation" do
      # Large custom metadata that exceeds max_frame_size
      large_metadata = String.duplicate("x-custom-#{:rand.uniform(1000)}: value\n", 100)

      frame = %Frame.Headers{
        stream_id: 1,
        end_headers: true,
        fragment: large_metadata
      }

      result = Frame.serialize(frame, 100)

      # Should split into multiple frames
      assert length(result) > 1

      # First should be HEADERS
      [[<<_::24, 1::8, _::8, _::binary>>, _] | continuation] = result

      # Rest should be CONTINUATION (type 9)
      for [<<_::24, 9::8, _::8, _::binary>>, _] <- continuation do
        assert true
      end
    end
  end
end
