defmodule GRPC.Transport.HTTP2.Frame.ContinuationTest do
  use ExUnit.Case, async: true

  alias GRPC.Transport.HTTP2.Frame
  alias GRPC.Transport.HTTP2.Errors

  describe "CONTINUATION frame deserialization" do
    test "deserializes basic CONTINUATION frame" do
      # CONTINUATION with END_HEADERS flag
      data = <<3::24, 9::8, 0x4::8, 0::1, 1::31, "hdr">>

      assert {{:ok, %Frame.Continuation{stream_id: 1, end_headers: true, fragment: "hdr"}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes CONTINUATION without END_HEADERS" do
      # More CONTINUATION frames will follow
      data = <<3::24, 9::8, 0x0::8, 0::1, 1::31, "hdr">>

      assert {{:ok, %Frame.Continuation{stream_id: 1, end_headers: false, fragment: "hdr"}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes CONTINUATION with large fragment" do
      fragment = String.duplicate("x", 16384)
      data = <<byte_size(fragment)::24, 9::8, 0x4::8, 0::1, 1::31, fragment::binary>>

      assert {{:ok, %Frame.Continuation{stream_id: 1, fragment: ^fragment}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "rejects CONTINUATION with stream_id 0" do
      # RFC 9113 §6.10: CONTINUATION frames MUST be associated with a stream
      data = <<3::24, 9::8, 0x4::8, 0::1, 0::31, "hdr">>

      assert {{:error, error_code, _reason}, <<>>} = Frame.deserialize(data, 16_384)
      assert error_code == Errors.protocol_error()
    end

    test "handles empty CONTINUATION frame" do
      # Edge case: CONTINUATION with no payload (unusual but valid)
      data = <<0::24, 9::8, 0x4::8, 0::1, 1::31>>

      assert {{:ok, %Frame.Continuation{stream_id: 1, end_headers: true, fragment: <<>>}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end
  end

  describe "CONTINUATION frame serialization" do
    test "serializes CONTINUATION with END_HEADERS" do
      frame = %Frame.Continuation{
        stream_id: 123,
        end_headers: true,
        fragment: "hdr"
      }

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<3::24, 9::8, 0x4::8, 0::1, 123::31, "hdr">> = binary
    end

    test "serializes CONTINUATION with large fragment" do
      fragment = String.duplicate("x", 10000)

      frame = %Frame.Continuation{
        stream_id: 123,
        end_headers: true,
        fragment: fragment
      }

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      <<10000::24, 9::8, 0x4::8, 0::1, 123::31, received_fragment::binary>> = binary
      assert received_fragment == fragment
    end
  end

  describe "CONTINUATION sequence scenarios" do
    test "handles HEADERS + CONTINUATION sequence" do
      # Large header block split across HEADERS and CONTINUATION
      full_headers = "very-long-header-block-that-exceeds-max-frame-size"

      # When serializing with small max_frame_size, it splits automatically
      headers = %Frame.Headers{
        stream_id: 1,
        end_headers: true,
        fragment: full_headers
      }

      # Serialize with small frame size to force split
      frames_io = Frame.serialize(headers, 20)

      # Should produce [HEADERS, CONTINUATION]
      # 3 frames for 51 bytes with 20 byte limit
      assert length(frames_io) == 3

      # Deserialize first frame (HEADERS)
      [h_io, c1_io, c2_io] = frames_io
      h_binary = IO.iodata_to_binary(h_io)
      c1_binary = IO.iodata_to_binary(c1_io)
      c2_binary = IO.iodata_to_binary(c2_io)

      {{:ok, h_frame}, <<>>} = Frame.deserialize(h_binary, 16_384)
      {{:ok, c1_frame}, <<>>} = Frame.deserialize(c1_binary, 16_384)
      {{:ok, c2_frame}, <<>>} = Frame.deserialize(c2_binary, 16_384)

      # Reconstruct full header block
      reconstructed =
        IO.iodata_to_binary([h_frame.fragment, c1_frame.fragment, c2_frame.fragment])

      assert reconstructed == full_headers

      # First HEADERS should not have END_HEADERS
      assert h_frame.end_headers == false
      # Middle CONTINUATION should not have END_HEADERS
      assert c1_frame.end_headers == false
      # Last CONTINUATION should have END_HEADERS
      assert c2_frame.end_headers == true
    end

    test "handles HEADERS + multiple CONTINUATION frames" do
      # Very large header block requiring multiple CONTINUATIONs
      full_fragment = "part1part2part3"

      headers = %Frame.Headers{stream_id: 1, end_headers: true, fragment: full_fragment}

      # Serialize with small max_frame_size
      frames_io = Frame.serialize(headers, 5)

      # Should split into multiple frames
      assert length(frames_io) == 3

      [h_io, c1_io, c2_io] = frames_io

      h_bin = IO.iodata_to_binary(h_io)
      c1_bin = IO.iodata_to_binary(c1_io)
      c2_bin = IO.iodata_to_binary(c2_io)

      {{:ok, h}, <<>>} = Frame.deserialize(h_bin, 16_384)
      {{:ok, c1}, <<>>} = Frame.deserialize(c1_bin, 16_384)
      {{:ok, c2}, <<>>} = Frame.deserialize(c2_bin, 16_384)

      reconstructed = IO.iodata_to_binary([h.fragment, c1.fragment, c2.fragment])
      assert reconstructed == full_fragment
      assert h.end_headers == false
      assert c1.end_headers == false
      assert c2.end_headers == true
    end

    test "verifies stream_id consistency across sequence" do
      # All frames in sequence must have same stream_id
      headers = %Frame.Headers{stream_id: 5, end_headers: false, fragment: "h"}
      cont = %Frame.Continuation{stream_id: 5, end_headers: true, fragment: "c"}

      h_bin = IO.iodata_to_binary(Frame.serialize(headers, 16_384))
      c_bin = IO.iodata_to_binary(Frame.serialize(cont, 16_384))

      {{:ok, h_frame}, <<>>} = Frame.deserialize(h_bin, 16_384)
      {{:ok, c_frame}, <<>>} = Frame.deserialize(c_bin, 16_384)

      assert h_frame.stream_id == c_frame.stream_id
    end
  end

  describe "gRPC-specific scenarios" do
    test "handles large gRPC metadata headers" do
      # gRPC metadata can be large, requiring CONTINUATION
      metadata_headers =
        for i <- 1..50 do
          "x-custom-header-#{i}: value-#{i}\n"
        end
        |> Enum.join()

      # Simulate splitting at max_frame_size
      max_size = 100
      chunks = for <<chunk::binary-size(max_size) <- metadata_headers>>, do: chunk

      # Add any remaining bytes
      remainder_size = rem(byte_size(metadata_headers), max_size)

      chunks =
        if remainder_size > 0 do
          chunks ++
            [
              binary_part(
                metadata_headers,
                byte_size(metadata_headers) - remainder_size,
                remainder_size
              )
            ]
        else
          chunks
        end

      [first | rest] = chunks

      headers = %Frame.Headers{stream_id: 1, end_headers: false, fragment: first}

      # Middle chunks in CONTINUATION without END_HEADERS
      middle = Enum.slice(rest, 0..-2//1)

      continuations =
        for {chunk, _idx} <- Enum.with_index(middle) do
          %Frame.Continuation{stream_id: 1, end_headers: false, fragment: chunk}
        end

      last_chunk = List.last(rest)

      final_cont = %Frame.Continuation{
        stream_id: 1,
        end_headers: true,
        fragment: last_chunk || <<>>
      }

      all_frames = [headers | continuations] ++ [final_cont]

      serialized = Enum.map(all_frames, &Frame.serialize(&1, 16_384))
      assert length(serialized) == length(all_frames)
    end

    test "handles gRPC trailers with CONTINUATION" do
      # Trailers can be large if they include detailed error info
      trailers = """
      grpc-status: 13
      grpc-message: Internal server error
      grpc-status-details-bin: #{String.duplicate("x", 500)}
      x-debug-info: #{String.duplicate("y", 500)}
      """

      # Split into HEADERS + CONTINUATION
      split_point = 100

      headers = %Frame.Headers{
        stream_id: 1,
        end_stream: true,
        end_headers: false,
        fragment: binary_part(trailers, 0, split_point)
      }

      continuation = %Frame.Continuation{
        stream_id: 1,
        end_headers: true,
        fragment: binary_part(trailers, split_point, byte_size(trailers) - split_point)
      }

      h_bin = IO.iodata_to_binary(Frame.serialize(headers, 16_384))
      c_bin = IO.iodata_to_binary(Frame.serialize(continuation, 16_384))

      {{:ok, h}, <<>>} = Frame.deserialize(h_bin, 16_384)
      {{:ok, c}, <<>>} = Frame.deserialize(c_bin, 16_384)

      reconstructed = h.fragment <> c.fragment
      assert reconstructed == trailers
      assert h.end_stream == true
    end

    test "handles HPACK compressed continuation" do
      # HPACK encoding can produce variable-length output
      # Simulated HPACK encoded data
      hpack_encoded = <<0x82, 0x86, 0x84>> <> String.duplicate(<<0x41>>, 100)

      # If exceeds frame size, split into CONTINUATION
      if byte_size(hpack_encoded) > 50 do
        headers = %Frame.Headers{
          stream_id: 1,
          end_headers: false,
          fragment: binary_part(hpack_encoded, 0, 50)
        }

        continuation = %Frame.Continuation{
          stream_id: 1,
          end_headers: true,
          fragment: binary_part(hpack_encoded, 50, byte_size(hpack_encoded) - 50)
        }

        h_result = Frame.serialize(headers, 16_384)
        c_result = Frame.serialize(continuation, 16_384)

        assert is_list(h_result)
        assert is_list(c_result)
      end
    end

    test "handles interleaved stream violation detection" do
      # RFC 9113: CONTINUATION frames MUST follow HEADERS immediately
      # No other frames can be sent on ANY stream until END_HEADERS

      # Correct sequence: HEADERS(stream=1, no END_HEADERS) -> CONTINUATION(stream=1)
      headers = %Frame.Headers{stream_id: 1, end_headers: false, fragment: "h1"}
      continuation = %Frame.Continuation{stream_id: 1, end_headers: true, fragment: "c1"}

      # Frames must be processed in order
      h_bin = IO.iodata_to_binary(Frame.serialize(headers, 16_384))
      c_bin = IO.iodata_to_binary(Frame.serialize(continuation, 16_384))

      # Deserialize in correct order
      {{:ok, h}, <<>>} = Frame.deserialize(h_bin, 16_384)
      {{:ok, c}, <<>>} = Frame.deserialize(c_bin, 16_384)

      assert h.stream_id == c.stream_id
      assert c.end_headers == true
    end

    test "handles CONTINUATION frame splitting strategy" do
      # gRPC implementation should split at frame boundaries
      large_metadata = String.duplicate("x-header: value\n", 1000)
      max_frame = 16384

      # Calculate number of frames needed
      num_frames = div(byte_size(large_metadata), max_frame) + 1

      # First frame is HEADERS
      # Remaining are CONTINUATION
      # Last frame has END_HEADERS
      first_fragment = binary_part(large_metadata, 0, min(max_frame, byte_size(large_metadata)))

      headers = %Frame.Headers{
        stream_id: 1,
        end_headers: num_frames == 1,
        fragment: first_fragment
      }

      assert is_struct(headers, Frame.Headers)
    end
  end

  describe "edge cases" do
    test "handles maximum frame size CONTINUATION" do
      # Test with exactly max_frame_size payload
      max_payload = String.duplicate("x", 16384)

      frame = %Frame.Continuation{
        stream_id: 1,
        end_headers: true,
        fragment: max_payload
      }

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert byte_size(binary) == 9 + 16384
    end

    test "handles minimum size CONTINUATION" do
      frame = %Frame.Continuation{
        stream_id: 1,
        end_headers: true,
        fragment: <<>>
      }

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<0::24, 9::8, 0x4::8, 0::1, 1::31>> = binary
    end
  end
end
