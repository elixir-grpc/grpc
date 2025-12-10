defmodule GRPC.Transport.HTTP2.Frame.DataTest do
  use ExUnit.Case, async: true

  alias GRPC.Transport.HTTP2.Frame
  alias GRPC.Transport.HTTP2.Errors

  describe "DATA frame deserialization" do
    test "deserializes basic DATA frame" do
      # DATA frame: stream_id=1, no padding, end_stream=false, data="hello"
      data = <<5::24, 0::8, 0::8, 0::1, 1::31, "hello">>

      assert {{:ok, %Frame.Data{stream_id: 1, end_stream: false, data: "hello"}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes DATA frame with END_STREAM flag" do
      # Flags: END_STREAM (0x1)
      data = <<5::24, 0::8, 0x1::8, 0::1, 1::31, "hello">>

      assert {{:ok, %Frame.Data{stream_id: 1, end_stream: true, data: "hello"}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes DATA frame with padding" do
      # Flags: PADDED (0x8), padding_length=3
      payload = <<3::8, "hello", 0::8, 0::8, 0::8>>
      data = <<byte_size(payload)::24, 0::8, 0x8::8, 0::1, 1::31, payload::binary>>

      assert {{:ok, %Frame.Data{stream_id: 1, data: "hello"}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes empty DATA frame" do
      data = <<0::24, 0::8, 0::8, 0::1, 1::31>>

      assert {{:ok, %Frame.Data{stream_id: 1, data: <<>>}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "rejects DATA frame with stream_id 0" do
      # RFC 9113 §6.1: DATA frames MUST be associated with a stream
      data = <<3::24, 0::8, 0::8, 0::1, 0::31, "abc">>

      assert {{:error, error_code, "DATA frame with zero stream_id (RFC9113§6.1)"}, <<>>} =
               Frame.deserialize(data, 16_384)

      assert error_code == Errors.protocol_error()
    end

    test "handles large DATA frames" do
      large_data = :binary.copy(<<1>>, 10_000)
      data = <<10_000::24, 0::8, 0::8, 0::1, 1::31, large_data::binary>>

      assert {{:ok, %Frame.Data{data: ^large_data}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "rejects DATA frame with excessive padding" do
      # Padding length exceeds payload
      payload = <<10::8, "abc">>
      data = <<byte_size(payload)::24, 0::8, 0x8::8, 0::1, 1::31, payload::binary>>

      assert {{:error, _error_code, _reason}, <<>>} = Frame.deserialize(data, 16_384)
    end
  end

  describe "DATA frame serialization" do
    test "serializes basic DATA frame" do
      frame = %Frame.Data{stream_id: 123, end_stream: false, data: "hello"}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<5::24, 0::8, 0::8, 0::1, 123::31, "hello">> = binary
    end

    test "serializes DATA frame with END_STREAM flag" do
      frame = %Frame.Data{stream_id: 123, end_stream: true, data: "hello"}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<5::24, 0::8, 0x1::8, 0::1, 123::31, "hello">> = binary
    end

    test "serializes empty DATA frame" do
      frame = %Frame.Data{stream_id: 123, end_stream: false, data: <<>>}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<0::24, 0::8, 0::8, 0::1, 123::31>> = binary
    end

    test "splits DATA frame exceeding max_frame_size" do
      # 5 bytes of data, but max_frame_size is 2
      frame = %Frame.Data{stream_id: 123, end_stream: false, data: "hello"}

      result = Frame.serialize(frame, 2)

      assert [
               [<<2::24, 0::8, 0::8, 0::1, 123::31>>, "he"],
               [<<2::24, 0::8, 0::8, 0::1, 123::31>>, "ll"],
               [<<1::24, 0::8, 0::8, 0::1, 123::31>>, "o"]
             ] = result
    end

    test "sets END_STREAM only on last frame when splitting" do
      # Should split into 3 frames, END_STREAM only on last
      frame = %Frame.Data{stream_id: 123, end_stream: true, data: "hello"}

      result = Frame.serialize(frame, 2)

      # First two frames should not have END_STREAM
      [[<<2::24, 0::8, 0x0::8, _::binary>>, _], [<<2::24, 0::8, 0x0::8, _::binary>>, _], _last] =
        result

      # Last frame should have END_STREAM (0x1)
      [[<<1::24, 0::8, 0x1::8, 0::1, 123::31>>, "o"]] = [List.last(result)]
    end

    test "handles binary data (not just strings)" do
      binary_data = <<0, 1, 2, 255, 254, 253>>
      frame = %Frame.Data{stream_id: 123, end_stream: false, data: binary_data}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<6::24, 0::8, 0::8, 0::1, 123::31, ^binary_data::binary>> = binary
    end

    test "preserves iodata structure for efficiency" do
      frame = %Frame.Data{stream_id: 123, data: ["hello", " ", "world"]}

      result = Frame.serialize(frame, 16_384)

      assert is_list(result)
      assert IO.iodata_to_binary(result) =~ "hello world"
    end
  end

  describe "gRPC-specific scenarios" do
    test "handles gRPC message framing (5-byte length prefix)" do
      # gRPC message: compressed_flag(1) + length(4) + data
      grpc_msg = <<0::8, 5::32, "hello">>
      frame = %Frame.Data{stream_id: 1, end_stream: false, data: grpc_msg}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      <<_header::9-bytes, payload::binary>> = binary
      assert payload == grpc_msg
    end

    test "handles multiple gRPC messages in single DATA frame" do
      # Two gRPC messages back-to-back
      msg1 = <<0::8, 5::32, "hello">>
      msg2 = <<0::8, 5::32, "world">>
      combined = msg1 <> msg2

      frame = %Frame.Data{stream_id: 1, end_stream: false, data: combined}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      <<_header::9-bytes, payload::binary>> = binary
      assert payload == combined
    end

    test "handles compressed gRPC messages" do
      # Compressed message: flag=1
      compressed_msg = <<1::8, 100::32, :zlib.compress("large data")::binary>>
      frame = %Frame.Data{stream_id: 1, end_stream: false, data: compressed_msg}

      result = Frame.serialize(frame, 16_384)
      assert is_list(result)
    end
  end
end
