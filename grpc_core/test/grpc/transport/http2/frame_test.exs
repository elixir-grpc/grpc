defmodule GRPC.Transport.HTTP2.FrameTest do
  use ExUnit.Case, async: true

  alias GRPC.Transport.HTTP2.Frame
  alias GRPC.Transport.HTTP2.Errors

  describe "deserialize/2" do
    test "deserializes DATA frame (type 0x0)" do
      # Frame: length=3, type=0, flags=0, stream_id=1, payload="abc"
      data = <<3::24, 0::8, 0::8, 0::1, 1::31, "abc">>

      assert {{:ok, %Frame.Data{stream_id: 1, data: "abc"}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes HEADERS frame (type 0x1)" do
      # Frame: length=3, type=1, flags=4 (END_HEADERS), stream_id=1
      data = <<3::24, 1::8, 4::8, 0::1, 1::31, "hdr">>

      assert {{:ok, %Frame.Headers{stream_id: 1, end_headers: true, fragment: "hdr"}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes SETTINGS frame (type 0x4)" do
      # Frame: length=0, type=4, flags=0, stream_id=0 (empty SETTINGS)
      data = <<0::24, 4::8, 0::8, 0::1, 0::31>>

      assert {{:ok, %Frame.Settings{ack: false}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes PING frame (type 0x6)" do
      # Frame: length=8, type=6, flags=0, stream_id=0, payload=8 bytes
      payload = <<1, 2, 3, 4, 5, 6, 7, 8>>
      data = <<8::24, 6::8, 0::8, 0::1, 0::31, payload::binary>>

      assert {{:ok, %Frame.Ping{ack: false, payload: ^payload}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes GOAWAY frame (type 0x7)" do
      # Frame: length=8, type=7, flags=0, stream_id=0
      data = <<8::24, 7::8, 0::8, 0::1, 0::31, 0::1, 3::31, 0::32>>

      assert {{:ok, %Frame.Goaway{last_stream_id: 3, error_code: 0}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes WINDOW_UPDATE frame (type 0x8)" do
      # Frame: length=4, type=8, flags=0, stream_id=1
      data = <<4::24, 8::8, 0::8, 0::1, 1::31, 0::1, 1000::31>>

      assert {{:ok, %Frame.WindowUpdate{stream_id: 1, size_increment: 1000}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes unknown frame type" do
      # Frame: length=3, type=99 (unknown), flags=0, stream_id=1
      data = <<3::24, 99::8, 0::8, 0::1, 1::31, "xyz">>

      assert {{:ok, %Frame.Unknown{type: 99, stream_id: 1, payload: "xyz"}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "returns error when payload exceeds max_frame_size" do
      # Frame with length=17000 but max_frame_size=16384
      data = <<17000::24, 0::8, 0::8, 0::1, 1::31, "data">>

      assert {{:error, error_code, "Payload size too large (RFC9113§4.2)"}, _rest} =
               Frame.deserialize(data, 16_384)

      assert error_code == Errors.frame_size_error()
    end

    test "returns {:more, buffer} when frame is incomplete" do
      # Partial frame (only header, no payload)
      data = <<10::24, 0::8, 0::8, 0::1, 1::31>>

      assert {{:more, ^data}, <<>>} = Frame.deserialize(data, 16_384)
    end

    test "returns nil for empty buffer" do
      assert Frame.deserialize(<<>>, 16_384) == nil
    end

    test "handles multiple frames in buffer" do
      # Two DATA frames back-to-back
      frame1 = <<3::24, 0::8, 0::8, 0::1, 1::31, "abc">>
      frame2 = <<3::24, 0::8, 0::8, 0::1, 2::31, "def">>
      data = frame1 <> frame2

      assert {{:ok, %Frame.Data{stream_id: 1, data: "abc"}}, rest} =
               Frame.deserialize(data, 16_384)

      assert {{:ok, %Frame.Data{stream_id: 2, data: "def"}}, <<>>} =
               Frame.deserialize(rest, 16_384)
    end

    test "preserves remaining data after deserialization" do
      frame = <<3::24, 0::8, 0::8, 0::1, 1::31, "abc">>
      extra = <<1, 2, 3, 4, 5>>
      data = frame <> extra

      assert {{:ok, %Frame.Data{}}, ^extra} = Frame.deserialize(data, 16_384)
    end
  end

  describe "serialize/2" do
    test "serializes DATA frame" do
      frame = %Frame.Data{stream_id: 1, end_stream: false, data: "hello"}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      # Check frame header: length=5, type=0, flags=0, stream_id=1
      <<length::24, type::8, _flags::8, _reserved::1, stream_id::31, payload::binary>> = binary

      assert length == 5
      assert type == 0
      assert stream_id == 1
      assert payload == "hello"
    end

    test "serializes SETTINGS frame" do
      frame = %Frame.Settings{ack: false, settings: []}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      # Check frame header: length=0, type=4, stream_id=0
      <<length::24, type::8, _flags::8, _reserved::1, stream_id::31>> = binary

      assert length == 0
      assert type == 4
      assert stream_id == 0
    end

    test "serializes PING frame" do
      payload = <<1, 2, 3, 4, 5, 6, 7, 8>>
      frame = %Frame.Ping{ack: false, payload: payload}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      # Check frame header: length=8, type=6, stream_id=0
      <<length::24, type::8, _flags::8, _reserved::1, stream_id::31, data::binary>> = binary

      assert length == 8
      assert type == 6
      assert stream_id == 0
      assert data == payload
    end

    test "handles iodata efficiently" do
      # Frame.serialize should return iodata (list), not binary
      frame = %Frame.Data{stream_id: 1, data: "test"}

      result = Frame.serialize(frame, 16_384)

      # Result should be iodata (list)
      assert is_list(result)

      # But should convert to valid binary
      binary = IO.iodata_to_binary(result)
      assert is_binary(binary)
    end
  end

  describe "Frame.Flags" do
    test "set/1 returns 0 for empty list" do
      assert Frame.Flags.set([]) == 0x0
    end

    test "set/1 sets single bit" do
      assert Frame.Flags.set([0]) == 0b00000001
      assert Frame.Flags.set([1]) == 0b00000010
      assert Frame.Flags.set([2]) == 0b00000100
      assert Frame.Flags.set([7]) == 0b10000000
    end

    test "set/1 sets multiple bits" do
      assert Frame.Flags.set([0, 2]) == 0b00000101
      assert Frame.Flags.set([0, 1, 2, 3]) == 0b00001111
    end

    test "set?/2 guard works correctly" do
      require Frame.Flags

      # bits 0 and 2 set
      flags = 0b00000101

      assert Frame.Flags.set?(flags, 0)
      refute Frame.Flags.set?(flags, 1)
      assert Frame.Flags.set?(flags, 2)
      refute Frame.Flags.set?(flags, 3)
    end

    test "clear?/2 guard works correctly" do
      require Frame.Flags

      # bits 0 and 2 set
      flags = 0b00000101

      refute Frame.Flags.clear?(flags, 0)
      assert Frame.Flags.clear?(flags, 1)
      refute Frame.Flags.clear?(flags, 2)
      assert Frame.Flags.clear?(flags, 3)
    end
  end

  describe "RFC 9113 compliance" do
    test "default max_frame_size is 16,384 bytes" do
      # RFC 9113 §4.2
      max_frame_size = 16_384

      # Should accept frame at max size
      data = <<max_frame_size::24, 0::8, 0::8, 0::1, 1::31, 0::size(max_frame_size)-unit(8)>>
      assert {{:ok, _frame}, <<>>} = Frame.deserialize(data, max_frame_size)
    end

    test "reserved bit must be 0" do
      # RFC 9113 §4.1 - Reserved bit (R) must be unset
      # Frame with reserved bit = 0
      data = <<3::24, 0::8, 0::8, 0::1, 1::31, "abc">>
      assert {{:ok, _frame}, <<>>} = Frame.deserialize(data, 16_384)
    end

    test "frame types 0-9 are defined" do
      # RFC 9113 §6 - Frame types
      # DATA=0, HEADERS=1, PRIORITY=2, RST_STREAM=3, SETTINGS=4,
      # PUSH_PROMISE=5, PING=6, GOAWAY=7, WINDOW_UPDATE=8, CONTINUATION=9

      # Type 0 (DATA) requires stream_id > 0, so we test with stream_id=1
      data = <<0::24, 0::8, 0::8, 0::1, 1::31>>
      assert {{:ok, frame}, _} = Frame.deserialize(data, 16_384)
      refute match?(%Frame.Unknown{}, frame)

      # Types 1-9 can be tested with stream_id=0 (except some need valid payload)
      for type <- 1..9 do
        # Use appropriate payloads for each type
        {stream_id, payload} =
          case type do
            # HEADERS needs priority data
            1 -> {1, <<0, 0, 0, 0>>}
            # PRIORITY needs 5 bytes
            2 -> {1, <<0, 0, 0, 0, 0>>}
            # RST_STREAM needs 4 bytes
            3 -> {1, <<0, 0, 0, 0>>}
            # SETTINGS
            4 -> {0, <<>>}
            # PUSH_PROMISE needs promised stream id
            5 -> {1, <<0, 0, 0, 0>>}
            # PING needs 8 bytes
            6 -> {0, <<0, 0, 0, 0, 0, 0, 0, 0>>}
            # GOAWAY needs 8 bytes
            7 -> {0, <<0, 0, 0, 0, 0, 0, 0, 0>>}
            # WINDOW_UPDATE needs 4 bytes
            8 -> {1, <<0, 0, 0, 100>>}
            # CONTINUATION
            9 -> {1, <<>>}
          end

        length = byte_size(payload)
        data = <<length::24, type::8, 0::8, 0::1, stream_id::31, payload::binary>>
        result = Frame.deserialize(data, 16_384)

        # Should not return Unknown frame for types 0-9
        assert {{:ok, frame}, _} = result
        refute match?(%Frame.Unknown{}, frame), "Type #{type} returned Unknown frame"
      end
    end

    test "unknown frame types are ignored gracefully" do
      # RFC 9113 §4.1 - Implementations MUST ignore unknown frame types
      data = <<3::24, 255::8, 0::8, 0::1, 1::31, "xyz">>

      assert {{:ok, %Frame.Unknown{type: 255}}, <<>>} = Frame.deserialize(data, 16_384)
    end
  end
end
