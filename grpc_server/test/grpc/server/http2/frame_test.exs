defmodule GRPC.Transport.HTTP2.FrameTest do
  use ExUnit.Case, async: true

  alias GRPC.Transport.HTTP2.{Frame, Errors}

  describe "frame deserialization" do
    test "deserializes DATA frames" do
      # DATA frame: stream_id=1, no padding, end_stream=false
      frame = <<0, 0, 3, 0, 0, 0, 0, 0, 1, 1, 2, 3>>

      assert {{:ok, %Frame.Data{stream_id: 1, end_stream: false, data: <<1, 2, 3>>}}, <<>>} =
               Frame.deserialize(frame, 16_384)
    end

    test "deserializes HEADERS frames" do
      # HEADERS frame: stream_id=1, end_stream=false, end_headers=false
      frame = <<0, 0, 3, 1, 0x00, 0, 0, 0, 1, 1, 2, 3>>

      assert {{:ok,
               %Frame.Headers{
                 stream_id: 1,
                 end_stream: false,
                 end_headers: false,
                 fragment: <<1, 2, 3>>
               }}, <<>>} = Frame.deserialize(frame, 16_384)
    end

    test "deserializes SETTINGS frames" do
      # SETTINGS frame: max_frame_size=32768
      frame = <<0, 0, 6, 4, 0, 0, 0, 0, 0, 0, 5, 0, 0, 128, 0>>

      assert {{:ok, %Frame.Settings{ack: false, settings: %{max_frame_size: 32_768}}}, <<>>} =
               Frame.deserialize(frame, 16_384)
    end

    test "deserializes SETTINGS ACK frames" do
      frame = <<0, 0, 0, 4, 1, 0, 0, 0, 0>>

      assert {{:ok, %Frame.Settings{ack: true}}, <<>>} = Frame.deserialize(frame, 16_384)
    end

    test "deserializes PING frames" do
      frame = <<0, 0, 8, 6, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8>>

      assert {{:ok, %Frame.Ping{ack: false, payload: <<1, 2, 3, 4, 5, 6, 7, 8>>}}, <<>>} =
               Frame.deserialize(frame, 16_384)
    end

    test "deserializes PING ACK frames" do
      frame = <<0, 0, 8, 6, 1, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8>>

      assert {{:ok, %Frame.Ping{ack: true, payload: <<1, 2, 3, 4, 5, 6, 7, 8>>}}, <<>>} =
               Frame.deserialize(frame, 16_384)
    end

    test "deserializes GOAWAY frames" do
      frame = <<0, 0, 8, 7, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2>>

      assert {{:ok, %Frame.Goaway{last_stream_id: 1, error_code: 2, debug_data: <<>>}}, <<>>} =
               Frame.deserialize(frame, 16_384)
    end

    test "deserializes RST_STREAM frames" do
      frame = <<0, 0, 4, 3, 0, 0, 0, 0, 1, 0, 0, 0, 8>>

      assert {{:ok, %Frame.RstStream{stream_id: 1, error_code: 8}}, <<>>} =
               Frame.deserialize(frame, 16_384)
    end

    test "deserializes WINDOW_UPDATE frames" do
      frame = <<0, 0, 4, 8, 0, 0, 0, 0, 0, 0, 0, 0, 100>>

      assert {{:ok, %Frame.WindowUpdate{stream_id: 0, size_increment: 100}}, <<>>} =
               Frame.deserialize(frame, 16_384)
    end

    test "deserializes CONTINUATION frames" do
      frame = <<0, 0, 3, 9, 0x00, 0, 0, 0, 1, 1, 2, 3>>

      assert {{:ok,
               %Frame.Continuation{
                 stream_id: 1,
                 end_headers: false,
                 fragment: <<1, 2, 3>>
               }}, <<>>} = Frame.deserialize(frame, 16_384)
    end

    test "deserializes PRIORITY frames" do
      frame = <<0, 0, 5, 2, 0, 0, 0, 0, 1, 0::1, 12::31, 34>>

      assert {{:ok,
               %Frame.Priority{
                 stream_id: 1,
                 exclusive_dependency: false,
                 stream_dependency: 12,
                 weight: 34
               }}, <<>>} = Frame.deserialize(frame, 16_384)
    end

    test "deserializes unknown frame types" do
      # Unknown type 0xFF
      frame = <<0, 0, 3, 0xFF, 0, 0, 0, 0, 1, 1, 2, 3>>

      assert {{:ok,
               %Frame.Unknown{
                 type: 0xFF,
                 flags: 0,
                 stream_id: 1,
                 payload: <<1, 2, 3>>
               }}, <<>>} = Frame.deserialize(frame, 16_384)
    end

    test "returns extra data when frame is followed by more data" do
      frame = <<0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3>>

      assert {{:ok, %Frame.Data{stream_id: 1, data: <<>>}}, <<1, 2, 3>>} =
               Frame.deserialize(frame, 16_384)
    end

    test "asks for more data when frame is incomplete" do
      frame = <<0, 0, 10, 0, 0, 0, 0, 0, 1>>

      assert {{:more, <<0, 0, 10, 0, 0, 0, 0, 0, 1>>}, <<>>} =
               Frame.deserialize(frame, 16_384)
    end

    test "returns nil when buffer is empty" do
      assert Frame.deserialize(<<>>, 16_384) == nil
    end

    test "rejects frames that exceed max_frame_size" do
      # Frame with length 100, max_frame_size 50
      frame = <<0, 0, 100, 0, 0, 0, 0, 0, 1>> <> :binary.copy(<<0>>, 100)

      assert {{:error, error_code, "Payload size too large (RFC9113§4.2)"}, _rest} =
               Frame.deserialize(frame, 50)

      assert error_code == Errors.frame_size_error()
    end
  end

  describe "frame serialization" do
    test "serializes DATA frames" do
      frame = %Frame.Data{
        stream_id: 123,
        end_stream: false,
        data: <<1, 2, 3>>
      }

      assert Frame.serialize(frame, 16_384) == [
               [<<0, 0, 3, 0, 0, 0, 0, 0, 123>>, <<1, 2, 3>>]
             ]
    end

    test "serializes DATA frames with end_stream set" do
      frame = %Frame.Data{
        stream_id: 123,
        end_stream: true,
        data: <<1, 2, 3>>
      }

      assert Frame.serialize(frame, 16_384) == [
               [<<0, 0, 3, 0, 1, 0, 0, 0, 123>>, <<1, 2, 3>>]
             ]
    end

    test "serializes HEADERS frames" do
      frame = %Frame.Headers{
        stream_id: 123,
        end_stream: false,
        fragment: <<1, 2, 3>>
      }

      assert Frame.serialize(frame, 16_384) == [
               [<<0, 0, 3, 1, 4, 0, 0, 0, 123>>, <<1, 2, 3>>]
             ]
    end

    test "serializes SETTINGS frames" do
      frame = %Frame.Settings{
        ack: false,
        settings: %{
          header_table_size: 8_192,
          max_frame_size: 32_768
        }
      }

      result = Frame.serialize(frame, 16_384)
      assert [[header, payload]] = result
      assert <<0, 0, 12, 4, 0, 0, 0, 0, 0>> = header
      # Payload should contain both settings
      payload_binary = IO.iodata_to_binary(payload)
      assert byte_size(payload_binary) == 12
    end

    test "serializes SETTINGS ACK frames" do
      frame = %Frame.Settings{ack: true, settings: %{}}

      assert Frame.serialize(frame, 16_384) == [[<<0, 0, 0, 4, 1, 0, 0, 0, 0>>, <<>>]]
    end

    test "serializes PING frames" do
      frame = %Frame.Ping{ack: false, payload: <<1, 2, 3, 4, 5, 6, 7, 8>>}

      assert Frame.serialize(frame, 16_384) == [
               [<<0, 0, 8, 6, 0, 0, 0, 0, 0>>, <<1, 2, 3, 4, 5, 6, 7, 8>>]
             ]
    end

    test "serializes GOAWAY frames" do
      frame = %Frame.Goaway{last_stream_id: 1, error_code: 2, debug_data: <<>>}

      assert Frame.serialize(frame, 16_384) == [
               [<<0, 0, 8, 7, 0, 0, 0, 0, 0>>, <<0, 0, 0, 1, 0, 0, 0, 2>>]
             ]
    end

    test "serializes RST_STREAM frames" do
      frame = %Frame.RstStream{stream_id: 1, error_code: 8}

      assert Frame.serialize(frame, 16_384) == [
               [<<0, 0, 4, 3, 0, 0, 0, 0, 1>>, <<0, 0, 0, 8>>]
             ]
    end

    test "serializes WINDOW_UPDATE frames" do
      frame = %Frame.WindowUpdate{stream_id: 123, size_increment: 234}

      assert Frame.serialize(frame, 16_384) == [
               [<<0, 0, 4, 8, 0, 0, 0, 0, 123>>, <<0, 0, 0, 234>>]
             ]
    end

    test "splits DATA frames that exceed max_frame_size" do
      frame = %Frame.Data{
        stream_id: 123,
        end_stream: false,
        data: <<1, 2, 3>>
      }

      assert Frame.serialize(frame, 2) == [
               [<<0, 0, 2, 0, 0, 0, 0, 0, 123>>, <<1, 2>>],
               [<<0, 0, 1, 0, 0, 0, 0, 0, 123>>, <<3>>]
             ]
    end

    test "splits HEADERS frames into HEADERS + CONTINUATION" do
      frame = %Frame.Headers{
        stream_id: 123,
        end_stream: false,
        fragment: <<1, 2, 3>>
      }

      assert Frame.serialize(frame, 2) == [
               [<<0, 0, 2, 1, 0, 0, 0, 0, 123>>, <<1, 2>>],
               [<<0, 0, 1, 9, 4, 0, 0, 0, 123>>, <<3>>]
             ]
    end
  end
end
