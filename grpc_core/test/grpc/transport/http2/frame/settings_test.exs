defmodule GRPC.Transport.HTTP2.Frame.SettingsTest do
  use ExUnit.Case, async: true

  alias GRPC.Transport.HTTP2.Frame

  describe "SETTINGS frame deserialization" do
    test "deserializes empty SETTINGS frame" do
      # Empty SETTINGS frame (connection preface)
      data = <<0::24, 4::8, 0x0::8, 0::1, 0::31>>

      assert {{:ok, %Frame.Settings{ack: false, settings: settings}}, <<>>} =
               Frame.deserialize(data, 16_384)

      assert settings == %{}
    end

    test "deserializes SETTINGS ACK frame" do
      # SETTINGS frame with ACK flag (0x1)
      data = <<0::24, 4::8, 0x1::8, 0::1, 0::31>>

      assert {{:ok, %Frame.Settings{ack: true, settings: nil}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes SETTINGS frame with max_concurrent_streams" do
      # SETTINGS_MAX_CONCURRENT_STREAMS (0x3) = 100
      settings_payload = <<0x3::16, 100::32>>

      data =
        <<byte_size(settings_payload)::24, 4::8, 0x0::8, 0::1, 0::31, settings_payload::binary>>

      assert {{:ok, %Frame.Settings{settings: %{max_concurrent_streams: 100}}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes SETTINGS frame with multiple settings" do
      settings_payload = <<
        0x1::16,
        8192::32,
        # SETTINGS_HEADER_TABLE_SIZE
        0x3::16,
        100::32,
        # SETTINGS_MAX_CONCURRENT_STREAMS
        0x4::16,
        32768::32
        # SETTINGS_INITIAL_WINDOW_SIZE
      >>

      data =
        <<byte_size(settings_payload)::24, 4::8, 0x0::8, 0::1, 0::31, settings_payload::binary>>

      assert {{:ok,
               %Frame.Settings{
                 settings: %{
                   header_table_size: 8192,
                   max_concurrent_streams: 100,
                   initial_window_size: 32768
                 }
               }}, <<>>} = Frame.deserialize(data, 16_384)
    end
  end

  describe "SETTINGS frame serialization" do
    test "serializes empty SETTINGS frame" do
      frame = %Frame.Settings{ack: false, settings: %{}}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<0::24, 4::8, 0x0::8, 0::1, 0::31>> = binary
    end

    test "serializes SETTINGS ACK frame" do
      frame = %Frame.Settings{ack: true, settings: nil}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<0::24, 4::8, 0x1::8, 0::1, 0::31>> = binary
    end

    test "serializes SETTINGS frame with max_concurrent_streams" do
      frame = %Frame.Settings{ack: false, settings: %{max_concurrent_streams: 100}}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<6::24, 4::8, 0x0::8, 0::1, 0::31, 0x3::16, 100::32>> = binary
    end

    test "serializes SETTINGS frame omitting default values" do
      # Default values should be omitted
      frame = %Frame.Settings{
        ack: false,
        settings: %{
          header_table_size: 4096,
          # default
          max_concurrent_streams: 200,
          # non-default
          initial_window_size: 65535,
          # default
          max_frame_size: 16384
          # default
        }
      }

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      # Should only include max_concurrent_streams (non-default)
      assert <<6::24, 4::8, 0x0::8, 0::1, 0::31, 0x3::16, 200::32>> = binary
    end
  end

  describe "gRPC-specific scenarios" do
    test "handles gRPC recommended settings" do
      # gRPC typically uses specific settings
      frame = %Frame.Settings{
        settings: %{
          header_table_size: 8192,
          max_concurrent_streams: 100,
          initial_window_size: 1_048_576,
          max_frame_size: 16384
        }
      }

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      # Verify it's a valid SETTINGS frame (header_table_size, max_concurrent_streams, initial_window_size)
      # max_frame_size with default value is omitted
      <<length::24, 4::8, 0x0::8, 0::1, 0::31, _payload::binary>> = binary
      assert length == 18
    end

    test "handles connection preface SETTINGS" do
      # Client/server exchange SETTINGS during connection preface
      settings = %Frame.Settings{
        settings: %{
          max_concurrent_streams: 100,
          initial_window_size: 65535
        }
      }

      result = Frame.serialize(settings, 16_384)
      assert is_list(result)
    end

    test "handles SETTINGS ACK response" do
      # After receiving SETTINGS, peer must send SETTINGS ACK
      ack = %Frame.Settings{ack: true, settings: nil}

      result = Frame.serialize(ack, 16_384)
      binary = IO.iodata_to_binary(result)

      # Empty payload with ACK flag
      assert <<0::24, 4::8, 0x1::8, 0::1, 0::31>> = binary
    end

    test "handles window size updates via SETTINGS" do
      # Changing SETTINGS_INITIAL_WINDOW_SIZE affects flow control
      frame = %Frame.Settings{
        settings: %{
          initial_window_size: 1_048_576
        }
      }

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      <<6::24, 4::8, 0x0::8, 0::1, 0::31, 0x4::16, 1_048_576::32>> = binary
    end

    test "handles unlimited max_concurrent_streams" do
      # :infinity means no limit on concurrent streams
      frame = %Frame.Settings{
        settings: %{
          max_concurrent_streams: :infinity
        }
      }

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      # :infinity is omitted (no setting sent)
      assert <<0::24, 4::8, 0x0::8, 0::1, 0::31>> = binary
    end
  end
end
