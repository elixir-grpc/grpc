defmodule GRPC.Transport.HTTP2.Frame.PingTest do
  use ExUnit.Case, async: true

  alias GRPC.Transport.HTTP2.Frame
  alias GRPC.Transport.HTTP2.Errors

  describe "PING frame deserialization" do
    test "deserializes PING frame" do
      # PING with 8 bytes of opaque data
      payload = <<1, 2, 3, 4, 5, 6, 7, 8>>
      data = <<8::24, 6::8, 0x0::8, 0::1, 0::31, payload::binary>>

      assert {{:ok, %Frame.Ping{ack: false, payload: ^payload}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes PING ACK frame" do
      # PING with ACK flag (0x1)
      payload = <<1, 2, 3, 4, 5, 6, 7, 8>>
      data = <<8::24, 6::8, 0x1::8, 0::1, 0::31, payload::binary>>

      assert {{:ok, %Frame.Ping{ack: true, payload: ^payload}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "rejects PING frame with non-zero stream_id" do
      # RFC 9113 §6.7: PING frames MUST be associated with stream 0
      payload = <<1, 2, 3, 4, 5, 6, 7, 8>>
      data = <<8::24, 6::8, 0x0::8, 0::1, 1::31, payload::binary>>

      assert {{:error, error_code, _reason}, <<>>} = Frame.deserialize(data, 16_384)
      assert error_code == Errors.protocol_error()
    end

    test "rejects PING frame with incorrect length" do
      # RFC 9113 §6.7: PING frames MUST be exactly 8 bytes
      payload = <<1, 2, 3, 4>>
      data = <<4::24, 6::8, 0x0::8, 0::1, 0::31, payload::binary>>

      assert {{:error, error_code, _reason}, <<>>} = Frame.deserialize(data, 16_384)
      assert error_code == Errors.frame_size_error()
    end

    test "rejects PING frame with length too large" do
      # Must be exactly 8 bytes, not more
      payload = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>
      data = <<10::24, 6::8, 0x0::8, 0::1, 0::31, payload::binary>>

      assert {{:error, error_code, _reason}, <<>>} = Frame.deserialize(data, 16_384)
      assert error_code == Errors.frame_size_error()
    end

    test "handles PING with all zeros" do
      payload = <<0, 0, 0, 0, 0, 0, 0, 0>>
      data = <<8::24, 6::8, 0x0::8, 0::1, 0::31, payload::binary>>

      assert {{:ok, %Frame.Ping{ack: false, payload: ^payload}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "handles PING with all ones" do
      payload = <<255, 255, 255, 255, 255, 255, 255, 255>>
      data = <<8::24, 6::8, 0x0::8, 0::1, 0::31, payload::binary>>

      assert {{:ok, %Frame.Ping{ack: false, payload: ^payload}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end
  end

  describe "PING frame serialization" do
    test "serializes PING frame" do
      payload = <<1, 2, 3, 4, 5, 6, 7, 8>>
      frame = %Frame.Ping{ack: false, payload: payload}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<8::24, 6::8, 0x0::8, 0::1, 0::31, ^payload::binary>> = binary
    end

    test "serializes PING ACK frame" do
      payload = <<1, 2, 3, 4, 5, 6, 7, 8>>
      frame = %Frame.Ping{ack: true, payload: payload}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<8::24, 6::8, 0x1::8, 0::1, 0::31, ^payload::binary>> = binary
    end

    test "preserves payload exactly" do
      # Ensure payload is not modified
      payload = <<0xDE, 0xAD, 0xBE, 0xEF, 0xCA, 0xFE, 0xBA, 0xBE>>
      frame = %Frame.Ping{ack: false, payload: payload}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      <<_::9-bytes, received_payload::8-bytes>> = binary
      assert received_payload == payload
    end
  end

  describe "PING round-trip" do
    test "PING request and ACK response match payload" do
      # Client sends PING
      client_payload = <<1, 2, 3, 4, 5, 6, 7, 8>>
      ping = %Frame.Ping{ack: false, payload: client_payload}

      # Server responds with PING ACK containing same payload
      pong = %Frame.Ping{ack: true, payload: client_payload}

      ping_binary = IO.iodata_to_binary(Frame.serialize(ping, 16_384))
      pong_binary = IO.iodata_to_binary(Frame.serialize(pong, 16_384))

      # Deserialize both
      {{:ok, ping_frame}, <<>>} = Frame.deserialize(ping_binary, 16_384)
      {{:ok, pong_frame}, <<>>} = Frame.deserialize(pong_binary, 16_384)

      # Verify payload matches
      assert IO.iodata_to_binary(ping_frame.payload) == IO.iodata_to_binary(pong_frame.payload)
      assert ping_frame.ack == false
      assert pong_frame.ack == true
    end
  end

  describe "gRPC-specific scenarios" do
    test "handles keepalive PING" do
      # gRPC uses PING for connection keepalive
      # Typically uses timestamp or counter as payload
      timestamp = System.system_time(:millisecond)
      payload = <<timestamp::64>>

      ping = %Frame.Ping{ack: false, payload: payload}
      pong = %Frame.Ping{ack: true, payload: payload}

      ping_frame = Frame.serialize(ping, 16_384)
      pong_frame = Frame.serialize(pong, 16_384)

      assert is_list(ping_frame)
      assert is_list(pong_frame)
    end

    test "handles latency measurement" do
      # Can use PING to measure RTT
      # Use a positive timestamp value
      timestamp = System.system_time(:millisecond)
      payload = <<timestamp::signed-64>>

      # Send PING
      ping = %Frame.Ping{ack: false, payload: payload}
      _ping_binary = IO.iodata_to_binary(Frame.serialize(ping, 16_384))

      # Receive PING ACK
      {{:ok, pong}, <<>>} =
        Frame.deserialize(<<8::24, 6::8, 0x1::8, 0::1, 0::31, payload::binary>>, 16_384)

      # Calculate RTT (in real scenario, would have network delay)
      <<received_time::signed-64>> = IO.iodata_to_binary(pong.payload)
      assert received_time == timestamp
    end

    test "handles connection health check" do
      # gRPC clients periodically send PING to check connection
      health_check = %Frame.Ping{
        ack: false,
        payload: <<"HEALTH", 0::16>>
      }

      result = Frame.serialize(health_check, 16_384)
      binary = IO.iodata_to_binary(result)

      {{:ok, received}, <<>>} = Frame.deserialize(binary, 16_384)
      assert IO.iodata_to_binary(received.payload) == <<"HEALTH", 0::16>>
    end

    test "handles PING flood protection scenario" do
      # In production, need to rate-limit PING frames
      # Test multiple PINGs with different payload
      pings =
        for i <- 1..10 do
          %Frame.Ping{ack: false, payload: <<i::64>>}
        end

      serialized = Enum.map(pings, &Frame.serialize(&1, 16_384))

      assert length(serialized) == 10
      assert Enum.all?(serialized, &is_list/1)
    end

    test "handles PING timeout scenario" do
      # Send PING, simulate no response (timeout detection)
      ping = %Frame.Ping{
        ack: false,
        payload: <<System.system_time(:second)::64>>
      }

      ping_binary = IO.iodata_to_binary(Frame.serialize(ping, 16_384))

      # In real implementation, would start timer and close connection if no ACK
      assert byte_size(ping_binary) == 17
    end
  end
end
