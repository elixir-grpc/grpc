defmodule GRPC.Transport.HTTP2.Frame.WindowUpdateTest do
  use ExUnit.Case, async: true
  import Bitwise

  alias GRPC.Transport.HTTP2.Frame
  alias GRPC.Transport.HTTP2.Errors

  describe "WINDOW_UPDATE frame deserialization" do
    test "deserializes WINDOW_UPDATE for stream" do
      # WINDOW_UPDATE for stream 123, increment 1000
      data = <<4::24, 8::8, 0x0::8, 0::1, 123::31, 0::1, 1000::31>>

      assert {{:ok, %Frame.WindowUpdate{stream_id: 123, size_increment: 1000}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes WINDOW_UPDATE for connection (stream 0)" do
      # WINDOW_UPDATE for connection-level flow control
      data = <<4::24, 8::8, 0x0::8, 0::1, 0::31, 0::1, 65535::31>>

      assert {{:ok, %Frame.WindowUpdate{stream_id: 0, size_increment: 65535}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "deserializes WINDOW_UPDATE with maximum increment" do
      # Maximum increment: 2^31 - 1
      max_increment = (1 <<< 31) - 1
      data = <<4::24, 8::8, 0x0::8, 0::1, 1::31, 0::1, max_increment::31>>

      assert {{:ok, %Frame.WindowUpdate{stream_id: 1, size_increment: ^max_increment}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end

    test "rejects WINDOW_UPDATE with zero increment" do
      # RFC 9113 §6.9: A receiver MUST treat increment of 0 as error
      data = <<4::24, 8::8, 0x0::8, 0::1, 123::31, 0::1, 0::31>>

      assert {{:error, error_code, _reason}, <<>>} = Frame.deserialize(data, 16_384)
      assert error_code == Errors.frame_size_error()
    end

    test "rejects WINDOW_UPDATE with incorrect length" do
      # RFC 9113 §6.9: WINDOW_UPDATE frames MUST be 4 bytes
      data = <<2::24, 8::8, 0x0::8, 0::1, 123::31, 100::16>>

      assert {{:error, error_code, _reason}, <<>>} = Frame.deserialize(data, 16_384)
      assert error_code == Errors.frame_size_error()
    end

    test "handles reserved bit correctly" do
      # Reserved bit (first bit) should be ignored
      data = <<4::24, 8::8, 0x0::8, 0::1, 123::31, 1::1, 1000::31>>

      assert {{:ok, %Frame.WindowUpdate{stream_id: 123, size_increment: 1000}}, <<>>} =
               Frame.deserialize(data, 16_384)
    end
  end

  describe "WINDOW_UPDATE frame serialization" do
    test "serializes WINDOW_UPDATE for stream" do
      frame = %Frame.WindowUpdate{stream_id: 123, size_increment: 1000}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<4::24, 8::8, 0x0::8, 0::1, 123::31, 0::1, 1000::31>> = binary
    end

    test "serializes WINDOW_UPDATE for connection" do
      frame = %Frame.WindowUpdate{stream_id: 0, size_increment: 65535}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<4::24, 8::8, 0x0::8, 0::1, 0::31, 0::1, 65535::31>> = binary
    end

    test "serializes WINDOW_UPDATE with maximum increment" do
      max_increment = (1 <<< 31) - 1
      frame = %Frame.WindowUpdate{stream_id: 1, size_increment: max_increment}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<4::24, 8::8, 0x0::8, 0::1, 1::31, 0::1, ^max_increment::31>> = binary
    end

    test "sets reserved bit to 0" do
      frame = %Frame.WindowUpdate{stream_id: 123, size_increment: 1000}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      # Extract window size increment field and check reserved bit
      <<_::9-bytes, reserved::1, increment::31>> = binary
      assert reserved == 0
      assert increment == 1000
    end
  end

  describe "gRPC-specific scenarios" do
    test "handles stream-level window update after consuming data" do
      # After processing DATA frame, update stream window
      consumed = 8192
      window_update = %Frame.WindowUpdate{stream_id: 1, size_increment: consumed}

      result = Frame.serialize(window_update, 16_384)
      binary = IO.iodata_to_binary(result)

      {{:ok, received}, <<>>} = Frame.deserialize(binary, 16_384)
      assert received.size_increment == consumed
    end

    test "handles connection-level window update" do
      # Update connection-level window after processing multiple streams
      connection_update = %Frame.WindowUpdate{
        stream_id: 0,
        size_increment: 1_048_576
      }

      result = Frame.serialize(connection_update, 16_384)
      binary = IO.iodata_to_binary(result)

      {{:ok, received}, <<>>} = Frame.deserialize(binary, 16_384)
      assert received.stream_id == 0
      assert received.size_increment == 1_048_576
    end

    test "handles window exhaustion prevention" do
      # Send window update before window is fully exhausted
      # Keep at least 50% window available
      initial_window = 65535
      consumed = div(initial_window, 2)

      update = %Frame.WindowUpdate{stream_id: 1, size_increment: consumed}

      result = Frame.serialize(update, 16_384)
      assert is_list(result)
    end

    test "handles large message flow control" do
      # For large gRPC messages, need multiple window updates
      chunk_size = 16384

      updates =
        for _i <- 1..10 do
          %Frame.WindowUpdate{stream_id: 1, size_increment: chunk_size}
        end

      serialized = Enum.map(updates, &Frame.serialize(&1, 16_384))

      assert length(serialized) == 10
      assert Enum.all?(serialized, &is_list/1)
    end

    test "handles window update for streaming RPCs" do
      # Bidirectional streaming needs careful window management
      # Client updates window as it consumes server data
      client_update = %Frame.WindowUpdate{stream_id: 1, size_increment: 32768}

      # Server updates window as it consumes client data
      server_update = %Frame.WindowUpdate{stream_id: 1, size_increment: 32768}

      client_binary = IO.iodata_to_binary(Frame.serialize(client_update, 16_384))
      server_binary = IO.iodata_to_binary(Frame.serialize(server_update, 16_384))

      assert client_binary == server_binary
    end

    test "handles window overflow detection" do
      # Receiving window update that would overflow window size
      # Current window: 65535, increment: max_value would exceed 2^31-1
      # Implementation should detect this as flow control error

      # Send legitimate max increment
      max_increment = (1 <<< 31) - 1
      frame = %Frame.WindowUpdate{stream_id: 1, size_increment: max_increment}

      result = Frame.serialize(frame, 16_384)
      binary = IO.iodata_to_binary(result)

      {{:ok, received}, <<>>} = Frame.deserialize(binary, 16_384)
      assert received.size_increment == max_increment
    end

    test "handles immediate window update strategy" do
      # Eagerly update window after every DATA frame
      data_frame_size = 8192

      update = %Frame.WindowUpdate{
        stream_id: 1,
        size_increment: data_frame_size
      }

      result = Frame.serialize(update, 16_384)
      assert is_list(result)
    end

    test "handles batched window updates" do
      # Buffer multiple DATA frames, then send one window update
      total_consumed = 8192 + 8192 + 4096

      update = %Frame.WindowUpdate{
        stream_id: 1,
        size_increment: total_consumed
      }

      result = Frame.serialize(update, 16_384)
      binary = IO.iodata_to_binary(result)

      {{:ok, received}, <<>>} = Frame.deserialize(binary, 16_384)
      assert received.size_increment == total_consumed
    end

    test "handles connection vs stream window priority" do
      # Both connection and stream windows must be available
      # Update both when consuming data

      connection_update = %Frame.WindowUpdate{stream_id: 0, size_increment: 16384}
      stream_update = %Frame.WindowUpdate{stream_id: 1, size_increment: 16384}

      conn_binary = IO.iodata_to_binary(Frame.serialize(connection_update, 16_384))
      stream_binary = IO.iodata_to_binary(Frame.serialize(stream_update, 16_384))

      {{:ok, conn}, <<>>} = Frame.deserialize(conn_binary, 16_384)
      {{:ok, stream}, <<>>} = Frame.deserialize(stream_binary, 16_384)

      assert conn.stream_id == 0
      assert stream.stream_id == 1
      assert conn.size_increment == stream.size_increment
    end

    test "handles window update timing for backpressure" do
      # Delay window update to apply backpressure on sender
      # Send smaller increments to slow down data flow
      throttled_increment = 4096

      update = %Frame.WindowUpdate{
        stream_id: 1,
        size_increment: throttled_increment
      }

      result = Frame.serialize(update, 16_384)
      binary = IO.iodata_to_binary(result)

      assert <<4::24, 8::8, 0x0::8, 0::1, 1::31, 0::1, ^throttled_increment::31>> = binary
    end
  end

  describe "flow control scenarios" do
    test "handles window depletion and replenishment" do
      # Start with initial window: 65535
      # Consume data in chunks, replenish periodically

      chunks = [16384, 16384, 16384, 16383]

      updates =
        for {chunk, idx} <- Enum.with_index(chunks, 1) do
          %Frame.WindowUpdate{stream_id: idx, size_increment: chunk}
        end

      total_increment = Enum.sum(chunks)
      assert total_increment == 65535

      serialized = Enum.map(updates, &Frame.serialize(&1, 16_384))
      assert length(serialized) == 4
    end

    test "handles concurrent stream window updates" do
      # Multiple active streams, each with independent windows
      stream_updates =
        for stream_id <- 1..10 do
          %Frame.WindowUpdate{stream_id: stream_id, size_increment: 8192}
        end

      # Plus connection-level update
      connection_update = %Frame.WindowUpdate{stream_id: 0, size_increment: 81920}

      all_updates = [connection_update | stream_updates]
      serialized = Enum.map(all_updates, &Frame.serialize(&1, 16_384))

      assert length(serialized) == 11
    end
  end
end
