defmodule GRPC.Transport.HTTP2.Frame.Ping do
  @moduledoc false

  import GRPC.Transport.HTTP2.Frame.Flags

  defstruct ack: false, payload: nil

  @typedoc "An HTTP/2 PING frame"
  @type t :: %__MODULE__{
          ack: boolean(),
          payload: iodata()
        }

  @ack_bit 0

  @spec deserialize(
          GRPC.Transport.HTTP2.Frame.flags(),
          GRPC.Transport.HTTP2.Stream.stream_id(),
          iodata()
        ) ::
          {:ok, t()} | {:error, GRPC.Transport.HTTP2.Errors.error_code(), binary()}
  def deserialize(flags, 0, <<payload::binary-size(8)>>) when set?(flags, @ack_bit) do
    {:ok, %__MODULE__{ack: true, payload: payload}}
  end

  def deserialize(flags, 0, <<payload::binary-size(8)>>) when not set?(flags, @ack_bit) do
    {:ok, %__MODULE__{ack: false, payload: payload}}
  end

  def deserialize(_flags, stream_id, _payload) when stream_id != 0 do
    {:error, GRPC.Transport.HTTP2.Errors.protocol_error(),
     "Invalid stream ID in PING frame (RFC9113§6.7)"}
  end

  def deserialize(_flags, _stream_id, _payload) do
    {:error, GRPC.Transport.HTTP2.Errors.frame_size_error(),
     "PING frame with invalid payload size (RFC9113§6.7)"}
  end

  defimpl GRPC.Transport.HTTP2.Frame.Serializable do
    @ack_bit 0

    def serialize(%GRPC.Transport.HTTP2.Frame.Ping{ack: true} = frame, _max_frame_size),
      do: [{6, set([@ack_bit]), 0, frame.payload}]

    def serialize(%GRPC.Transport.HTTP2.Frame.Ping{ack: false} = frame, _max_frame_size),
      do: [{6, 0, 0, frame.payload}]
  end
end
