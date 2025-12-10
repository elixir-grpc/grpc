defmodule GRPC.Transport.HTTP2.Frame.WindowUpdate do
  @moduledoc false

  import Bitwise

  defstruct stream_id: nil, size_increment: nil

  @typedoc "An HTTP/2 WINDOW_UPDATE frame"
  @type t :: %__MODULE__{
          stream_id: GRPC.Transport.HTTP2.Stream.stream_id(),
          size_increment: non_neg_integer()
        }

  @max_window_increment (1 <<< 31) - 1

  @spec deserialize(
          GRPC.Transport.HTTP2.Frame.flags(),
          GRPC.Transport.HTTP2.Stream.stream_id(),
          iodata()
        ) ::
          {:ok, t()} | {:error, GRPC.Transport.HTTP2.Errors.error_code(), binary()}
  def deserialize(_flags, stream_id, <<_reserved::1, size_increment::31>>)
      when size_increment > 0 and size_increment <= @max_window_increment do
    {:ok, %__MODULE__{stream_id: stream_id, size_increment: size_increment}}
  end

  def deserialize(_flags, _stream_id, _payload) do
    {:error, GRPC.Transport.HTTP2.Errors.frame_size_error(),
     "Invalid WINDOW_UPDATE frame (RFC9113§6.9)"}
  end

  defimpl GRPC.Transport.HTTP2.Frame.Serializable do
    def serialize(%GRPC.Transport.HTTP2.Frame.WindowUpdate{} = frame, _max_frame_size) do
      [{0x8, 0x0, frame.stream_id, <<0::1, frame.size_increment::31>>}]
    end
  end
end
