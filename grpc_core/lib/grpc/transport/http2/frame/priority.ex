defmodule GRPC.Transport.HTTP2.Frame.Priority do
  @moduledoc false

  defstruct stream_id: nil,
            exclusive_dependency: false,
            stream_dependency: nil,
            weight: nil

  @typedoc "An HTTP/2 PRIORITY frame"
  @type t :: %__MODULE__{
          stream_id: GRPC.Transport.HTTP2.Stream.stream_id(),
          exclusive_dependency: boolean(),
          stream_dependency: GRPC.Transport.HTTP2.Stream.stream_id(),
          weight: non_neg_integer()
        }

  @spec deserialize(GRPC.Transport.HTTP2.Frame.flags(), GRPC.Transport.HTTP2.Stream.stream_id(), iodata()) ::
          {:ok, t()} | {:error, GRPC.Transport.HTTP2.Errors.error_code(), binary()}
  def deserialize(_flags, 0, _payload) do
    {:error, GRPC.Transport.HTTP2.Errors.protocol_error(),
     "PRIORITY frame with zero stream_id (RFC9113§6.3)"}
  end

  def deserialize(
        _flags,
        stream_id,
        <<exclusive_dependency::1, stream_dependency::31, weight::8>>
      ) do
    {:ok,
     %__MODULE__{
       stream_id: stream_id,
       exclusive_dependency: exclusive_dependency == 0x01,
       stream_dependency: stream_dependency,
       weight: weight
     }}
  end

  def deserialize(_flags, _stream_id, _payload) do
    {:error, GRPC.Transport.HTTP2.Errors.frame_size_error(),
     "Invalid payload size in PRIORITY frame (RFC9113§6.3)"}
  end

  defimpl GRPC.Transport.HTTP2.Frame.Serializable do
    def serialize(%GRPC.Transport.HTTP2.Frame.Priority{} = frame, _max_frame_size) do
      exclusive = if frame.exclusive_dependency, do: 0x01, else: 0x00
      payload = <<exclusive::1, frame.stream_dependency::31, frame.weight::8>>
      [{0x2, 0x0, frame.stream_id, payload}]
    end
  end
end
