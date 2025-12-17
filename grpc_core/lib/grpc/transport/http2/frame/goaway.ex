defmodule GRPC.Transport.HTTP2.Frame.Goaway do
  @moduledoc false

  defstruct last_stream_id: 0, error_code: 0, debug_data: <<>>

  @typedoc "An HTTP/2 GOAWAY frame"
  @type t :: %__MODULE__{
          last_stream_id: GRPC.Transport.HTTP2.Stream.stream_id(),
          error_code: GRPC.Transport.HTTP2.Errors.error_code(),
          debug_data: iodata()
        }

  @spec deserialize(
          GRPC.Transport.HTTP2.Frame.flags(),
          GRPC.Transport.HTTP2.Stream.stream_id(),
          iodata()
        ) ::
          {:ok, t()} | {:error, GRPC.Transport.HTTP2.Errors.error_code(), binary()}
  def deserialize(
        _flags,
        0,
        <<_reserved::1, last_stream_id::31, error_code::32, debug_data::binary>>
      ) do
    {:ok,
     %__MODULE__{last_stream_id: last_stream_id, error_code: error_code, debug_data: debug_data}}
  end

  def deserialize(_flags, stream_id, _payload) when stream_id != 0 do
    {:error, GRPC.Transport.HTTP2.Errors.protocol_error(),
     "GOAWAY frame with non-zero stream_id (RFC9113§6.8)"}
  end

  defimpl GRPC.Transport.HTTP2.Frame.Serializable do
    def serialize(%GRPC.Transport.HTTP2.Frame.Goaway{} = frame, _max_frame_size) do
      payload = <<0::1, frame.last_stream_id::31, frame.error_code::32, frame.debug_data::binary>>
      [{7, 0, 0, payload}]
    end
  end
end
