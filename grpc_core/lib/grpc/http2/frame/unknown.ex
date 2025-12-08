defmodule GRPC.HTTP2.Frame.Unknown do
  @moduledoc false

  defstruct type: nil,
            flags: nil,
            stream_id: nil,
            payload: nil

  @typedoc "An HTTP/2 frame of unknown type"
  @type t :: %__MODULE__{
          type: GRPC.HTTP2.Frame.frame_type(),
          flags: GRPC.HTTP2.Frame.flags(),
          stream_id: GRPC.HTTP2.Stream.stream_id(),
          payload: iodata()
        }

  # Note this is arity 4
  @spec deserialize(
          GRPC.HTTP2.Frame.frame_type(),
          GRPC.HTTP2.Frame.flags(),
          GRPC.HTTP2.Stream.stream_id(),
          iodata()
        ) :: {:ok, t()}
  def deserialize(type, flags, stream_id, payload) do
    {:ok, %__MODULE__{type: type, flags: flags, stream_id: stream_id, payload: payload}}
  end
end
