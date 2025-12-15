defmodule GRPC.Transport.HTTP2.Frame.Continuation do
  @moduledoc false

  import GRPC.Transport.HTTP2.Frame.Flags

  defstruct stream_id: nil,
            end_headers: false,
            fragment: nil

  @typedoc "An HTTP/2 CONTINUATION frame"
  @type t :: %__MODULE__{
          stream_id: GRPC.Transport.HTTP2.Stream.stream_id(),
          end_headers: boolean(),
          fragment: iodata()
        }

  @end_headers_bit 2

  @spec deserialize(
          GRPC.Transport.HTTP2.Frame.flags(),
          GRPC.Transport.HTTP2.Stream.stream_id(),
          iodata()
        ) ::
          {:ok, t()} | {:error, GRPC.Transport.HTTP2.Errors.error_code(), binary()}
  def deserialize(_flags, 0, _payload) do
    {:error, GRPC.Transport.HTTP2.Errors.protocol_error(),
     "CONTINUATION frame with zero stream_id (RFC9113§6.10)"}
  end

  def deserialize(flags, stream_id, <<fragment::binary>>) do
    {:ok,
     %__MODULE__{
       stream_id: stream_id,
       end_headers: set?(flags, @end_headers_bit),
       fragment: fragment
     }}
  end

  defimpl GRPC.Transport.HTTP2.Frame.Serializable do
    @end_headers_bit 2

    def serialize(%GRPC.Transport.HTTP2.Frame.Continuation{} = frame, max_frame_size) do
      fragment_length = IO.iodata_length(frame.fragment)

      if fragment_length <= max_frame_size do
        [{9, set([@end_headers_bit]), frame.stream_id, frame.fragment}]
      else
        <<this_frame::binary-size(max_frame_size), rest::binary>> =
          IO.iodata_to_binary(frame.fragment)

        [
          {9, 0, frame.stream_id, this_frame}
          | GRPC.Transport.HTTP2.Frame.Serializable.serialize(
              %GRPC.Transport.HTTP2.Frame.Continuation{
                stream_id: frame.stream_id,
                fragment: rest
              },
              max_frame_size
            )
        ]
      end
    end
  end
end
