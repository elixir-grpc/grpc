defmodule GRPC.HTTP2.Frame.PushPromise do
  @moduledoc false

  import GRPC.HTTP2.Frame.Flags

  defstruct stream_id: nil,
            end_headers: false,
            promised_stream_id: nil,
            fragment: nil

  @typedoc "An HTTP/2 PUSH_PROMISE frame"
  @type t :: %__MODULE__{
          stream_id: GRPC.HTTP2.Stream.stream_id(),
          end_headers: boolean(),
          promised_stream_id: GRPC.HTTP2.Stream.stream_id(),
          fragment: iodata()
        }

  @end_headers_bit 2
  @padding_bit 3

  @spec deserialize(GRPC.HTTP2.Frame.flags(), GRPC.HTTP2.Stream.stream_id(), iodata()) ::
          {:ok, t()} | {:error, GRPC.HTTP2.Errors.error_code(), binary()}
  def deserialize(_flags, 0, _payload) do
    {:error, GRPC.HTTP2.Errors.protocol_error(),
     "PUSH_PROMISE frame with zero stream_id (RFC9113§6.6)"}
  end

  def deserialize(
        flags,
        stream_id,
        <<padding_length::8, _reserved::1, promised_stream_id::31, rest::binary>>
      )
      when set?(flags, @padding_bit) and byte_size(rest) >= padding_length do
    {:ok,
     %__MODULE__{
       stream_id: stream_id,
       end_headers: set?(flags, @end_headers_bit),
       promised_stream_id: promised_stream_id,
       fragment: binary_part(rest, 0, byte_size(rest) - padding_length)
     }}
  end

  def deserialize(flags, stream_id, <<_reserved::1, promised_stream_id::31, fragment::binary>>)
      when clear?(flags, @padding_bit) do
    {:ok,
     %__MODULE__{
       stream_id: stream_id,
       end_headers: set?(flags, @end_headers_bit),
       promised_stream_id: promised_stream_id,
       fragment: fragment
     }}
  end

  def deserialize(flags, _stream_id, <<_padding_length::8, _rest::binary>>)
      when set?(flags, @padding_bit) do
    {:error, GRPC.HTTP2.Errors.protocol_error(),
     "PUSH_PROMISE frame with invalid padding length (RFC9113§6.6)"}
  end

  defimpl GRPC.HTTP2.Frame.Serializable do
    @end_headers_bit 2

    def serialize(%GRPC.HTTP2.Frame.PushPromise{} = frame, _max_frame_size) do
      payload = <<0::1, frame.promised_stream_id::31, frame.fragment::binary>>
      [{0x5, set([@end_headers_bit]), frame.stream_id, payload}]
    end
  end
end
