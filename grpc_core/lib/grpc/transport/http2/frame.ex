defmodule GRPC.Transport.HTTP2.Frame do
  @moduledoc false
  # HTTP/2 frame parsing and serialization adapted from Bandit

  @typedoc "Indicates a frame type"
  @type frame_type :: non_neg_integer()

  @typedoc "The flags passed along with a frame"
  @type flags :: byte()

  @typedoc "A valid HTTP/2 frame"
  @type frame ::
          GRPC.Transport.HTTP2.Frame.Data.t()
          | GRPC.Transport.HTTP2.Frame.Headers.t()
          | GRPC.Transport.HTTP2.Frame.Priority.t()
          | GRPC.Transport.HTTP2.Frame.RstStream.t()
          | GRPC.Transport.HTTP2.Frame.Settings.t()
          | GRPC.Transport.HTTP2.Frame.Ping.t()
          | GRPC.Transport.HTTP2.Frame.Goaway.t()
          | GRPC.Transport.HTTP2.Frame.WindowUpdate.t()
          | GRPC.Transport.HTTP2.Frame.Continuation.t()
          | GRPC.Transport.HTTP2.Frame.Unknown.t()

  @spec deserialize(binary(), non_neg_integer()) ::
          {{:ok, frame()}, iodata()}
          | {{:more, iodata()}, <<>>}
          | {{:error, GRPC.Transport.HTTP2.Errors.error_code(), binary()}, iodata()}
          | nil
  def deserialize(
        <<length::24, type::8, flags::8, _reserved::1, stream_id::31,
          payload::binary-size(length), rest::binary>>,
        max_frame_size
      )
      when length <= max_frame_size do
    case deserialize_frame_by_type(type, flags, stream_id, payload) do
      {:ok, frame} -> {{:ok, frame}, rest}
      {:error, error_code, reason} -> {{:error, error_code, reason}, rest}
    end
  end

  def deserialize(
        <<length::24, _type::8, _flags::8, _reserved::1, _stream_id::31, rest::binary>>,
        max_frame_size
      )
      when length > max_frame_size do
    {{:error, GRPC.Transport.HTTP2.Errors.frame_size_error(),
      "Payload size too large (RFC9113§4.2)"}, rest}
  end

  # nil is used to indicate for Stream.unfold/2 that the frame deserialization is finished
  def deserialize(<<>>, _max_frame_size) do
    nil
  end

  def deserialize(msg, _max_frame_size) do
    {{:more, msg}, <<>>}
  end

  defp deserialize_frame_by_type(type, flags, stream_id, payload) do
    case type do
      0 -> GRPC.Transport.HTTP2.Frame.Data.deserialize(flags, stream_id, payload)
      1 -> GRPC.Transport.HTTP2.Frame.Headers.deserialize(flags, stream_id, payload)
      2 -> GRPC.Transport.HTTP2.Frame.Priority.deserialize(flags, stream_id, payload)
      3 -> GRPC.Transport.HTTP2.Frame.RstStream.deserialize(flags, stream_id, payload)
      4 -> GRPC.Transport.HTTP2.Frame.Settings.deserialize(flags, stream_id, payload)
      5 -> GRPC.Transport.HTTP2.Frame.PushPromise.deserialize(flags, stream_id, payload)
      6 -> GRPC.Transport.HTTP2.Frame.Ping.deserialize(flags, stream_id, payload)
      7 -> GRPC.Transport.HTTP2.Frame.Goaway.deserialize(flags, stream_id, payload)
      8 -> GRPC.Transport.HTTP2.Frame.WindowUpdate.deserialize(flags, stream_id, payload)
      9 -> GRPC.Transport.HTTP2.Frame.Continuation.deserialize(flags, stream_id, payload)
      _unknown -> GRPC.Transport.HTTP2.Frame.Unknown.deserialize(type, flags, stream_id, payload)
    end
  end

  defprotocol Serializable do
    @moduledoc false

    @spec serialize(any(), non_neg_integer()) :: [
            {GRPC.Transport.HTTP2.Frame.frame_type(), GRPC.Transport.HTTP2.Frame.flags(),
             GRPC.Transport.HTTP2.Stream.stream_id(), iodata()}
          ]
    def serialize(frame, max_frame_size)
  end

  @spec serialize(frame(), non_neg_integer()) :: iolist()
  def serialize(frame, max_frame_size) do
    frame
    |> Serializable.serialize(max_frame_size)
    |> Enum.map(fn {type, flags, stream_id, payload} ->
      [<<IO.iodata_length(payload)::24, type::8, flags::8, 0::1, stream_id::31>>, payload]
    end)
  end
end
