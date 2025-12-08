defmodule GRPC.HTTP2.Frame do
  @moduledoc false
  # HTTP/2 frame parsing and serialization adapted from Bandit

  @typedoc "Indicates a frame type"
  @type frame_type :: non_neg_integer()

  @typedoc "The flags passed along with a frame"
  @type flags :: byte()

  @typedoc "A valid HTTP/2 frame"
  @type frame ::
          GRPC.HTTP2.Frame.Data.t()
          | GRPC.HTTP2.Frame.Headers.t()
          | GRPC.HTTP2.Frame.Priority.t()
          | GRPC.HTTP2.Frame.RstStream.t()
          | GRPC.HTTP2.Frame.Settings.t()
          | GRPC.HTTP2.Frame.Ping.t()
          | GRPC.HTTP2.Frame.Goaway.t()
          | GRPC.HTTP2.Frame.WindowUpdate.t()
          | GRPC.HTTP2.Frame.Continuation.t()
          | GRPC.HTTP2.Frame.Unknown.t()

  @spec deserialize(binary(), non_neg_integer()) ::
          {{:ok, frame()}, iodata()}
          | {{:more, iodata()}, <<>>}
          | {{:error, GRPC.HTTP2.Errors.error_code(), binary()}, iodata()}
          | nil
  def deserialize(
        <<length::24, type::8, flags::8, _reserved::1, stream_id::31,
          payload::binary-size(length), rest::binary>>,
        max_frame_size
      )
      when length <= max_frame_size do
    type
    |> case do
      0x0 -> GRPC.HTTP2.Frame.Data.deserialize(flags, stream_id, payload)
      0x1 -> GRPC.HTTP2.Frame.Headers.deserialize(flags, stream_id, payload)
      0x2 -> GRPC.HTTP2.Frame.Priority.deserialize(flags, stream_id, payload)
      0x3 -> GRPC.HTTP2.Frame.RstStream.deserialize(flags, stream_id, payload)
      0x4 -> GRPC.HTTP2.Frame.Settings.deserialize(flags, stream_id, payload)
      0x5 -> GRPC.HTTP2.Frame.PushPromise.deserialize(flags, stream_id, payload)
      0x6 -> GRPC.HTTP2.Frame.Ping.deserialize(flags, stream_id, payload)
      0x7 -> GRPC.HTTP2.Frame.Goaway.deserialize(flags, stream_id, payload)
      0x8 -> GRPC.HTTP2.Frame.WindowUpdate.deserialize(flags, stream_id, payload)
      0x9 -> GRPC.HTTP2.Frame.Continuation.deserialize(flags, stream_id, payload)
      _unknown -> GRPC.HTTP2.Frame.Unknown.deserialize(type, flags, stream_id, payload)
    end
    |> case do
      {:ok, frame} -> {{:ok, frame}, rest}
      {:error, error_code, reason} -> {{:error, error_code, reason}, rest}
    end
  end

  def deserialize(
        <<length::24, _type::8, _flags::8, _reserved::1, _stream_id::31, rest::binary>>,
        max_frame_size
      )
      when length > max_frame_size do
    {{:error, GRPC.HTTP2.Errors.frame_size_error(), "Payload size too large (RFC9113§4.2)"}, rest}
  end

  # nil is used to indicate for Stream.unfold/2 that the frame deserialization is finished
  def deserialize(<<>>, _max_frame_size) do
    nil
  end

  def deserialize(msg, _max_frame_size) do
    {{:more, msg}, <<>>}
  end

  defmodule Flags do
    @moduledoc false

    import Bitwise

    defguard set?(flags, bit) when band(flags, bsl(1, bit)) != 0
    defguard clear?(flags, bit) when band(flags, bsl(1, bit)) == 0

    @spec set([0..255]) :: 0..255
    def set([]), do: 0x0
    def set([bit | rest]), do: bor(bsl(1, bit), set(rest))
  end

  defprotocol Serializable do
    @moduledoc false

    @spec serialize(any(), non_neg_integer()) :: [
            {GRPC.HTTP2.Frame.frame_type(), GRPC.HTTP2.Frame.flags(),
             GRPC.HTTP2.Stream.stream_id(), iodata()}
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
