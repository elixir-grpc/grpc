defmodule GRPC.Transport.HTTP2.Frame.Settings do
  @moduledoc false

  import GRPC.Transport.HTTP2.Frame.Flags
  import Bitwise

  @max_window_size (1 <<< 31) - 1
  @min_frame_size 1 <<< 14
  @max_frame_size (1 <<< 24) - 1

  defstruct ack: false, settings: nil

  @typedoc "An HTTP/2 SETTINGS frame"
  @type t :: %__MODULE__{ack: true, settings: nil} | %__MODULE__{ack: false, settings: map()}

  @ack_bit 0

  @spec deserialize(
          GRPC.Transport.HTTP2.Frame.flags(),
          GRPC.Transport.HTTP2.Stream.stream_id(),
          iodata()
        ) ::
          {:ok, t()} | {:error, GRPC.Transport.HTTP2.Errors.error_code(), binary()}
  def deserialize(flags, 0, payload) when not set?(flags, @ack_bit) do
    payload
    |> Stream.unfold(fn
      <<>> -> nil
      <<setting::16, value::32, rest::binary>> -> {{:ok, {setting, value}}, rest}
      <<rest::binary>> -> {{:error, rest}, <<>>}
    end)
    |> Enum.reduce_while({:ok, %{}}, fn
      {:ok, {1, value}}, {:ok, acc} ->
        {:cont, {:ok, Map.put(acc, :header_table_size, value)}}

      {:ok, {2, val}}, {:ok, acc} when val in [0, 1] ->
        {:cont, {:ok, acc}}

      {:ok, {2, _value}}, {:ok, _acc} ->
        {:halt,
         {:error, GRPC.Transport.HTTP2.Errors.protocol_error(),
          "Invalid enable_push value (RFC9113§6.5)"}}

      {:ok, {3, value}}, {:ok, acc} ->
        {:cont, {:ok, Map.put(acc, :max_concurrent_streams, value)}}

      {:ok, {4, value}}, {:ok, _acc} when value > @max_window_size ->
        {:halt,
         {:error, GRPC.Transport.HTTP2.Errors.flow_control_error(),
          "Invalid window_size (RFC9113§6.5)"}}

      {:ok, {4, value}}, {:ok, acc} ->
        {:cont, {:ok, Map.put(acc, :initial_window_size, value)}}

      {:ok, {5, value}}, {:ok, _acc} when value < @min_frame_size ->
        {:halt,
         {:error, GRPC.Transport.HTTP2.Errors.frame_size_error(),
          "Invalid max_frame_size (RFC9113§6.5)"}}

      {:ok, {5, value}}, {:ok, _acc} when value > @max_frame_size ->
        {:halt,
         {:error, GRPC.Transport.HTTP2.Errors.frame_size_error(),
          "Invalid max_frame_size (RFC9113§6.5)"}}

      {:ok, {5, value}}, {:ok, acc} ->
        {:cont, {:ok, Map.put(acc, :max_frame_size, value)}}

      {:ok, {6, value}}, {:ok, acc} ->
        {:cont, {:ok, Map.put(acc, :max_header_list_size, value)}}

      {:ok, {_setting, _value}}, {:ok, acc} ->
        {:cont, {:ok, acc}}

      {:error, _rest}, _acc ->
        {:halt,
         {:error, GRPC.Transport.HTTP2.Errors.frame_size_error(),
          "Invalid SETTINGS size (RFC9113§6.5)"}}
    end)
    |> case do
      {:ok, settings} -> {:ok, %__MODULE__{ack: false, settings: settings}}
      {:error, error_code, reason} -> {:error, error_code, reason}
    end
  end

  def deserialize(flags, 0, <<>>) when set?(flags, @ack_bit) do
    {:ok, %__MODULE__{ack: true}}
  end

  def deserialize(flags, 0, _payload) when set?(flags, @ack_bit) do
    {:error, GRPC.Transport.HTTP2.Errors.frame_size_error(),
     "SETTINGS ack frame with non-empty payload (RFC9113§6.5)"}
  end

  def deserialize(_flags, _stream_id, _payload) do
    {:error, GRPC.Transport.HTTP2.Errors.protocol_error(), "Invalid SETTINGS frame (RFC9113§6.5)"}
  end

  defimpl GRPC.Transport.HTTP2.Frame.Serializable do
    @ack_bit 0

    def serialize(%GRPC.Transport.HTTP2.Frame.Settings{ack: true}, _max_frame_size),
      do: [{4, set([@ack_bit]), 0, <<>>}]

    def serialize(%GRPC.Transport.HTTP2.Frame.Settings{ack: false} = frame, _max_frame_size) do
      payload =
        frame.settings
        |> Enum.uniq_by(fn {setting, _} -> setting end)
        |> Enum.map(fn
          {:header_table_size, 4_096} -> <<>>
          {:header_table_size, value} -> <<1::16, value::32>>
          {:max_concurrent_streams, :infinity} -> <<>>
          {:max_concurrent_streams, value} -> <<3::16, value::32>>
          {:initial_window_size, 65_535} -> <<>>
          {:initial_window_size, value} -> <<4::16, value::32>>
          {:max_frame_size, 16_384} -> <<>>
          {:max_frame_size, value} -> <<5::16, value::32>>
          {:max_header_list_size, :infinity} -> <<>>
          {:max_header_list_size, value} -> <<6::16, value::32>>
        end)

      [{4, 0, 0, payload}]
    end
  end
end
