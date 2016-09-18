defmodule GRPC.Message do
  use Bitwise, only_operators: true
  @max_message_length 1<<<32 - 1
  def delimited(message, opts) do
    compress_flag = if opts[:compressor], do: <<1>>, else: <<0>>
    length = byte_size(message)
    if length > @max_message_length do
      {:error, "Encoded message is too large (#{length} bytes)"}
    else
      result = compress_flag <> <<length::size(4)-unit(8)>> <> message
      {:ok, result}
    end
  end
end
