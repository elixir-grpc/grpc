defmodule GRPC.Message do
  use Bitwise, only_operators: true
  @max_message_length 1<<<32 - 1

  @doc """
  Delimited-Message → Compressed-Flag Message-Length Message
  Compressed-Flag → 0 / 1 # encoded as 1 byte unsigned integer
  Message-Length → {length of Message} # encoded as 4 byte unsigned integer
  Message → *{binary octet}

  ## Examples

      iex> message = <<1, 2, 3, 4, 5, 6, 7, 8>>
      iex> GRPC.Message.delimited(message)
      {:ok, <<0, 0, 0, 0, 8, 1, 2, 3, 4, 5, 6, 7, 8>>}
      iex> message = <<1, 2, 3, 4, 5, 6, 7, 8>>
      iex> GRPC.Message.delimited(message, compressor: true)
      {:ok, <<1, 0, 0, 0, 8, 1, 2, 3, 4, 5, 6, 7, 8>>}
      iex> message = <<1, 2, 3, 4, 5, 6, 7, 8, 9>>
      iex> GRPC.Message.delimited(message, max_message_length: 8)
      {:error, "Encoded message is too large (9 bytes)"}
  """
  def delimited(message, opts \\ []) do
    compress_flag = if opts[:compressor], do: <<1>>, else: <<0>>
    length = byte_size(message)
    max_length = opts[:max_message_length] || @max_message_length
    if length > max_length do
      {:error, "Encoded message is too large (#{length} bytes)"}
    else
      result = compress_flag <> <<length::size(4)-unit(8)>> <> message
      {:ok, result}
    end
  end
end
