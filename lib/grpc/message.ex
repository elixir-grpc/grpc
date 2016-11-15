defmodule GRPC.Message do
  use Bitwise, only_operators: true
  @max_message_length 1<<<32 - 1

  @moduledoc """
  Transform data between encoded protobuf and HTTP/2 body of gRPC.

  gRPC body format is:

      # http://www.grpc.io/docs/guides/wire.html
      Delimited-Message -> Compressed-Flag Message-Length Message
      Compressed-Flag -> 0 / 1 # encoded as 1 byte unsigned integer
      Message-Length -> {length of Message} # encoded as 4 byte unsigned integer
      Message -> *{binary octet}
  """

  @doc """
  Transform protobuf data to gRPC body

  ## Examples

      iex> message = <<1, 2, 3, 4, 5, 6, 7, 8>>
      iex> GRPC.Message.to_data(message)
      {:ok, <<0, 0, 0, 0, 8, 1, 2, 3, 4, 5, 6, 7, 8>>}
      iex> GRPC.Message.to_data(message, iolist: true)
      {:ok, [<<0>>, <<0, 0, 0, 8>>, <<1, 2, 3, 4, 5, 6, 7, 8>>]}
      iex> message = <<1, 2, 3, 4, 5, 6, 7, 8>>
      iex> GRPC.Message.to_data(message, compressor: true)
      {:ok, <<1, 0, 0, 0, 8, 1, 2, 3, 4, 5, 6, 7, 8>>}
      iex> message = <<1, 2, 3, 4, 5, 6, 7, 8, 9>>
      iex> GRPC.Message.to_data(message, max_message_length: 8)
      {:error, "Encoded message is too large (9 bytes)"}
  """
  @spec to_data(binary, keyword) :: {:ok, binary} | {:error, String.t}
  def to_data(message, opts \\ []) do
    compress_flag = if opts[:compressor], do: <<1>>, else: <<0>>
    length = byte_size(message)
    max_length = opts[:max_message_length] || @max_message_length
    if length > max_length do
      {:error, "Encoded message is too large (#{length} bytes)"}
    else
      result = [compress_flag, <<length::size(4)-unit(8)>>, message]
      result = if opts[:iolist], do: result, else: Enum.join(result)
      {:ok, result}
    end
  end

  @doc """
  Transform gRPC body to protobuf data

  ## Examples

      iex> GRPC.Message.from_data(<<0, 0, 0, 0, 8, 1, 2, 3, 4, 5, 6, 7, 8>>)
      <<1, 2, 3, 4, 5, 6, 7, 8>>
  """
  @spec from_data(binary) :: binary
  def from_data(data) do
    <<_flag::bytes-size(1), _length::bytes-size(4), message::binary>> = data
    message
  end
end
