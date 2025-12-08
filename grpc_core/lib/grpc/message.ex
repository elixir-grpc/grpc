defmodule GRPC.Message do
  @moduledoc """
  Transform data between encoded protobuf and HTTP/2 body of gRPC.

  gRPC body format is:

    http://www.grpc.io/docs/guides/wire.html
    Delimited-Message -> Compressed-Flag Message-Length Message
    Compressed-Flag -> 0 / 1 # encoded as 1 byte unsigned integer
    Message-Length -> {length of Message} # encoded as 4 byte unsigned integer
    Message -> *{binary octet}
  """

  import Bitwise
  alias GRPC.RPCError

  @max_message_length bsl(1, 32 - 1)
  @trailers_flag 0b1000_0000

  @doc """
  Transforms Protobuf data into a gRPC body binary.

  ## Options

    * `:compressor` - the optional `GRPC.Compressor` to be used.
    * `:iolist` - if `true`, encodes the data as an `t:iolist()` instead of a `t:binary()`
    * `:max_message_length` - the maximum number of bytes for the encoded message.

  ## Examples

      iex> message = ["m", [["es", "sa"], "ge"]]
      iex> GRPC.Message.to_data(message)
      {:ok, <<0, 0, 0, 0, 7, "message">>, 12}
      iex> GRPC.Message.to_data(message, iolist: true)
      {:ok, [0, <<0, 0, 0, 7>>, ["m", [["es", "sa"], "ge"]]], 12}

  Error cases:

      iex> message = <<1, 2, 3, 4, 5, 6, 7, 8, 9>>
      iex> GRPC.Message.to_data(message, %{max_message_length: 8})
      {:error, "Encoded message is too large (9 bytes)"}

  """
  @spec to_data(iodata, keyword()) ::
          {:ok, iodata, non_neg_integer} | {:error, String.t()}
  def to_data(message, opts \\ []) do
    compressor = opts[:compressor]
    iolist = opts[:iolist]
    codec = opts[:codec]
    max_length = opts[:max_message_length] || @max_message_length
    additional_flags = opts[:message_flag] || 0

    {flag, message} =
      if compressor do
        {1 ||| additional_flags, compressor.compress(message)}
      else
        {0 ||| additional_flags, message}
      end

    length = IO.iodata_length(message)

    if length > max_length do
      {:error, "Encoded message is too large (#{length} bytes)"}
    else
      result = [flag, <<length::size(4)-unit(8)>>, message]

      result =
        if function_exported?(codec, :pack_for_channel, 1),
          do: codec.pack_for_channel(result),
          else: result

      result = if iolist, do: result, else: IO.iodata_to_binary(result)

      {:ok, result, length + 5}
    end
  end

  @doc """
  Transforms gRPC body into Protobuf data.

  ## Examples

      iex> GRPC.Message.from_data(<<0, 0, 0, 0, 8, 1, 2, 3, 4, 5, 6, 7, 8>>)
      {<<1, 2, 3, 4, 5, 6, 7, 8>>, <<>>}
  """
  @spec from_data(binary) :: {message :: binary, rest :: binary}
  def from_data(data) do
    <<_flag::unsigned-integer-size(8), length::big-32, message::bytes-size(length), rest::binary>> =
      data

    {message, rest}
  end

  @doc """
  Transform gRPC body into Protobuf data with compression.

  ## Examples

      iex> GRPC.Message.from_data(%{compressor: nil}, <<0, 0, 0, 0, 8, 1, 2, 3, 4, 5, 6, 7, 8>>)
      {:ok, <<1, 2, 3, 4, 5, 6, 7, 8>>, <<>>}
  """
  @spec from_data(map, binary) ::
          {:ok, message :: binary, rest :: binary}
          | {:trailers, map, rest :: binary}
          | {:error, GRPC.RPCError.t()}
  def from_data(%{compressor: nil}, data) do
    case data do
      <<0, length::big-32, message::bytes-size(length), rest::binary>> ->
        {:ok, message, rest}

      <<1, _length::bytes-size(4), _::binary>> ->
        {:error,
         RPCError.exception(
           status: :internal,
           message: "Compressed flag is set, but not specified in headers."
         )}

      <<@trailers_flag, length::big-32, message::bytes-size(length), rest::binary>> ->
        {:trailers, parse_trailers(message), rest}

      _ ->
        {:error, RPCError.exception(status: :invalid_argument, message: "Message is malformed.")}
    end
  end

  def from_data(%{compressor: compressor}, data) do
    case data do
      <<1, length::big-32, message::bytes-size(length), rest::binary>> ->
        {:ok, compressor.decompress(message), rest}

      <<0, length::big-32, message::bytes-size(length), rest::binary>> ->
        {:ok, message, rest}

      <<@trailers_flag, length::big-32, message::bytes-size(length), rest::binary>> ->
        {:trailers, parse_trailers(message), rest}

      _ ->
        {:error, RPCError.exception(status: :invalid_argument, message: "Message is malformed.")}
    end
  end

  defp parse_trailers(data) do
    data
    |> String.split("\r\n")
    |> Enum.reduce(%{}, fn line, acc ->
      [k, v] = String.split(line, ":", parts: 2)
      Map.put(acc, k, String.trim(v))
    end)
  end

  def from_frame(bin), do: from_frame(bin, [])
  def from_frame(<<>>, acc), do: Enum.reverse(acc)

  def from_frame(
        <<_flag::unsigned-integer-size(8), length::32, msg::bytes-size(length), rest::binary>>,
        acc
      ) do
    from_frame(rest, [msg | acc])
  end

  def complete?(
        <<_flag::unsigned-integer-size(8), length::unsigned-integer-size(32), message::binary>>
      ) do
    length == byte_size(message)
  end

  def complete?(_), do: false

  def message_length(data) do
    <<_flag::unsigned-integer-size(8), length::unsigned-integer-size(32), _message::binary>> =
      data

    length
  end

  @doc """
  Get message data from data buffer.

  ## Examples

      iex> GRPC.Message.get_message(<<0, 0, 0, 0, 8, 1, 2, 3, 4, 5, 6, 7, 8, 0, 0, 0>>)
      {{0, <<1, 2, 3, 4, 5, 6, 7, 8>>}, <<0, 0, 0>>}
      iex> GRPC.Message.get_message(<<1, 0, 0, 0, 8, 1, 2, 3, 4, 5, 6, 7, 8>>)
      {{1, <<1, 2, 3, 4, 5, 6, 7, 8>>}, <<>>}
      iex> GRPC.Message.get_message(<<0, 0, 0, 0, 8, 1, 2, 3, 4, 5, 6, 7>>)
      false
  """
  def get_message(
        <<flag::unsigned-integer-size(8), length::unsigned-integer-size(32),
          message::bytes-size(length), rest::binary>>
      ) do
    case flag do
      @trailers_flag -> {{:trailers, message}, rest}
      _ -> {{flag, message}, rest}
    end
  end

  def get_message(_) do
    false
  end

  def get_message(data, nil = _compressor) do
    case data do
      <<@trailers_flag::8, length::unsigned-integer-size(32), message::bytes-size(length),
        rest::binary>> ->
        {{:trailers, message}, rest}

      <<flag::unsigned-integer-size(8), length::unsigned-integer-size(32),
        message::bytes-size(length), rest::binary>> ->
        {{flag, message}, rest}

      _other ->
        data
    end
  end

  def get_message(data, compressor) do
    case data do
      <<1::8, length::unsigned-integer-32, message::bytes-size(length), rest::binary>> ->
        {{1, compressor.decompress(message)}, rest}

      <<0::8, length::unsigned-integer-32, message::bytes-size(length), rest::binary>> ->
        {{0, message}, rest}

      <<@trailers_flag::8, length::unsigned-integer-32, message::bytes-size(length),
        rest::binary>> ->
        {{:trailers, message}, rest}

      _other ->
        data
    end
  end
end
