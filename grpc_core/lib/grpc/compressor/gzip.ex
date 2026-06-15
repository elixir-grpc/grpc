defmodule GRPC.Compressor.Gzip do
  @behaviour GRPC.Compressor

  # 4 MB – matches gRPC-Go's default max receive message size.
  # Override with: Application.put_env(:grpc, :max_decompressed_message_length, bytes)
  @default_max_decompressed_size 4 * 1024 * 1024

  # Feed compressed input in 8 KB slices so we can detect an oversized output
  # before allocating the full decompressed payload.
  @input_chunk_size 8_192

  def name do
    "gzip"
  end

  def compress(data) do
    :zlib.gzip(data)
  end

  def decompress(data) do
    max_size =
      Application.get_env(:grpc, :max_decompressed_message_length, @default_max_decompressed_size)

    z = :zlib.open()
    # windowBits 31 = 15 (max window) + 16 (gzip format flag)
    :ok = :zlib.inflateInit(z, 31)

    try do
      chunks = inflate_chunks(z, data, max_size, 0, [])
      :zlib.inflateEnd(z)
      IO.iodata_to_binary(chunks)
    after
      :zlib.close(z)
    end
  end

  defp inflate_chunks(_z, <<>>, _max_size, _acc_size, acc), do: acc

  defp inflate_chunks(z, data, max_size, acc_size, acc) do
    {chunk, rest} = split_chunk(data)
    output = :zlib.inflate(z, chunk)
    new_size = acc_size + IO.iodata_length(output)

    if new_size > max_size do
      raise GRPC.RPCError,
        status: :resource_exhausted,
        message: "Decompressed message exceeds limit of #{max_size} bytes"
    end

    inflate_chunks(z, rest, max_size, new_size, [acc, output])
  end

  defp split_chunk(data) when byte_size(data) <= @input_chunk_size, do: {data, <<>>}

  defp split_chunk(<<chunk::bytes-size(@input_chunk_size), rest::binary>>), do: {chunk, rest}
end
