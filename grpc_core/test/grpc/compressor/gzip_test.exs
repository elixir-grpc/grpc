defmodule GRPC.Compressor.GzipTest do
  use ExUnit.Case, async: true

  alias GRPC.Compressor.Gzip

  test "round-trips small payloads correctly" do
    data = String.duplicate("hello world ", 50)
    assert Gzip.decompress(Gzip.compress(data)) == data
  end

  test "raises GRPC.RPCError :resource_exhausted on decompression bomb" do
    # Build a small gzip payload that decompresses to 2x the default 4 MB limit.
    bomb_size = 9 * 1024 * 1024
    compressed = :zlib.gzip(:binary.copy(<<0>>, bomb_size))

    assert_raise GRPC.RPCError, ~r/exceeds limit/, fn ->
      Gzip.decompress(compressed)
    end
  end

  test "respects :max_decompressed_message_length application env" do
    # Temporarily lower the limit to 1 KB.
    Application.put_env(:grpc, :max_decompressed_message_length, 1024)

    on_exit(fn ->
      Application.delete_env(:grpc, :max_decompressed_message_length)
    end)

    oversized = :zlib.gzip(:binary.copy(<<0>>, 2048))

    assert_raise GRPC.RPCError, ~r/exceeds limit/, fn ->
      Gzip.decompress(oversized)
    end
  end

  test "accepts payload exactly at the configured limit" do
    limit = 4096
    Application.put_env(:grpc, :max_decompressed_message_length, limit)

    on_exit(fn ->
      Application.delete_env(:grpc, :max_decompressed_message_length)
    end)

    # Use incompressible data so the decompressed size is exactly `limit`.
    data = :crypto.strong_rand_bytes(limit)
    compressed = :zlib.gzip(data)

    assert Gzip.decompress(compressed) == data
  end
end
