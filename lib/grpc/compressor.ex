defmodule GRPC.Compressor do
  @callback name() :: String.t()
  @callback compress(iodata()) :: binary
  @callback decompress(binary) :: binary
end
