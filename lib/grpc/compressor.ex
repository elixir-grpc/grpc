defmodule GRPC.Compressor do
  @callback name() :: String.t()
  @callback compress(binary) :: binary
  @callback decompress(binary) :: binary
end
