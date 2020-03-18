defmodule GRPC.Codec.Proto do
  @behaviour GRPC.Codec

  def name() do
    "proto"
  end

  def encode(%mod{} = struct) do
    mod.encode(struct)
  end

  def pack_encoded(binary), do: binary

  def prepare_decode(binary), do: binary

  def decode(binary, module) do
    module.decode(binary)
  end
end
