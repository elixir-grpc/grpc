defmodule GRPC.Codec.JSON do
  @behaviour GRPC.Codec

  def name() do
    "json"
  end

  def encode(struct) do
    Protobuf.JSON.encode!(struct)
  end


  def decode(<<>>, module) do
    struct(module, [])
  end

  def decode(binary, module) do
    Protobuf.JSON.decode!(binary, module)
  end
end
