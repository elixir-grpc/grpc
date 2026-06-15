defmodule GRPC.Codec.Proto do
  @behaviour GRPC.Codec

  def name() do
    "proto"
  end

  def encode(struct, _opts \\ []) do
    Protobuf.encode_to_iodata(struct)
  end

  def decode(binary, module) do
    Protobuf.decode(binary, module)
  end
end
