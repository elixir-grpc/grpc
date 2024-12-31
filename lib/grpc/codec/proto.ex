defmodule GRPC.Codec.Proto do
  @behaviour GRPC.Codec

  def name() do
    "proto"
  end

  def encode(struct) do
    Protobuf.Encoder.encode_to_iodata(struct)
  end

  def decode(binary, module) do
    module.decode(binary)
  end
end
