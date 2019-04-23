defmodule GRPC.Codec.Proto do
  @behaviour GRPC.Codec

  def content_subtype() do
    "proto"
  end

  def encode(struct) do
    Protobuf.Encoder.encode(struct)
  end

  def decode(binary, module) do
    Protobuf.Decoder.decode(binary, module)
  end
end
