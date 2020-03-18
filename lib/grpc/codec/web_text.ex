defmodule GRPC.Codec.WebText do
  @behaviour GRPC.Codec

  def name() do
    "text"
  end

  def encode(struct) do
    Protobuf.Encoder.encode(struct)
  end

  def pack_encoded(binary) do
    Base.encode64(binary)
  end

  def prepare_decode(binary) do
    Base.decode64!(binary)
  end

  def decode(binary, module) do
    Protobuf.Decoder.decode(binary, module)
  end
end
