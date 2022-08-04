defmodule GRPC.Codec.WebText do
  @behaviour GRPC.Codec

  def name() do
    "text"
  end

  def encode(struct) do
    Protobuf.Encoder.encode(struct)
  end

  def pack_for_channel(data) when is_list(data) do
    data
    |> IO.iodata_to_binary()
    |> Base.encode64()
  end

  def pack_for_channel(binary) do
    Base.encode64(binary)
  end

  def unpack_from_channel(binary) do
    Base.decode64!(binary)
  end

  def decode(binary, module) do
    Protobuf.Decoder.decode(binary, module)
  end
end
