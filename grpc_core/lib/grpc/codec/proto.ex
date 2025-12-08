defmodule GRPC.Codec.Proto do
  @behaviour GRPC.Codec

  # Inline codec functions for better performance
  @compile {:inline, name: 0, encode: 2, decode: 2}

  def name, do: "proto"

  def encode(struct, _opts \\ []), do: Protobuf.Encoder.encode_to_iodata(struct)

  def decode(binary, module), do: module.decode(binary)
end
