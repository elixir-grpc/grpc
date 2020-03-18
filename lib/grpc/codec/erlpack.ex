defmodule GRPC.Codec.Erlpack do
  @behaviour GRPC.Codec

  def name() do
    "erlpack"
  end

  def encode(struct) do
    :erlang.term_to_binary(struct)
  end

  def pack_encoded(binary), do: binary

  def prepare_decode(binary), do: binary

  def decode(binary, _module) do
    :erlang.binary_to_term(binary)
  end
end
