defmodule GRPC.Message.Protobuf do
  @moduledoc false

  # Module for encoding or decoding message using Protobuf.

  @spec encode(module, struct) :: binary
  def encode(mod, struct) do
    apply(mod, :encode, [struct])
  end

  @spec decode(module, binary) :: struct
  def decode(mod, message) do
    apply(mod, :decode, [message])
  end
end
