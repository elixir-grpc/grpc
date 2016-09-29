defmodule GRPC.Message.Protobuf do
  def encode(mod, message) do
    apply(mod, :encode, [message])
  end

  def decode(mod, message) do
    apply(mod, :decode, [message])
  end
end
