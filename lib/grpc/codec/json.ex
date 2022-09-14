defmodule GRPC.Codec.JSON do
  @behaviour GRPC.Codec

  def name() do
    "json"
  end

  def encode(struct) do
    Protobuf.JSON.encode!(struct)
  end

  def decode(<<>>, _module) do
    %{}
  end

  def decode(binary, _module) do
    if jason = load_jason() do
      jason.decode!(binary)
    else
      raise "`:jason` library not loaded"
    end
  end

  defp load_jason, do: Code.ensure_loaded?(Jason) and Jason
end
