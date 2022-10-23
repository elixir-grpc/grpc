defmodule GRPC.Factory do
  @moduledoc false

  use GRPC.Factories.Channel
  use GRPC.Factories.Client.Stream

  # Protobuf factories
  use GRPC.Factories.Proto.HelloWorld

  def build(resource, attrs \\ %{}) do
    name = :"#{resource}_factory"

    data =
      if function_exported?(__MODULE__, name, 1) do
        apply(__MODULE__, name, [attrs])
      else
        apply(__MODULE__, name, [])
      end

    Map.merge(data, Map.new(attrs))
  end
end
