defmodule GRPC.Factory do
  @moduledoc false

  use ExMachina

  use GRPC.Factories.Channel
  use GRPC.Factories.Client.Stream

  # Protobuf factories
  use GRPC.Factories.Proto.HelloWorld
end
