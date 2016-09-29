defmodule GRPC.ServiceTest do
  use ExUnit.Case, async: true

  defmodule Helloworld do
    @external_resource Path.expand("../../priv/protos/helloworld.proto", __DIR__)
    use Protobuf, from: Path.expand("../../priv/protos/helloworld.proto", __DIR__)
  end

  defmodule Helloworld.Greeter.Service do
    use GRPC.Service, name: "helloworld.Greeter"

    alias Helloworld.{HelloRequest, HelloReply}

    rpc :SayHello, HelloRequest, HelloReply
  end

  defmodule Helloworld.Greeter.Stub do
    use GRPC.Stub, service: Helloworld.Greeter.Service
  end

  defmodule Helloworld.Greeter.Server do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(request) do
      Helloworld.HelloReply.new(message: "Hello #{request.name}")
    end
  end

  test "the client has functions created by rpc" do
    GRPC.Server.start(Helloworld.Greeter.Server, "localhost:50051", insecure: true)

    {:ok, channel} = GRPC.Channel.connect("localhost:50051", insecure: true)
    reply = channel |> Helloworld.Greeter.Stub.say_hello(Helloworld.HelloRequest.new(name: "grpc-elixir"))
    assert reply == Helloworld.HelloReply.new(message: "Hello grpc-elixir")
  end
end
