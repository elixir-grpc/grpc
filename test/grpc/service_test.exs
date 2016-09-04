defmodule GRPC.ServiceTest do
  use ExUnit.Case, async: true

  defmodule Helloworld do
    @external_resource Path.expand("../proto/helloworld.proto", __DIR__)
    use Protobuf, from: Path.expand("../proto/helloworld.proto", __DIR__)
  end

  defmodule Helloworld.Greeter.Service do
    use GRPC.Service, name: "helloworld.Greeter",
                      marshal_function: :encode,
                      unmarshal_function: :decode

    alias Helloworld.{HelloRequest, HelloReply}

    rpc :SayHello, HelloRequest, HelloReply
  end

  defmodule Helloworld.Greeter.Stub do
    use GRPC.Stub, service: Helloworld.Greeter.Service
  end

  @tag :integration
  test "the client has functions created by rpc" do
    channel = GRPC.Core.Channel.create("localhost:50051", %{}, :this_channel_is_insecure)
    result = channel |> Helloworld.Greeter.Stub.say_hello(Helloworld.HelloRequest.new(name: "grpc-elixir"))
    assert result[:message] == Helloworld.HelloReply.new(message: "Hello grpc-elixir")
  end
end
