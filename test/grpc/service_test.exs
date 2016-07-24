defmodule GRPC.ServiceTest do
  use ExUnit.Case, async: true

  defmodule Helloworld do
    use Protobuf, from: Path.expand("../proto/helloworld.proto", __DIR__)
  end

  defmodule Helloworld.Greeter.Service do
    use GRPC.Service, name: "helloworld.Greeter",
                      marshal_function: :encode,
                      unmarshal_function: :decode

    alias Helloworld.{HelloRequest, HelloReply}

    rpc :SayHello, HelloRequest, HelloReply
    rpc :AskName, HelloRequest, HelloReply
  end

  defmodule Helloworld.Greeter.Stub do
    use GRPC.Stub, service: Helloworld.Greeter.Service
  end

  test "the client has functions created by rpc" do
    channel = %GRPC.Channel{host: "localhost:50051", creds: :this_channel_is_insecure}
    res1 = channel
      |> Helloworld.Greeter.Stub.say_hello(Helloworld.HelloRequest.new(name: "Foo"))
    res2 = channel
      |> Helloworld.Greeter.Stub.ask_name(Helloworld.HelloRequest.new(name: "Bar"))
    assert Helloworld.HelloRequest.decode(res1) == %Helloworld.HelloRequest{name: "Foo"}
    assert Helloworld.HelloRequest.decode(res2) == %Helloworld.HelloRequest{name: "Bar"}
  end
end
