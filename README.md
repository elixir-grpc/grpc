# gRPC Elixir

The Elixir implementation of [gRPC](https://github.com/grpc/grpc).

**WARNING: This is unstable. Don't use in production!**

## Installation

The package can be installed as:

  1. Add `grpc` to your list of dependencies in `mix.exs`:

    ```elixir
    def deps do
      [{:grpc, github: "tony612/grpc-elixir"}]
    end
    ```

  2. Ensure `grpc` is started before your application:

    ```elixir
    def application do
      [applications: [:grpc]]
    end
    ```

## Usage

Client:

```elixir
defmodule Helloworld do
  @external_resource Path.expand("priv/protos/helloworld.proto", __DIR__)
  use Protobuf, from: Path.expand("priv/protos/helloworld.proto", __DIR__)
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

> {:ok, channel} = GRPC.Channel.connect("localhost:50051", insecure: true)
> reply = channel |> Helloworld.Greeter.Stub.say_hello(Helloworld.HelloRequest.new(name: "grpc-elixir"))
%Helloworld.HelloReply{message: "Hello grpc-elixir"}
```

See `test` dir for more examples
