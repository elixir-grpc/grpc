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

*Code generating from proto is not supported now*

Service definition:

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
```

Server:

```elixir
defmodule Helloworld.Greeter.Server do
  use GRPC.Server, service: Helloworld.Greeter.Service

  def say_hello(request) do
    Helloworld.HelloReply.new(message: "Hello #{request.name}")
  end
end

GRPC.Server.start(Helloworld.Greeter.Server, "localhost:50051", insecure: true)
```

Client:

```elixir
defmodule Helloworld.Greeter.Stub do
  use GRPC.Stub, service: Helloworld.Greeter.Service
end

{:ok, channel} = GRPC.Channel.connect("localhost:50051", insecure: true)
reply = channel |> Helloworld.Greeter.Stub.say_hello(Helloworld.HelloRequest.new(name: "grpc-elixir"))
%Helloworld.HelloReply{message: "Hello grpc-elixir"}
```

See `test` dir for more examples
