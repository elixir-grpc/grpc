# gRPC Elixir

[![Build Status](https://travis-ci.org/tony612/grpc-elixir.svg?branch=master)](https://travis-ci.org/tony612/grpc-elixir)
[![Inline docs](http://inch-ci.org/github/tony612/grpc-elixir.svg?branch=master)](http://inch-ci.org/github/tony612/grpc-elixir)

The Elixir implementation of [gRPC](http://www.grpc.io/).

**WARNING: APIs are unstable now. Be careful to use it in production!**

## Installation

The package can be installed as:

  1. Add `grpc` to your list of dependencies in `mix.exs`:

      ```elixir
      def deps do
        [{:grpc, github: "tony612/grpc-elixir"}]
      end
      ```

  2. (Before Elixir 1.4)Ensure `grpc` is started before your application:

      ```elixir
      def application do
        [applications: [:grpc]]
      end
      ```

## Usage

1. Generate Elixir code from proto file as [protobuf-elixir](https://github.com/tony612/protobuf-elixir#usage) shows(especially the `gRPC Support` section).
2. Implement the server side code like below and remember to return the expected message types.
```elixir
defmodule Helloworld.Greeter.Server do
  use GRPC.Server, service: Helloworld.Greeter.Service

  @spec say_hello(Helloworld.HelloRequest.t, GRPC.Server.Stream.t) :: Helloworld.HelloReply.t
  def say_hello(request, _stream) do
    Helloworld.HelloReply.new(message: "Hello #{request.name}")
  end
end
```
3. Run the server and client like this:
```elixir
iex> GRPC.Server.start(Helloworld.Greeter.Server, 50051)
iex> {:ok, channel} = GRPC.Stub.connect("localhost:50051")
iex> request = Helloworld.HelloRequest.new(name: "grpc-elixir")
iex> {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(request)
```

### Start the server

You can start the gRPC server as a supervised process. First, add `GRPC.Server.Supervisor` to your supervision tree.

```elixir
# In the start function of your Application
def start(_type, _args) do
  children = [
    # ...
    supervisor(GRPC.Server.Supervisor, [{Helloworld.Greeter.Server, 50051}])
  ]

  opts = [strategy: :one_for_one, name: YourApp]
  Supervisor.start_link(children, opts)
end
```

Then run grpc.server using a mix task

```
$ mix grpc.server
```

or start it when starting your application:

```elixir
# config.exs
config :grpc, start_server: true

$ iex -S mix
```

Check [examples](examples) and [interop](interop)(Interoperability Test) for all examples

## TODO

- [x] Unary RPC
- [x] Server streaming RPC
- [x] Client streaming RPC
- [x] Bidirectional streaming RPC
- [x] Helloworld and RouteGuide examples
- [x] Doc and more tests
- [x] Authentication with TLS
- [x] Improve code generation from protos ([protobuf-elixir](https://github.com/tony612/protobuf-elixir) [#8](https://github.com/tony612/grpc-elixir/issues/8))
- [x] Timeout for unary calls
- [x] Errors handling
- [ ] Benchmarking
- [ ] Logging
- [ ] Data compression
- [ ] Support other encoding(other than protobuf)

## Sponsors

This project is being sponsored by [Tubi](https://tubitv.com/). Thank you!

<img src="https://user-images.githubusercontent.com/1253659/37473536-4db44048-28a9-11e8-90d5-f8a2f5a8d53c.jpg" height="80">

## Contributing

You contributions are welcome!

Please open issues if you have questions, problems and ideas. You can create pull
requests directly if you want to fix little bugs, add small features and so on.
But you'd better use issues first if you want to add a big feature or change a
lot of code.
