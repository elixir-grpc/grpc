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

  2. Ensure `grpc` is started before your application:

      ```elixir
      def application do
       [applications: [:grpc]]
      end
      ```

## Usage

Generate Elixir code from proto file

```
$ mix do deps.get, compile
$ mix grpc.gen priv/protos/YOUR_SERVICE.proto --out lib/
$ mix grpc.gen.server priv/protos/YOUR_SERVICE.proto --out lib/
```

Implement functions in the generated server template – remember to return the expected message types –,
then run the server and client like this:

```elixir
iex> GRPC.Server.start(Helloworld.Greeter.Server, 50051)
iex> {:ok, channel} = GRPC.Stub.connect("localhost:50051")
iex> request = Helloworld.HelloRequest.new(name: "grpc-elixir")
iex> channel |> Helloworld.Greeter.Stub.say_hello(request)
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

  opts = [strategy: :one_for_one, name: HelloworldApp]
  Supervisor.start_link(children, opts)
end
```

Then run grpc.server:

```
$ mix grpc.server
```

or start it when starting your application:

```elixir
# config.exs
config :grpc, start_server: true

$ iex -S mix
```

Check [examples](examples) for all examples

## TODO

- [x] Unary RPC
- [x] Server streaming RPC
- [x] Client streaming RPC
- [x] Bidirectional streaming RPC
- [x] Helloworld and RouteGuide examples
- [x] Doc and more tests
- [x] Authentication with TLS
- [ ] Improve code generation from protos ([protobuf-elixir](https://github.com/tony612/protobuf-elixir) [#8](https://github.com/tony612/grpc-elixir/issues/8))
- [ ] Improve timeout(now there's simple timeout)
- [ ] Errors handling
- [ ] Data compression
- [ ] Benchmarking
- [ ] Logging

## Contributing

You contributions are welcome!

Please open issues if you have questions, problems and ideas. You can create pull
requests directly if you want to fix little bugs, add small features and so on.
But you'd better use issues first if you want to add a big feature or change a
lot of code.
