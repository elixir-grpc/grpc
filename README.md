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

Implement functions in the generated server template, then run the server
and client like this:

```elixir
iex> GRPC.Server.start(Helloworld.Greeter.Server, 50051)
iex> {:ok, channel} = GRPC.Stub.connect("localhost:50051")
iex> request = Helloworld.HelloRequest.new(name: "grpc-elixir")
iex> channel |> Greeter.Stub.say_hello(request)
```

Check [examples](examples) for all examples

## TODO

- [x] Unary RPC
- [x] Server streaming RPC
- [x] Client streaming RPC
- [x] Bidirectional streaming RPC
- [x] Helloworld and RouteGuide examples
- [x] Doc and more tests
- [ ] Authentication
- [ ] Improve timeout(now there's simple timeout)
- [ ] Errors handling
- [ ] Data compression
- [ ] Benchmarking

## Contributing

You contributions are welcome!

Please open issues if you have questions, problems and ideas. You can create pull
requests directly if you want to fix little bugs, add small features and so on.
But you'd better use issues first if you want to add a big feature or change a
lot of code.
