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

```shell
$ mix do deps.get, compile
$ mix grpc.gen priv/protos/helloworld.proto --out lib/
```

Define your server, then run the server and client.

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
