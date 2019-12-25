# gRPC Elixir

[![Hex.pm](https://img.shields.io/hexpm/v/grpc.svg)](https://hex.pm/packages/grpc)
[![Travis Status](https://travis-ci.org/elixir-grpc/grpc.svg?branch=master)](https://travis-ci.org/elixir-grpc/grpc)
[![GitHub actions Status](https://github.com/elixir-grpc/grpc/workflows/CI/badge.svg)](https://github.com/elixir-grpc/grpc/actions)
[![Inline docs](http://inch-ci.org/github/elixir-grpc/grpc.svg?branch=master)](http://inch-ci.org/github/elixir-grpc/grpc)

An Elixir implementation of [gRPC](http://www.grpc.io/).

**WARNING: Be careful to use it in production! Test and benchmark in advance.**

**NOTICE: Erlang/OTP needs >= 20.3.2**

## Installation

The package can be installed as:

  ```elixir
  def deps do
    [
      {:grpc, github: "elixir-grpc/grpc"},
      # This may be needed when grpc_{cowlib/gun/cowboy} packages on hex are used, see mix.exs for
      # details.
      # Also see below explain.
      {:cowlib, "~> 2.8.0", hex: :grpc_cowlib, override: true},
    ]
  end
  ```

**NOTICE: grpc_gun & grpc_cowlib may be used**

grpc rely on them heavily but I can't control the release of gun and cowlib. I don't want to always
wait for their releases epecially when their HTTP/2 support are not very stable. And hex doesn't allow
dependencies are GitHub, so I published my own versions to hex so that grpc can be published.

My current releases don't have big differences with the upstream ones. And I think this hack will only be
used in the near future because cowlib, gun and cowboy are becoming more and more stable.

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

3. Start the server

You can start the gRPC server as a supervised process. First, add `GRPC.Server.Supervisor` to your supervision tree.

```elixir
# Define your endpoint
defmodule Helloworld.Endpoint do
  use GRPC.Endpoint

  intercept GRPC.Logger.Server
  run Helloworld.Greeter.Server
end

# In the start function of your Application
defmodule HelloworldApp do
  use Application
  def start(_type, _args) do
    children = [
      # ...
      supervisor(GRPC.Server.Supervisor, [{Helloworld.Endpoint, 50051}])
    ]

    opts = [strategy: :one_for_one, name: YourApp]
    Supervisor.start_link(children, opts)
  end
end
```

Then start it when starting your application:

```elixir
# config.exs
config :grpc, start_server: true

# test.exs
config :grpc, start_server: false

$ iex -S mix
```

or run grpc.server using a mix task

```
$ mix grpc.server
```

4. Call rpc:
```elixir
iex> {:ok, channel} = GRPC.Stub.connect("localhost:50051")
iex> request = Helloworld.HelloRequest.new(name: "grpc-elixir")
iex> {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(request)

# With interceptors
iex> {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [GRPC.Logger.Client])
...
```

Check [examples](examples) and [interop](interop)(Interoperability Test) for some examples.

## TODO

- [x] Unary RPC
- [x] Server streaming RPC
- [x] Client streaming RPC
- [x] Bidirectional streaming RPC
- [x] Helloworld and RouteGuide examples
- [x] Doc and more tests
- [x] Authentication with TLS
- [x] Improve code generation from protos ([protobuf-elixir](https://github.com/tony612/protobuf-elixir) [#8](https://github.com/elixir-grpc/grpc/issues/8))
- [x] Timeout for unary calls
- [x] Errors handling
- [x] Benchmarking
- [x] Logging
- [x] Interceptors(See `GRPC.Endpoint`)
- [x] [Connection Backoff](https://github.com/grpc/grpc/blob/master/doc/connection-backoff.md)
- [x] Data compression
- [x] Support other encoding(other than protobuf)

## Benchmark

1. [Simple benchmark](examples/helloworld/README.md#Benchmark) by using [ghz](https://ghz.sh/)

2. [Benchmark](benchmark) followed by official spec

## Sponsors

This project is being sponsored by [Tubi](https://tubitv.com/). Thank you!

<img src="https://user-images.githubusercontent.com/1253659/37473536-4db44048-28a9-11e8-90d5-f8a2f5a8d53c.jpg" height="80">

## Contributing

You contributions are welcome!

Please open issues if you have questions, problems and ideas. You can create pull
requests directly if you want to fix little bugs, add small features and so on.
But you'd better use issues first if you want to add a big feature or change a
lot of code.
