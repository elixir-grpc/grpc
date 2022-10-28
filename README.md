# gRPC Elixir

[![Hex.pm](https://img.shields.io/hexpm/v/grpc.svg)](https://hex.pm/packages/grpc)
[![Travis Status](https://app.travis-ci.com/elixir-grpc/grpc.svg?branch=master)](https://app.travis-ci.com/elixir-grpc/grpc)
[![GitHub actions Status](https://github.com/elixir-grpc/grpc/workflows/CI/badge.svg)](https://github.com/elixir-grpc/grpc/actions)
[![License](https://img.shields.io/hexpm/l/grpc.svg)](https://github.com/elixir-grpc/grpc/blob/master/LICENSE.md)
[![Last Updated](https://img.shields.io/github/last-commit/elixir-grpc/grpc.svg)](https://github.com/elixir-grpc/grpc/commits/master)
[![Total Download](https://img.shields.io/hexpm/dt/grpc.svg)](https://hex.pm/packages/elixir-grpc/grpc)

An Elixir implementation of [gRPC](http://www.grpc.io/).

## Table of contents

- [Notice](#notice)
- [Installation](#installation)
- [Usage](#usage)
- [Features](#features)
- [Benchmark](#benchmark)
- [Contributing](#contributing)

## Notice
> __Note__
> The [Gun](https://github.com/ninenines/gun) library doesn't have a full 2.0 release yet, so we depend on `:grcp_gun 2.0.1` for now.
This is the same as `:gun 2.0.0-rc.2`, but [Hex](https://hex.pm/) doesn't let us depend on RC versions for releases.

## Installation

The package can be installed as:

  ```elixir
  def deps do
    [
      {:grpc, "~> 0.5.0"},
      # We don't force protobuf as a dependency for more
      # flexibility on which protobuf library is used,
      # but you probably want to use it as well
      {:protobuf, "~> 0.10"}
    ]
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
      {GRPC.Server.Supervisor, endpoint: Helloworld.Endpoint, port: 50051}
    ]

    opts = [strategy: :one_for_one, name: YourApp]
    Supervisor.start_link(children, opts)
  end
end
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

## Features

- Various kinds of RPC:
  - [Unary](https://grpc.io/docs/what-is-grpc/core-concepts/#unary-rpc)
  - [Server-streaming](https://grpc.io/docs/what-is-grpc/core-concepts/#server-streaming-rpc)
  - [Client-streaming](https://grpc.io/docs/what-is-grpc/core-concepts/#client-streaming-rpc)
  - [Bidirectional-streaming](https://grpc.io/docs/what-is-grpc/core-concepts/#bidirectional-streaming-rpc)
- [TLS Authentication](https://grpc.io/docs/guides/auth/#supported-auth-mechanisms)
- [Error handling](https://grpc.io/docs/guides/error/)
- Interceptors(See [`GRPC.Endpoint`](https://github.com/elixir-grpc/grpc/blob/master/lib/grpc/endpoint.ex))
- [Connection Backoff](https://github.com/grpc/grpc/blob/master/doc/connection-backoff.md)
- Data compression

## Benchmark

1. [Simple benchmark](examples/helloworld/README.md#Benchmark) by using [ghz](https://ghz.sh/)

2. [Benchmark](benchmark) followed by official spec

## Contributing

Your contributions are welcome!

Please open issues if you have questions, problems and ideas. You can create pull
requests directly if you want to fix little bugs, add small features and so on.
But you'd better use issues first if you want to add a big feature or change a
lot of code.
