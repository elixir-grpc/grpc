# gRPC Elixir

[![GitHub CI](https://github.com/elixir-grpc/grpc/actions/workflows/ci.yml/badge.svg)](https://github.com/elixir-grpc/grpc/actions/workflows/ci.yml)
[![Hex.pm](https://img.shields.io/hexpm/v/grpc.svg)](https://hex.pm/packages/grpc)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/grpc/)
[![License](https://img.shields.io/hexpm/l/grpc.svg)](https://github.com/elixir-grpc/grpc/blob/master/LICENSE.md)
[![Total Download](https://img.shields.io/hexpm/dt/grpc.svg)](https://hex.pm/packages/elixir-grpc/grpc)
[![Last Updated](https://img.shields.io/github/last-commit/elixir-grpc/grpc.svg)](https://github.com/elixir-grpc/grpc/commits/master)

An Elixir implementation of [gRPC](http://www.grpc.io/).

## Table of contents

- [Installation](#installation)
- [Usage](#usage)
  - [Simple RPC](#simple-rpc)
  - [HTTP Transcoding](#http-transcoding)
  - [Start Application](#start-application)
- [Features](#features)
- [Benchmark](#benchmark)
- [Contributing](#contributing)

## Installation

The package can be installed as:

  ```elixir
  def deps do
    [
      {:grpc, "~> 0.9"}
    ]
  end
  ```

## Usage

1. Write your protobuf file:

```protobuf
syntax = "proto3";

package helloworld;

// The request message containing the user's name.
message HelloRequest {
  string name = 1;
}

// The response message containing the greeting
message HelloReply {
  string message = 1;
}

// The greeting service definition.
service Greeter {
  // Greeting function
  rpc SayHello (HelloRequest) returns (HelloReply) {}
}

```

2. Then generate Elixir code from proto file as [protobuf-elixir](https://github.com/tony612/protobuf-elixir#usage) shows (especially the `gRPC Support` section) or using [protobuf_generate](https://hex.pm/packages/protobuf_generate) hex package. Example using `protobuf_generate` lib:

```shell
mix protobuf.generate --output-path=./lib --include-path=./priv/protos helloworld.proto
```

In the following sections you will see how to implement gRPC server logic.

### **Simple RPC**

1. Implement the server side code like below and remember to return the expected message types.

```elixir
defmodule Helloworld.Greeter.Server do
  use GRPC.Server, service: Helloworld.Greeter.Service

  @spec say_hello(Helloworld.HelloRequest.t, GRPC.Server.Stream.t) :: Helloworld.HelloReply.t
  def say_hello(request, _stream) do
    Helloworld.HelloReply.new(message: "Hello #{request.name}")
  end
end
```

2. Define gRPC endpoints

```elixir
# Define your endpoint
defmodule Helloworld.Endpoint do
  use GRPC.Endpoint

  intercept GRPC.Server.Interceptors.Logger
  run Helloworld.Greeter.Server
end
```

We will use this module [in the gRPC server startup section](#start-application).

**__Note:__** For other types of RPC call like streams see [here](interop/lib/interop/server.ex).

### **HTTP Transcoding**

1. Adding [grpc-gateway annotations](https://cloud.google.com/endpoints/docs/grpc/transcoding) to your protobuf file definition:

```protobuf
import "google/api/annotations.proto";
import "google/protobuf/timestamp.proto";

package helloworld;

// The greeting service definition.
service Greeter {
  // Sends a greeting
  rpc SayHello (HelloRequest) returns (HelloReply) {
    option (google.api.http) = {
      get: "/v1/greeter/{name}"
    };
  }

  rpc SayHelloFrom (HelloRequestFrom) returns (HelloReply) {
    option (google.api.http) = {
      post: "/v1/greeter"
      body: "*"
    };
  }
}
```

2. Add protoc plugin dependency and compile your protos using [protobuf_generate](https://github.com/drowzy/protobuf_generate) hex [package](https://hex.pm/packages/protobuf_generate):

In mix.exs:

```elixir
def deps do
  [
    {:grpc, "~> 0.7"},
    {:protobuf_generate, "~> 0.1.1"}
  ]
end
```

And in your terminal:

```shell
mix protobuf.generate \
  --include-path=priv/proto \
  --include-path=deps/googleapis \
  --generate-descriptors=true \
  --output-path=./lib \
  --plugins=ProtobufGenerate.Plugins.GRPCWithOptions \
  google/api/annotations.proto google/api/http.proto helloworld.proto
```

3. Enable http_transcode option in your Server module
```elixir
defmodule Helloworld.Greeter.Server do
  use GRPC.Server,
    service: Helloworld.Greeter.Service,
    http_transcode: true

  @spec say_hello(Helloworld.HelloRequest.t, GRPC.Server.Stream.t) :: Helloworld.HelloReply.t
  def say_hello(request, _stream) do
    %Helloworld.HelloReply{message: "Hello #{request.name}"}
  end
end
```

See full application code in [helloworld_transcoding](examples/helloworld_transcoding) example.

### **Start Application**

1. Start gRPC Server in your supervisor tree or Application module:

```elixir
# In the start function of your Application
defmodule HelloworldApp do
  use Application
  def start(_type, _args) do
    children = [
      # ...
      {GRPC.Server.Supervisor, endpoint: Helloworld.Endpoint, port: 50051, start_server: true}
    ]

    opts = [strategy: :one_for_one, name: YourApp]
    Supervisor.start_link(children, opts)
  end
end
```

2. Call rpc:

```elixir
iex> {:ok, channel} = GRPC.Stub.connect("localhost:50051")
iex> request = Helloworld.HelloRequest.new(name: "grpc-elixir")
iex> {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(request)

# With interceptors
iex> {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [GRPC.Client.Interceptors.Logger])
...
```

Check the [examples](examples) and [interop](interop) directories in the project's source code for some examples.

## Client Adapter and Configuration

The default adapter used by `GRPC.Stub.connect/2` is `GRPC.Client.Adapter.Gun`. Another option is to use `GRPC.Client.Adapters.Mint` instead, like so:

```elixir
GRPC.Stub.connect("localhost:50051",
  # Use Mint adapter instead of default Gun
  adapter: GRPC.Client.Adapters.Mint
)
```

The `GRPC.Client.Adapters.Mint` adapter accepts custom configuration. To do so, you can configure it from your mix application via:

```elixir
# File: your application's config file.
config :grpc, GRPC.Client.Adapters.Mint, custom_opts
```

The accepted options for configuration are the ones listed on [Mint.HTTP.connect/4](https://hexdocs.pm/mint/Mint.HTTP.html#connect/4-options)

## Features

- Various kinds of RPC:
  - [Unary](https://grpc.io/docs/what-is-grpc/core-concepts/#unary-rpc)
  - [Server-streaming](https://grpc.io/docs/what-is-grpc/core-concepts/#server-streaming-rpc)
  - [Client-streaming](https://grpc.io/docs/what-is-grpc/core-concepts/#client-streaming-rpc)
  - [Bidirectional-streaming](https://grpc.io/docs/what-is-grpc/core-concepts/#bidirectional-streaming-rpc)
- [HTTP Transcoding](https://cloud.google.com/endpoints/docs/grpc/transcoding)
- [TLS Authentication](https://grpc.io/docs/guides/auth/#supported-auth-mechanisms)
- [Error handling](https://grpc.io/docs/guides/error/)
- Interceptors (See [`GRPC.Endpoint`](https://github.com/elixir-grpc/grpc/blob/master/lib/grpc/endpoint.ex))
- [Connection Backoff](https://github.com/grpc/grpc/blob/master/doc/connection-backoff.md)
- Data compression
- [gRPC Reflection](https://github.com/elixir-grpc/grpc-reflection)

## Benchmark

1. [Simple benchmark](examples/helloworld/README.md#Benchmark) by using [ghz](https://ghz.sh/)

2. [Benchmark](benchmark) followed by official spec

## Contributing

Your contributions are welcome!

Please open issues if you have questions, problems and ideas. You can create pull
requests directly if you want to fix little bugs, add small features and so on.
But you'd better use issues first if you want to add a big feature or change a
lot of code.
