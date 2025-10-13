# gRPC Elixir

[![GitHub CI](https://github.com/elixir-grpc/grpc/actions/workflows/ci.yml/badge.svg)](https://github.com/elixir-grpc/grpc/actions/workflows/ci.yml)
[![Hex.pm](https://img.shields.io/hexpm/v/grpc.svg)](https://hex.pm/packages/grpc)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/grpc/)
[![License](https://img.shields.io/hexpm/l/grpc.svg)](https://github.com/elixir-grpc/grpc/blob/master/LICENSE)
[![Total Downloads](https://img.shields.io/hexpm/dt/grpc.svg)](https://hex.pm/packages/grpc)
[![Last Updated](https://img.shields.io/github/last-commit/elixir-grpc/grpc.svg)](https://github.com/elixir-grpc/grpc/commits/master)

**gRPC Elixir** is a full-featured Elixir implementation of the [gRPC](https://grpc.io) protocol, supporting unary and streaming RPCs, interceptors, HTTP transcoding, and TLS. This version adopts a unified stream-based model for all types of calls.

## Table of contents

- [Installation](#installation)
- [Protobuf Code Generation](#protobuf-code-generation)
- [Server Implementation](#server-implementation)
  - [Unary RPC using Stream API](#unary-rpc-using-stream-api)
  - [Server-Side Streaming](#server-side-streaming)
  - [Bidirectional Streaming](#bidirectional-streaming)
- [Application Startup](#application-startup)
- [Client Usage](#client-usage)
- [HTTP Transcoding](#http-transcoding)
- [CORS](#cors)
- [Features](#features)
- [Benchmark](#benchmark)
- [Contributing](#contributing)

## Installation

The package can be installed as:

```elixir
def deps do
  [
    {:grpc, "~> 0.10"},
    {:protobuf, "~> 0.14"}, # optional for import wellknown google types
    {:grpc_reflection, "~> 0.2"} # optional enable grpc reflection
  ]
end
```

## Protobuf Code Generation

Use `protoc` with [protobuf elixir plugin](https://github.com/elixir-protobuf/protobuf) or using [protobuf_generate](https://hexdocs.pm/protobuf_generate/readme.html) hex package to generate the necessary files.

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
service GreetingServer {
  rpc SayUnaryHello (HelloRequest) returns (HelloReply) {}
  rpc SayServerHello (HelloRequest) returns (stream HelloReply) {}
  rpc SayBidStreamHello (stream HelloRequest) returns (stream HelloReply) {}
}
```

2. Compile protos (protoc + elixir plugin):

```bash
protoc --elixir_out=plugins=grpc:./lib -I./priv/protos helloworld.proto
```

## Server Implementation

All RPC calls must be implemented using the stream-based API, even for unary requests.

>__NOTE__: The old API was deprecated based on `GRPC.Server.send_reply/2` and direct `struct` returns was deprecated as of version `0.10.x`.

### Unary RPC using Stream API

```elixir
defmodule HelloworldStreams.Server do
  use GRPC.Server, service: Helloworld.GreetingServer.Service

  alias GRPC.Stream

  alias Helloworld.HelloRequest
  alias Helloworld.HelloReply

  @spec say_unary_hello(HelloRequest.t(), GRPC.Server.Stream.t()) :: any()
  def say_unary_hello(request, _materializer) do
    GRPC.Stream.unary(request)
    |> GRPC.Stream.map(fn %HelloReply{} = reply ->
      %HelloReply{message: "[Reply] #{reply.message}"}
    end)
    |> GRPC.Stream.run()
  end
end
```

### Server-Side Streaming

```elixir
def say_server_hello(request, materializer) do
  Stream.repeatedly(fn ->
    index = :rand.uniform(10)
    %HelloReply{message: "[#{index}] Hello #{request.name}"}
  end)
  |> Stream.take(10)
  |> GRPC.Stream.from()
  |> GRPC.Stream.run_with(materializer)
end
```

### Bidirectional Streaming

```elixir
@spec say_bid_stream_hello(Enumerable.t(), GRPC.Server.Stream.t()) :: any()
def say_bid_stream_hello(request, materializer) do
  output_stream =
    Stream.repeatedly(fn ->
      index = :rand.uniform(10)
      %HelloReply{message: "[#{index}] Server response"}
    end)

  GRPC.Stream.from(request, join_with: output_stream)
  |> GRPC.Stream.map(fn
    %HelloRequest{name: name} -> %HelloReply{message: "Welcome #{name}"}
    other -> other
  end)
  |> GRPC.Stream.run_with(materializer)
end
```
The Stream API supports composable stream transformations via `ask`, `map`, `run` and others functions, enabling clean and declarative stream pipelines. See the table below:

| Function                         | Description  | Parameters / Options  |
|:---------------------------------|:-------------|:----------------------|
| **`from(input, opts \\\\ [])`**  | Converts a gRPC stream (or list) into a `Flow` with backpressure support. Allows joining with external `GenStage` producers. | **Parameters:**<br>• `input` — stream, list, or gRPC struct.<br>**Options:**<br>• `:join_with` — PID or name of an external `GenStage` producer.<br>• `:dispatcher` — dispatcher module (default: `GenStage.DemandDispatcher`).<br>• `:propagate_context` — if `true`, propagates the materializer context.<br>• `:materializer` — the current `%GRPC.Server.Stream{}`.<br>• Other options supported by `Flow`. |
| **`unary(input, opts \\\\ [])`** | Creates a `Flow` from a single gRPC request (unary). Useful for non-streaming calls that still leverage the Flow API. | **Parameters:**<br>• `input` — single gRPC message.<br>**Options:** same as `from/2`. |
| **`to_flow(stream)`**            | Returns the underlying `Flow` from a `GRPC.Stream`. If uninitialized, returns `Flow.from_enumerable([])`. | **Parameters:**<br>• `stream` — `%GRPC.Stream{}` struct. |
| **`run(stream)`**                | Executes the `Flow` for a unary stream and returns the first materialized result. | **Parameters:**<br>• `stream` — `%GRPC.Stream{}` with `unary: true` option. |
| **`run_with(stream, materializer, opts \\\\ [])`** | Executes the `Flow` and sends responses into the gRPC server stream. Supports `:dry_run` for test mode without sending messages. | **Parameters:**<br>• `stream` — `%GRPC.Stream{}`.<br>• `materializer` — `%GRPC.Server.Stream{}`.<br>**Options:**<br>• `:dry_run` — if `true`, responses are not sent. |
| **`ask(stream, target, timeout \\\\ 5000)`** | Sends a request to an external process (`PID` or named process) and waits for a response (`{:response, msg}`). Returns an updated stream or an error. | **Parameters:**<br>• `stream` — `%GRPC.Stream{}`.<br>• `target` — PID or atom.<br>• `timeout` — in milliseconds. |
| **`ask!(stream, target, timeout \\\\ 5000)`** | Same as `ask/3`, but raises an exception on failure (aborts the Flow). | Same parameters as `ask/3`. |
| **`filter(stream, fun)`** | Filters items in the stream by applying a concurrent predicate function. | **Parameters:**<br>• `stream` — `%GRPC.Stream{}`.<br>• `fun` — function `(item -> boolean)`. |
| **`flat_map(stream, fun)`** | Applies a function returning a list or enumerable, flattening the results. | **Parameters:**<br>• `stream` — `%GRPC.Stream{}`.<br>• `fun` — `(item -> Enumerable.t())`. |
| **`map(stream, fun)`** | Applies a transformation function to each item in the stream. | **Parameters:**<br>• `stream` — `%GRPC.Stream{}`.<br>• `fun` — `(item -> term)`. |
| **`map_with_context(stream, fun)`** | Applies a function to each item, passing the stream context (e.g., headers) as an additional argument. | **Parameters:**<br>• `stream` — `%GRPC.Stream{}`.<br>• `fun` — `(context, item -> term)`. |
| **`partition(stream, opts \\\\ [])`** | Partitions the stream to group items by key or condition before stateful operations like `reduce/3`. | **Parameters:**<br>• `stream` — `%GRPC.Stream{}`.<br>• `opts` — partitioning options (`Flow.partition/2`). |
| **`reduce(stream, acc_fun, reducer_fun)`** | Reduces the stream using an accumulator, useful for aggregations. | **Parameters:**<br>• `stream` — `%GRPC.Stream{}`.<br>• `acc_fun` — initializer function `() -> acc`.<br>• `reducer_fun` — `(item, acc -> acc)`. |
| **`uniq(stream)`** | Emits only distinct items from the stream (no custom uniqueness criteria). | **Parameters:**<br>• `stream` — `%GRPC.Stream{}`. |
| **`uniq_by(stream, fun)`** | Emits only unique items based on the return value of the provided function. | **Parameters:**<br>• `stream` — `%GRPC.Stream{}`.<br>• `fun` — `(item -> term)` for uniqueness determination. |
| **`get_headers(stream)`** | Retrieves HTTP/2 headers from a `%GRPC.Server.Stream{}`. | **Parameters:**<br>• `stream` — `%GRPC.Server.Stream{}`.<br>**Returns:** `map` containing decoded headers. |

For a complete list of available operators see [here](lib/grpc/stream.ex).

---

## Application Startup

Add the server supervisor to your application's supervision tree:

```elixir
defmodule Helloworld.Application do
  @ false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      GrpcReflection,
      {
        GRPC.Server.Supervisor, [
          endpoint: Helloworld.Endpoint,
          port: 50051,
          start_server: true,
          # adapter_opts: [# any adapter-specific options like tls configuration....]
        ]
      }
    ]

    opts = [strategy: :one_for_one, name: Helloworld.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

# Client Usage

```elixir
iex> {:ok, channel} = GRPC.Stub.connect("localhost:50051")
iex> request = Helloworld.HelloRequest.new(name: "grpc-elixir")
iex> {:ok, reply} = channel |> Helloworld.GreetingServer.Stub.say_unary_hello(request)

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
    {:grpc, "~> 0.10"},
    {:protobuf_generate, "~> 0.1.3"}
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

  # callback implementations...
end
```

See full application code in [helloworld_transcoding](examples/helloworld_transcoding) example.

### **CORS**

When accessing gRPC from a browser via HTTP transcoding or gRPC-Web, CORS headers may be required for the browser to allow access to the gRPC endpoint. Adding CORS headers can be done by using `GRPC.Server.Interceptors.CORS` as an interceptor in your `GRPC.Endpoint` module, configuring it as described in the module documentation:

Example:

```elixir
# Define your endpoint
defmodule Helloworld.Endpoint do
  use GRPC.Endpoint

  intercept GRPC.Server.Interceptors.Logger
  intercept GRPC.Server.Interceptors.CORS, allow_origin: "mydomain.io"
  run Helloworld.Greeter.Server
end
```

## Features

- Various kinds of RPC:
  - [Unary](https://grpc.io/docs/what-is-grpc/core-concepts/#unary-rpc)
  - [Server-streaming](https://grpc.io/docs/what-is-grpc/core-concepts/#server-streaming-rpc)
  - [Client-streaming](https://grpc.io/docs/what-is-grpc/core-concepts/#client-streaming-rpc)
  - [Bidirectional-streaming](https://grpc.io/docs/what-is-grpc/core-concepts/#bidirectional-streaming-rpc)
- [HTTP Transcoding](https://cloud.google.com/endpoints/docs/grpc/transcoding)
- [TLS Authentication](https://grpc.io/docs/guides/auth/#supported-auth-mechanisms)
- [Error Handling](https://grpc.io/docs/guides/error/)
- [Interceptors](https://grpc.io/docs/guides/interceptors/)
- [Connection Backoff](https://github.com/grpc/grpc/blob/master/doc/connection-backoff.md)
- [Data Compression](https://grpc.io/docs/guides/compression/)
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
