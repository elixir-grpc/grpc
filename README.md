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
  - [Effects and Error Handling](#effects-and-error-handling)
    - [Side Effects](#side-effects-with-effect2)
    - [Recovery from errors](#recovery-from-errors)
    - [Unified Error Matching and Propagation](#unified-error-matching-and-propagation)
- [Application Startup](#application-startup)
- [Client Usage](#client-usage)
  - [Basic Connection and RPC](#basic-connection-and-rpc)
  - [Using Interceptors](#using-interceptors)
  - [Target Schemes and Resolvers](#target-schemes-and-resolvers)
    - [Supported formats](#supported-formats)
    - [Example (DNS)](#example-dns)
    - [Example (Unix socket)](#example-unix-socket)
  - [Compression and Metadata](#compression-and-metadata)
  - [Client Adapters](#client-adapters)
    - [Using Mint Adapter](#using-mint-adapter)
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
    {:grpc, "~> 0.11"},
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
  def say_unary_hello(request, materializer) do
    GRPC.Stream.unary(request, materializer: materializer)
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
The Stream API supports composable stream transformations via `ask`, `map`, `run` and others functions, enabling clean and declarative stream pipelines. For a complete list of available operators see [here](lib/grpc/stream.ex).

---

### Effects and Error Handling

#### Side Effects

The `effect/2` operator executes user-defined functions for each element in the stream, allowing the integration of non-transformative actions such as logging, metrics, or external notifications.

Unlike transformation operators (e.g., `map/2`), `effect/2` does not modify or filter values — it preserves the original stream while executing the provided callback safely for each emitted element.

```elixir
iex> parent = self()
iex> stream =
...>   GRPC.Stream.from([1, 2, 3])
...>   |> GRPC.Stream.effect(fn x -> send(parent, {:seen, x * 2}) end)
...>   |> GRPC.Stream.to_flow()
...>   |> Enum.to_list()
iex> assert_receive {:seen, 2}
iex> assert_receive {:seen, 4}
iex> assert_receive {:seen, 6}
iex> stream
[1, 2, 3]
```

Key characteristics:

* The callback function (`effect_fun`) is invoked for each item emitted downstream.
* The result of the callback is ignored, ensuring that the stream’s structure and values remain unchanged.
* Execution is lazy and occurs only when the stream is materialized using run/1, run_with/3, or to_flow/1.
* Exceptions raised inside the callback are captured internally, preventing interruption of the dataflow.

This operator is designed for observability, telemetry, auditing, and integration with external systems that must react to events flowing through the gRPC stream.

---

#### Recovery from errors

The `map_error/2` operator intercepts and transforms errors or exceptions emitted by previous stages in a stream pipeline.

It provides a unified mechanism for handling:

* Expected errors, such as validation or domain failures (`{:error, reason}`)
* Unexpected runtime errors, including raised or thrown exceptions inside other operators.

```elixir
iex> GRPC.Stream.from([1, 2])
...> |> GRPC.Stream.map(fn
...>   2 -> raise "boom"
...>   x -> x
...> end)
...> |> GRPC.Stream.map_error(fn
...>   {:error, {:exception, _reason}} ->
...>     {:error, GRPC.RPCError.exception(message: "Booomm")}
...> end)
``` 

In this example:

* The function inside `map/2` raises an exception for the value `2`.
* map_error/2 captures and transforms that error into a structured `GRPC.RPCError` response.
* The stream continues processing without being interrupted.

This makes map_error/2 suitable for input validation, runtime fault recovery, and user-facing error translation within gRPC pipelines.

---

#### Unified Error Matching and Propagation

All stream operators share a unified error propagation model that guarantees consistent handling of exceptions and failures across the pipeline.

This ensures that user-defined functions within the stream — whether pure transformations, side effects, or external calls — always produce a predictable and recoverable result, maintaining the integrity of the dataflow even in the presence of unexpected errors.

```elixir
def say_unary_hello(request, _materializer) do
  GRPCStream.unary(request)
  |> GRPCStream.ask(Transformer)
  |> GRPCStream.map(fn
    %HelloReply{} = reply ->
      %HelloReply{message: "[Reply] #{reply.message}"}

    {:error, reason} ->
      {:error, GRPC.RPCError.exception(message: "error calling external process: #{inspect(reason)}")}
    
    error ->
      Logger.error("Unknown error")
      error
  end)
  |> GRPCStream.run()
end
```

By normalizing all possible outcomes, `GRPC.Stream` ensures fault-tolerant, exception-safe pipelines where operators can freely raise, throw, or return tuples without breaking the flow execution.

This unified model allows developers to build composable and reliable streaming pipelines that gracefully recover from both domain and runtime errors.

>_NOTE_: In the example above, we could use `map_error/2` instead of `map/2` to handle error cases explicitly. However, since the function also performs a transformation on successful values, `map/2` remains appropriate and useful in this context.

---

## Application Startup

Add the server supervisor to your application's supervision tree:

```elixir
defmodule Helloworld.Application do
  @moduledoc false
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

This section demonstrates how to establish client connections and perform RPC calls using the Elixir gRPC client.

---

## Basic Connection and RPC


Typically, you start this client supervisor as part of your application's supervision tree:

```elixir
children = [
  {GRPC.Client.Supervisor, []}
]

opts = [strategy: :one_for_one, name: MyApp.Supervisor]
Supervisor.start_link(children, opts)
``` 

You can also start it manually in scripts or test environments:
```elixir
{:ok, _pid} = DynamicSupervisor.start_link(strategy: :one_for_one, name: GRPC.Client.Supervisor)
``` 

Then connect with gRPC server:

```elixir
iex> {:ok, channel} = GRPC.Stub.connect("localhost:50051")
iex> request = Helloworld.HelloRequest.new(name: "grpc-elixir")
iex> {:ok, reply} = channel |> Helloworld.GreetingServer.Stub.say_unary_hello(request)
```

---

## Using Interceptors

Client interceptors allow you to add logic to the request/response lifecycle, such as logging, tracing, or authentication.

```elixir
iex> {:ok, channel} =
...>   GRPC.Stub.connect("localhost:50051",
...>     interceptors: [GRPC.Client.Interceptors.Logger]
...>   )
iex> request = Helloworld.HelloRequest.new(name: "Alice")
iex> {:ok, reply} = channel |> Helloworld.GreetingServer.Stub.say_unary_hello(request)
```

---

## Target Schemes and Resolvers

The `connect/2` function supports URI-like targets that are resolved via the internal **gRPC** [Resolver](lib/grpc/client/resolver.ex).  
You can connect using `DNS`, `Unix Domain sockets`, `IPv4/IPv6`, or even `xDS-based endpoints`.

### Supported formats:

| Scheme    | Example                     | Description                                  |
|:----------|:----------------------------|:---------------------------------------------|
| `dns://`  | `"dns://example.com:50051"` | Resolves via DNS `A/AAAA` records            |
| `ipv4:`   | `"ipv4:10.0.0.5:50051"`     | Connects directly to an IPv4 address         |
| `unix:`   | `"unix:/tmp/service.sock"`  | Connects via a Unix domain socket            |
| `xds:///` | `"xds:///my-service"`       | Resolves via xDS control plane (Envoy/Istio) |
| none      | `"127.0.0.1:50051"`         | Implicit DNS (default port `50051`)          |

### Example (DNS):

```elixir
iex> {:ok, channel} = GRPC.Stub.connect("dns://orders.prod.svc.cluster.local:50051")
iex> request = Orders.GetOrderRequest.new(id: "123")
iex> {:ok, reply} = channel |> Orders.OrderService.Stub.get_order(request)
```

### Example (Unix socket):

```elixir
iex> {:ok, channel} = GRPC.Stub.connect("unix:/tmp/my.sock")
```

>__NOTE__: When using `DNS` or `xDS` targets, the connection layer periodically refreshes endpoints.
---

## Compression and Metadata

You can specify message compression and attach default headers to all requests.

```elixir
iex> {:ok, channel} =
...>   GRPC.Stub.connect("localhost:50051",
...>     compressor: GRPC.Compressor.Gzip,
...>     headers: [{"authorization", "Bearer my-token"}]
...>   )
```

---

## Client Adapters

By default, `GRPC.Stub.connect/2` uses the **Gun** adapter.  
You can switch to **Mint** (pure Elixir HTTP/2) or other adapters as needed.

### Using Mint Adapter

```elixir
iex> GRPC.Stub.connect("localhost:50051",
...>   adapter: GRPC.Client.Adapters.Mint
...> )
```

You can configure adapter options globally via your application’s config:

```elixir
# File: config/config.exs
config :grpc, GRPC.Client.Adapters.Mint,
  timeout: 10_000,
  transport_opts: [cacertfile: "/etc/ssl/certs/ca-certificates.crt"]
```

The accepted options are the same as [`Mint.HTTP.connect/4`](https://hexdocs.pm/mint/Mint.HTTP.html#connect/4-options).

---

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
    {:grpc, "~> 0.11"},
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
