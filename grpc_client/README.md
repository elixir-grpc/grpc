# gRPC Elixir Client

[![GitHub CI](https://github.com/elixir-grpc/grpc/actions/workflows/ci.yml/badge.svg)](https://github.com/elixir-grpc/grpc/actions/workflows/ci.yml)
[![Hex.pm](https://img.shields.io/hexpm/v/grpc.svg)](https://hex.pm/packages/grpc)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/grpc/)
[![License](https://img.shields.io/hexpm/l/grpc.svg)](https://github.com/elixir-grpc/grpc/blob/master/LICENSE)
[![Total Downloads](https://img.shields.io/hexpm/dt/grpc.svg)](https://hex.pm/packages/grpc)
[![Last Updated](https://img.shields.io/github/last-commit/elixir-grpc/grpc.svg)](https://github.com/elixir-grpc/grpc/commits/master)

**gRPC Elixir client** is a full-featured Elixir implementation of the [gRPC](https://grpc.io) protocol, supporting unary and streaming RPCs, interceptors, and TLS. This package adopts a unified stream-based model for all types of calls.

## Table of contents

- [Installation](#installation)
- [Protobuf Code Generation](#protobuf-code-generation)
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
- [Contributing](#contributing)

## Installation

The package can be installed as:

```elixir
def deps do
  [
    {:grpc_client, "~> 0.11"},
    {:protobuf, "~> 0.14"}, # optional for import wellknown google types
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

## Contributing

Your contributions are welcome!

Please open issues if you have questions, problems and ideas. You can create pull
requests directly if you want to fix little bugs, add small features and so on.
But you'd better use issues first if you want to add a big feature or change a
lot of code.
