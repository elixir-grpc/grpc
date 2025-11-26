# gRPC Client

gRPC client implementation for Elixir.

This package provides everything you need to build gRPC clients in Elixir:

- **Connection Management**: Channel and connection pooling
- **Load Balancing**: Round-robin and pick-first strategies
- **Name Resolution**: DNS, IPv4, IPv6, and Unix socket support
- **Adapters**: Gun and Mint HTTP/2 clients
- **Interceptors**: Request/response middleware
- **TLS Support**: Secure connections with custom certificates

## Installation

Add `grpc_client` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:grpc_client, "~> 0.12"}
  ]
end
```

## Quick Start

```elixir
# Connect to a gRPC server
{:ok, channel} = GRPC.Stub.connect("localhost:50051")

# Make a unary call
{:ok, reply} = MyApp.Greeter.Stub.say_hello(channel, request)

# Disconnect
GRPC.Stub.disconnect(channel)
```

## Documentation

For full documentation and examples, see:
- [Client Guide](https://hexdocs.pm/grpc/guides/getting_started/client.html)
- [API Documentation](https://hexdocs.pm/grpc_client)
def deps do
  [
    {:grpc_client, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/grpc_client>.

