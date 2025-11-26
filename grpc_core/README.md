# gRPC Core

Core gRPC types, codecs, and utilities for Elixir.

This package provides the foundational components used by both `grpc_server` and `grpc_client`:

- **Codecs**: Protocol Buffers, JSON, WebText, Erlpack
- **Compressors**: Gzip compression support
- **Transport**: HTTP/2 utilities
- **Core Types**: Status codes, errors, credentials, telemetry
- **Protoc Plugin**: Code generation tooling

## Installation

Add `grpc_core` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:grpc_core, "~> 0.12"}
  ]
end
```

## Usage

Most users won't need to depend on `grpc_core` directly. Instead, use:

- `{:grpc_client, "~> 0.12"}` for client-only applications
- `{:grpc_server, "~> 0.12"}` for server-only applications  

## Documentation

For full documentation, see the main [gRPC Elixir](https://hexdocs.pm/grpc) documentation.
def deps do
  [
    {:grpc_core, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/grpc_core>.

