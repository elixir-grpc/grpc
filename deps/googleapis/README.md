# Googleapis for Elixir

[![Hex.pm](https://img.shields.io/hexpm/v/googleapis.svg)](https://hex.pm/packages/googleapis)
[![Build Status](https://github.com/elixir-grpc/googleapis/actions/workflows/ci.yml/badge.svg)](https://github.com/elixir-grpc/googleapis/actions)
[![License](https://img.shields.io/badge/license-Apache%202-blue.svg)](LICENSE)

The **Googleapis for Elixir** project provides generated Elixir modules for Google APIs, built from official [Google Protocol Buffers definitions](https://github.com/googleapis/googleapis).  
It is designed to be used in conjunction with [Elixir gRPC](https://github.com/elixir-grpc/grpc) and [`protobuf`](https://hex.pm/packages/protobuf), enabling seamless interaction with Google services through idiomatic Elixir code.

---

## Features

- Generated Elixir modules for multiple Google APIs  
- Fully compatible with the [`protobuf`](https://hex.pm/packages/protobuf) Elixir library  
- Ideal for use with [`grpc`](https://hex.pm/packages/grpc) clients and servers  
- Maintained as part of the [Elixir gRPC](https://github.com/elixir-grpc/grpc) ecosystem  
- Licensed under Apache 2.0

---

## Installation

Add `googleapis` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:googleapis, "~> 0.1.0"}
  ]
end
``` 

## Usage

This package contains auto-generated modules based on Google's .proto definitions.
You can use these modules directly to encode/decode protobuf messages or to interact with Google APIs using gRPC.

Example (for illustrative purposes only):

```elixir
alias Google.Rpc.Status

# Create a new Status struct
status = %Status{
  code: 404,
  message: "Resource not found",
  details: []
}

# Encode the struct into a protobuf binary
encoded = Status.encode(status)

# Decode the binary back into a struct
decoded = Status.decode(encoded)

IO.inspect(decoded, label: "Decoded Status")
```

> The `Status` message is part of Google’s standard RPC error model.
It defines a logical error schema that is used by gRPC and REST APIs, containing an error code, message, and optional structured details.
More information: [Google API Design Guide](https://google.aip.dev/193)

## Dependencies

This package depends on:

* protobuf
 — for Protocol Buffers encoding/decoding

* ex_doc
 — for documentation generation (development only)