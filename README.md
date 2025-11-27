# gRPC Elixir

[![GitHub CI](https://github.com/surgeventures/grpc/actions/workflows/ci.yml/badge.svg)](https://github.com/surgeventures/grpc/actions/workflows/ci.yml)

Fresha fork of [elixir-grpc/grpc](https://github.com/elixir-grpc/grpc) - an Elixir implementation of [gRPC](http://www.grpc.io/).

**Requires: Elixir >= 1.15, OTP >= 24**

## Installation

```elixir
def deps do
  [
    {:grpc, "~> 0.10", hex: :grpc_fresha}
  ]
end
```

## Usage

1. Write your protobuf file:

```protobuf
syntax = "proto3";

package helloworld;

message HelloRequest {
  string name = 1;
}

message HelloReply {
  string message = 1;
}

service Greeter {
  rpc SayHello (HelloRequest) returns (HelloReply) {}
}
```

2. Generate Elixir code from proto file using [protobuf-elixir](https://github.com/elixir-protobuf/protobuf#usage):

```shell
protoc --elixir_out=plugins=grpc:./lib -I./priv/protos helloworld.proto
```

3. Implement the server:

```elixir
defmodule Helloworld.Greeter.Server do
  use GRPC.Server, service: Helloworld.Greeter.Service

  @spec say_hello(Helloworld.HelloRequest.t, GRPC.Server.Stream.t) :: Helloworld.HelloReply.t
  def say_hello(request, _stream) do
    Helloworld.HelloReply.new(message: "Hello #{request.name}")
  end
end
```

4. Define endpoint:

```elixir
defmodule Helloworld.Endpoint do
  use GRPC.Endpoint

  intercept GRPC.Server.Interceptors.Logger
  run Helloworld.Greeter.Server
end
```

5. Start server in your supervision tree:

```elixir
defmodule HelloworldApp do
  use Application

  def start(_type, _args) do
    children = [
      {GRPC.Server.Supervisor, endpoint: Helloworld.Endpoint, port: 50051, start_server: true}
    ]
    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
```

6. Call RPC:

```elixir
{:ok, channel} = GRPC.Stub.connect("localhost:50051")
request = Helloworld.HelloRequest.new(name: "grpc-elixir")
{:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(request)

# With interceptors
{:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [GRPC.Client.Interceptors.Logger])
```

## Client Adapters

Default adapter is `GRPC.Client.Adapters.Gun`. You can use Mint instead:

```elixir
GRPC.Stub.connect("localhost:50051", adapter: GRPC.Client.Adapters.Mint)
```

Configure Mint adapter options:

```elixir
config :grpc, GRPC.Client.Adapters.Mint,
  timeout: 10_000,
  transport_opts: [cacertfile: "/path/to/ca.crt"]
```

## HTTP Transcoding

Enable transcoding for HTTP/JSON access to gRPC methods:

```elixir
defmodule Helloworld.Greeter.Server do
  use GRPC.Server,
    service: Helloworld.Greeter.Service,
    http_transcode: true
  # ...
end
```

See [examples/helloworld_transcoding](examples/helloworld_transcoding) for full example.

## CORS

Add CORS headers for browser access:

```elixir
defmodule Helloworld.Endpoint do
  use GRPC.Endpoint

  intercept GRPC.Server.Interceptors.Logger
  intercept GRPC.Server.Interceptors.CORS, allow_origin: "mydomain.io"
  run Helloworld.Greeter.Server
end
```

## Features

- Unary, server-streaming, client-streaming, bidirectional-streaming RPC
- HTTP Transcoding
- TLS Authentication
- Interceptors
- Connection Backoff
- Data compression
- gRPC Reflection (via [grpc-reflection](https://github.com/elixir-grpc/grpc-reflection))

## Migration

See [MIGRATION.md](MIGRATION.md) for upgrade guides between versions.

## Contributing

See upstream [elixir-grpc/grpc](https://github.com/elixir-grpc/grpc) for issues and contributions.
