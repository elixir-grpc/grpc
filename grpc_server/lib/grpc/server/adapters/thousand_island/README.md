# ThousandIsland Adapter

Pure Elixir HTTP/2 gRPC server adapter using ThousandIsland.

## Overview

This adapter provides a pure Elixir implementation of HTTP/2 for gRPC, built on top of ThousandIsland TCP server. Unlike the Cowboy adapter, this implementation has no native dependencies.

## Usage

### Basic Setup

In your application supervisor:

```elixir
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      # Your gRPC endpoint using ThousandIsland adapter
      {GRPC.Server.Supervisor,
       endpoint: MyApp.Endpoint,
       port: 50051,
       start_server: true,
       adapter: GRPC.Server.Adapters.ThousandIsland,
       adapter_opts: [
         num_acceptors: 10,
         max_connections: 1000
       ]}
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

### Defining Your Endpoint

```elixir
defmodule MyApp.Endpoint do
  use GRPC.Endpoint

  intercept GRPC.Server.Interceptors.Logger
  run MyApp.Greeter.Server
  run MyApp.OtherService.Server
end
```

### Defining Your Service

```elixir
defmodule MyApp.Greeter.Service do
  use GRPC.Service, name: "helloworld.Greeter"

  rpc :SayHello, Helloworld.HelloRequest, Helloworld.HelloReply
end

defmodule MyApp.Greeter.Server do
  use GRPC.Server, service: MyApp.Greeter.Service

  @spec say_hello(Helloworld.HelloRequest.t(), GRPC.Server.Stream.t()) ::
          Helloworld.HelloReply.t()
  def say_hello(request, _stream) do
    Helloworld.HelloReply.new(message: "Hello #{request.name}")
  end
end
```

## Configuration Options

### Adapter Options

- `:num_acceptors` - Number of acceptor processes (default: 100)
- `:max_connections` - Maximum concurrent connections (default: 16_384)
- `:transport_options` - Additional transport options passed to ThousandIsland

### TLS Configuration

For production use, configure TLS:

```elixir
{GRPC.Server.Supervisor,
 endpoint: MyApp.Endpoint,
 port: 50051,
 start_server: true,
 adapter: GRPC.Server.Adapters.ThousandIsland,
 adapter_opts: [
   transport_module: ThousandIsland.Transports.SSL,
   transport_options: [
     certfile: "/path/to/cert.pem",
     keyfile: "/path/to/key.pem",
     alpn_preferred_protocols: ["h2"]
   ]
 ]}
```

## Features

### Supported

- ✅ HTTP/2 with HPACK header compression
- ✅ All 4 gRPC streaming types:
  - Unary (single request → single response)
  - Client streaming (stream → single response)
  - Server streaming (single → stream)
  - Bidirectional streaming (stream → stream)
- ✅ Flow control (connection and stream level)
- ✅ Multiple concurrent streams per connection
- ✅ gRPC error handling with status codes
- ✅ Content negotiation (protobuf, compression)
- ✅ Pure Elixir implementation (no NIFs)

### Not Yet Supported

- ⚠️ HTTP/2 Server Push
- ⚠️ SETTINGS frame priority handling
- ⚠️ HTTP/2 PRIORITY frames

## Architecture

```
GRPC.Server.Supervisor
  └── ThousandIsland (TCP acceptor pool)
       └── Connection processes (one per client)
            ├── Handler (HTTP/2 frame handling)
            ├── Connection (state + dispatch)
            ├── StreamRegistry (stream lifecycle)
            └── Dispatcher (gRPC service routing)
```

Each client connection is handled by a separate process that:

1. Accepts HTTP/2 connection preface
2. Manages HTTP/2 settings and flow control
3. Routes incoming gRPC requests to service implementations
4. Handles streaming in both directions
5. Encodes responses with proper framing and headers

## Testing

### Unit Tests

Unit tests for the adapter are in `test/grpc/server/adapters/thousand_island_test.exs`.

### Integration Tests

Integration tests belong in the **grpc_client** package, not in grpc_server. This follows the architecture where:

- `grpc_server` contains server implementation and unit tests
- `grpc_client` contains client implementation and integration tests (client ↔ server)

To test the ThousandIsland adapter end-to-end:

1. Start a test server using `GRPC.Server.Supervisor` with the ThousandIsland adapter
2. Use `GRPC.Stub` to create a client connection
3. Make RPC calls and verify responses

Example integration test (in grpc_client):

```elixir
# In grpc_client/test/integration/thousand_island_test.exs

setup_all do
  # Start server with ThousandIsland adapter
  {:ok, _pid, port} = GRPC.Server.start(
    MyTestEndpoint,
    port: 0,
    adapter: GRPC.Server.Adapters.ThousandIsland
  )
  
  # Connect client
  {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
  
  {:ok, channel: channel}
end

test "unary RPC works", %{channel: channel} do
  request = Helloworld.HelloRequest.new(name: "World")
  {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(request)
  
  assert reply.message == "Hello World"
end
```

## Performance Considerations

- Each connection runs in its own process
- Connection processes are supervised by ThousandIsland
- Streams are tracked in ETS for fast lookup
- Flow control prevents memory exhaustion
- HPACK compression reduces header overhead

For high-throughput scenarios, tune:

- `:num_acceptors` - More acceptors for connection bursts
- `:max_connections` - Total concurrent connection limit
- Initial flow control window sizes (in Settings)

## Comparison with Cowboy Adapter

| Feature | ThousandIsland | Cowboy |
|---------|---------------|--------|
| Implementation | Pure Elixir | NIF (Ranch) |
| HTTP/2 | Custom | Cowlib |
| Dependencies | Minimal | Ranch, Cowlib |
| Performance | Good | Excellent |
| Debugging | Easier | Harder |
| Production Ready | Testing | Yes |

## Development Status

**Current Status**: Feature-complete, testing phase

The adapter implements all core gRPC functionality. Integration testing with real clients is needed before production use.

## Contributing

The adapter code is in `lib/grpc/server/adapters/thousand_island/`:

- `adapter.ex` - Main adapter interface
- `handler.ex` - HTTP/2 frame handling
- `connection.ex` - Connection state and dispatch
- `dispatcher.ex` - gRPC service routing
- `stream_state.ex` - Individual stream lifecycle
- `stream_registry.ex` - Multi-stream coordination
- `stream_collector.ex` - Streaming response collection
- `http2/` - HTTP/2 protocol implementation

When contributing:

1. Add unit tests for new functionality
2. Follow existing code style
3. Update this README for user-visible changes
4. Add integration tests in grpc_client package
