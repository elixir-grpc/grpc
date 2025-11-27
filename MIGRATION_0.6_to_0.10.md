# Migration Guide: grpc_fresha 0.6 → 0.10

## Requirements

- **Elixir**: 1.15+ (was 1.5+)
- **OTP**: 25+ (was 21+)
- **protobuf**: ~> 0.14 (was ~> 0.10)

## Breaking Changes

### Module Renames

```elixir
# Error module
GRPC.Error           → GRPC.RPCError

# Client adapters
GRPC.Adapter.Gun     → GRPC.Client.Adapters.Gun

# Server adapters  
GRPC.Adapter.Cowboy  → GRPC.Server.Adapters.Cowboy
```

### RPC Response Tuples

```elixir
# Success - unchanged
{:ok, response}

# Error - now 2-tuple instead of 3-tuple
# Before (0.6):
{:error, %GRPC.RPCError{}, _extra}

# After (0.10):
{:error, %GRPC.RPCError{}}
```

### Protobuf Struct Construction

protobuf 0.14 deprecates `.new/1` - use struct syntax:

```elixir
# Before:
Google.Rpc.Status.new(code: 3, message: "error")

# After:
%Google.Rpc.Status{code: 3, message: "error"}
```

### HTTP2 Server Trailers

```elixir
# Before:
GRPC.Transport.HTTP2.server_trailers(status, message, custom_trailers)

# After (3rd arg removed):
GRPC.Transport.HTTP2.server_trailers(status, message)
```

### Base64 Binary Metadata

Binary metadata (`*-bin` headers) now uses standard base64 with padding:

```elixir
# Before: "woA"
# After:  "woA="
```

## New Features (Non-Breaking)

- **HTTP Transcoding**: REST/JSON gateway for gRPC services
- **CORS Support**: Cross-origin request handling
- **Mint Adapter**: Alternative HTTP/2 client (`GRPC.Client.Adapters.Mint`)
- **Flow Dependency**: For stream processing
- **Telemetry**: Built-in instrumentation events

## Quick Migration Checklist

1. Update `mix.exs` deps:
   ```elixir
   {:grpc_fresha, "~> 0.10"}
   {:protobuf, "~> 0.14"}
   ```

2. Find/replace module names:
   - `GRPC.Error` → `GRPC.RPCError`
   - `GRPC.Adapter.Gun` → `GRPC.Client.Adapters.Gun`
   - `GRPC.Adapter.Cowboy` → `GRPC.Server.Adapters.Cowboy`

3. Update error handling patterns from 3-tuple to 2-tuple

4. Replace `.new()` calls with `%Struct{}` syntax

5. Regenerate protobuf files:
   ```bash
   mix protobuf.generate --include-path=./priv/protos ./priv/protos/*.proto
   ```

6. Run tests to catch remaining issues
