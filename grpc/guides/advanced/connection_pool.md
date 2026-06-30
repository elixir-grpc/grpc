# Connection Pool

The Elixir gRPC client maintains a **connection pool** for every target address. Rather than opening a fresh HTTP/2 connection for each RPC, callers share a fixed set of long-lived connections. This reduces connection overhead, improves throughput under concurrency, and enables fine-grained control over how many connections and concurrent streams your client opens.

The pool is managed transparently: when you call a stub, the client checks out a connection, tracks the open stream count, and returns the connection when the call finishes. All of this happens without any additional code on your side.

---

## How It Works

When `GRPC.Stub.connect/2` is called, a supervised pool of connections is started for the target address. Each connection is an independent HTTP/2 session capable of multiplexing many concurrent streams.

On each RPC call the client:

1. Picks first connection from the pool with number open streams that is still below the configured limit.
2. Increments that connection's open-stream counter and records a lease.
3. Runs the RPC.
4. Decrements the counter and releases the lease when the call completes.

If every connection in the pool has reached `max_streams`, the client opens an **overflow** connection up to `max_overflow` extra connections. Overflow connections aren't discarded once the load subsides, but if pool dies or connections are otherwise dropped we reset to initial count.

---

## Configuration

Pool behaviour is controlled via the `:pool` option passed to `GRPC.Stub.connect/2` or `GRPC.Client.Connection.connect/2`.

| Option                   | Type                        | Default | Description                                                                                                        |
|:-------------------------|:----------------------------|:--------|:-------------------------------------------------------------------------------------------------------------------|
| `:size`                  | `non_neg_integer`           | `1`     | Number of persistent connections kept open.                                                                        |
| `:max_overflow`          | `non_neg_integer or nil`    | `0`     | Maximum number of extra connections created when the pool is fully saturated. `nil` means no client-side limit.    |
| `:max_streams`           | `pos_integer or nil`        | `nil`   | Maximum concurrent streams per connection. `nil` means no client-side limit (server limit applies).                |
| `:health_check_enabled`  | `boolean`                   | `false` | When `true`, a periodic gRPC health-check ping is sent on each connection every 10 minutes.                        |

---

## Examples

### Default pool (single connection)

```elixir
iex> {:ok, channel} = GRPC.Stub.connect("localhost:50051")
iex> {:ok, reply} = channel |> MyService.Stub.my_rpc(request)
```

### Multiple persistent connections

Open three connections upfront to distribute concurrent load:

```elixir
iex> {:ok, channel} = GRPC.Stub.connect("localhost:50051", pool: %{size: 3})
iex> {:ok, reply} = channel |> MyService.Stub.my_rpc(request)
```

### Overflow connections

Keep two persistent connections and allow up to five overflow connections during traffic spikes:

```elixir
iex> {:ok, channel} =
...>   GRPC.Stub.connect("localhost:50051",
...>     pool: %{size: 2, max_overflow: 5}
...>   )
```

### Limiting streams per connection

Cap each connection at 100 concurrent streams. Requests beyond this limit will use a new overflow connection (if allowed):

```elixir
iex> {:ok, channel} =
...>   GRPC.Stub.connect("localhost:50051",
...>     pool: %{size: 2, max_overflow: 1, max_streams: 100}
...>   )
```

---

## Disconnect

Calling `GRPC.Stub.disconnect/1` stops the pool supervisor and closes all underlying connections:

```elixir
iex> {:ok, channel} = GRPC.Stub.connect("localhost:50051")
iex> {:ok, _} = GRPC.Stub.disconnect(channel)
```

The returned channel has `pool: nil`, indicating the pool is no longer active.

---
