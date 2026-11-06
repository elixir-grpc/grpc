# Slow clients and server streams

`GRPC.Server.send_reply/3` returns once the connection has taken the reply. If the client reads slower than the server sends, `send_reply/3` waits, and so does the code that called it. `GRPC.Stream.run_with/3` calls `send_reply/3` from its Flow stages, so a waiting stage stops asking for more input.

This holds for the Cowboy adapter.

## Where the limit comes from

Cowboy queues the replies a client has no window for. Its `max_stream_buffer_size` (default 8,000,000 bytes) and `max_connection_buffer_size` (default 16,000,000) cap that queue. When a cap is reached, Cowboy stops acknowledging sends and `send_reply/3` waits until the client reads. A server that sends faster than the network takes the data holds at most those buffers for each stream or connection, plus one reply for each process that is sending.

Set both in `adapter_opts` to hold less:

```elixir
adapter_opts: [max_stream_buffer_size: 1_000_000, max_connection_buffer_size: 4_000_000]
```

## How long a send waits

By default a send waits as long as the client takes. A call that has a deadline stops waiting at the deadline: `send_reply/3` raises a `GRPC.RPCError` with status `:deadline_exceeded`.

To also bound the wait on calls with no deadline, set `:send_timeout` (milliseconds, default `:infinity`) in `adapter_opts`. When it passes, `send_reply/3` raises the same error.

```elixir
adapter_opts: [send_timeout: 30_000]
```

## When the client is gone

The server's process for the stream is stuck while it waits for the client, so it can't act on the disconnect itself. Cowboy asks it to shut down and kills it after `shutdown_timeout` (default 5,000 ms, also an option in `adapter_opts`). Until then the producer is still waiting in `send_reply/3`. When the stream's process is killed, `send_reply/3` exits with `:shutdown`, or the exit signal stops the calling process first. Use `try/after` for cleanup.
