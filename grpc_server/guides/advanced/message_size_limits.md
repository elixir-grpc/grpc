# Message size limits

A server limits the size of the messages it sends and receives. A message over a
limit fails the call with status `RESOURCE_EXHAUSTED` (`8`), and the error message
says which limit it hit.

| Option | Where to set it | Default | Applies to |
|--------|-----------------|---------|------------|
| `:max_message_length` | `config :grpc, max_message_length: bytes` | 2 GiB (`2_147_483_648`) | One message, sent or received |
| `:max_body_size` | `GRPC.Server.Supervisor` option | 4 MiB (`4_194_304`) | The whole request body of one call |
| `:max_decompressed_message_length` | `config :grpc, max_decompressed_message_length: bytes` | 4 MiB | One message after gzip decompression |

## `:max_message_length`

```elixir
# config/config.exs
config :grpc, max_message_length: 16 * 1024 * 1024
```

The limit is read on every message, so it also works from `Application.put_env/3`
at run time. It applies in both directions:

* A reply over the limit is not sent. The client gets `RESOURCE_EXHAUSTED` with a
  message such as `Encoded message is too large (2048 bytes), the limit is 1000 bytes`.
  This was `INTERNAL` before.
* A request message over the limit is refused, from the length in its prefix, before
  your handler runs. The client gets `RESOURCE_EXHAUSTED` with a message such as
  `Received message is too large (2048 bytes, the limit is 1000 bytes)`. For a request
  stream, each message is checked as the handler reads it.

The setting is global to the node. A node that runs a client and a server applies
the same limit to both. Compressed messages count by their size on the wire.

## `:max_body_size`

`:max_body_size` caps the whole request body, so on a request stream it limits the
total of all messages. Set it on the supervisor:

```elixir
children = [
  {GRPC.Server.Supervisor, endpoint: MyApp.Endpoint, port: 50051, max_body_size: 64 * 1024 * 1024}
]
```

A request over it fails with `RESOURCE_EXHAUSTED` and `Request body too large`.
It has to be at least as large as the biggest request you accept, so raise it
together with `:max_message_length` for large requests.

## Sending very large replies

On a loopback connection, a unary reply of 100 MB, 300 MB and 600 MB is sent
without error by the Cowboy adapter, using its default settings. No setting is needed
to send a reply larger than the HTTP/2 flow-control windows, which start at 64 KiB.

If a large reply fails, look at these first:

* **The client's timeout.** A unary call gives up after 10 seconds unless the client
  passes `:timeout`. The server learns the deadline from the `grpc-timeout` header
  and stops the call with `DEADLINE_EXCEEDED` when it passes. Raise the client's
  `:timeout`.
* **Cowboy timeouts.** The adapter sets `idle_timeout` and `inactivity_timeout` to
  `:infinity` unless you pass `:idle_timeout` in `:adapter_opts`. A finite value
  closes a connection that sends no data for that long.
* **Window and frame sizes.** `:initial_stream_window_size`,
  `:initial_connection_window_size` and `:max_frame_size_sent` in `:adapter_opts`
  are passed to Cowboy. See the `cowboy_http2` documentation for their meaning.
