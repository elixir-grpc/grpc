# Changelog

## Unreleased

### Enhancements

  * New Cowboy adapter options `:max_connection_age` and `:max_connection_age_grace`, in milliseconds. After the age, the server sends GOAWAY, lets open calls finish, and closes the connection. After the grace, it ends calls that are still open. The age has ±10% jitter and counts from the first call on the connection. Both default to `:infinity`, which changes nothing. Clients that balance over many servers, for example behind a Kubernetes headless service, then reconnect and find new replicas. The new "Connection age" guide has the details and the parts of gRPC proposal A9 that are not done: server keepalive PINGs and the check of client PINGs. Cowboy must change for both (#355).

  * New Cowboy adapter option `:max_connection_idle`, in milliseconds. When a connection has no open call for this time, the server closes it in the same way as for `:max_connection_age`: GOAWAY, then the open calls finish. The time counts from the end of the last call and has no jitter. A call that arrives while the server closes the connection is not lost. Unlike A9, the time starts at the first call. A9 starts it at the creation of the connection. Cowboy has no hook before the first stream. A connection that sends no call is never closed by this option. Only Cowboy's `:idle_timeout`, which this adapter sets to `:infinity` by default, limits it. The default is `:infinity`, which changes nothing. You can set `:max_connection_age` and `:max_connection_idle` together. The first limit to end closes the connection (#355).

  * New event: `[:grpc, :server, :rpc, :send_error]`. The Cowboy adapter publishes it when the handler returns but the adapter cannot encode, compress or send the reply. The metadata has `:kind`, `:reason`, `:stacktrace` and `:path`. It is not an `:exception` event, because the telemetry span of the handler is closed at that time. A client that disconnects during the reply still causes only `:abort`, because nothing raises. A new section in the error handling guide shows how to send these events to Sentry or Bugsnag (#349).

  * `GRPC.Server.Test` runs server interceptors in a test with no endpoint, server process or socket. `build_stream/3` makes a `GRPC.Server.Stream` for an RPC, `run_interceptors/4` runs the chain with the same code the server uses, and `response_headers/1`, `response_trailers/1` and `replies/1` return what was sent. See the new "Testing interceptors" guide (#423).

  * `config :grpc, max_message_length: bytes` now limits one message the server receives, as well as one it sends, and `guides/advanced/message_size_limits.md` documents it with `:max_body_size` and the other limits (#387). The default stays 2 GiB. A request message over the limit is refused with `RESOURCE_EXHAUSTED` before the handler runs, and the error says the limit. The Cowboy handler checks the 5-byte length prefix in the first chunk that holds it, for a unary request and for each message of a request stream, and stops reading, so a client sends about one flow-control window of an oversize message. This needs no `grpc_core` change.

  * `[:grpc, :server, :rpc, :abort]` is now published when the adapter stops an in-flight RPC — an expired deadline, a client cancellation, a dropped connection. The exit signal that stops the RPC process does not unwind it, so `:stop` and `:exception` cannot be published for such a call; the new event carries the stream, server, endpoint, request path, RPC pid and exit reason.

  * A test and a doc note now cover that the Cowboy adapter's accepted sockets, plain and TLS, have `TCP_NODELAY` on. This is Ranch's default and the adapter passes nothing that turns it off (#323).

### Behavior Changes

  * A reply that is over the message size limit now fails with `RESOURCE_EXHAUSTED` instead of `INTERNAL`, and the error message states the limit (#387).

  * A `GRPC.RPCError` raised while the Cowboy adapter sends the reply or the trailers, after the handler returned, or when no handler matches the request, now keeps its status. Before, the client got `UNKNOWN`. For example, a known path called with an HTTP method that the RPC does not accept now gets `UNIMPLEMENTED` (#496).

  * A unary request body that is cut short, or a request stream message with the compressed flag and no `grpc-encoding`, now gets `INTERNAL` instead of `INVALID_ARGUMENT`. A unary call with no message gets `UNIMPLEMENTED`. These are the codes grpc/grpc's `doc/statuscodes.md` gives a library; it never generates `INVALID_ARGUMENT` (#650).

### Bug Fixes

  * `GRPC.Server.send_reply/3` now waits until the Cowboy adapter has taken the reply, so a client that reads slowly slows the server stream. Before, replies piled up in the stream process's mailbox without limit. This also covers `GRPC.Stream.run_with/3`. The wait is bounded by Cowboy's `max_stream_buffer_size` and `max_connection_buffer_size`, by the call's deadline, and by the new `:send_timeout` adapter option (default `:infinity`). If the stream's process is gone, `send_reply/3` exits with `:shutdown`. See the "Slow clients and server streams" guide (#496).
  * `GRPC.Server.send_reply/3` no longer leaves a late ack in the caller's mailbox after a `:send_timeout` or deadline wait ends. It raises `:deadline_exceeded` at once, and queues nothing, when the call's deadline has already passed. A unary reply whose send times out, after the handler returned, now gives the client `DEADLINE_EXCEEDED`. Before, it gave `UNKNOWN`. The guide lists three limits: a handler blocked in `send_reply/3` does not read requests, a reply whose wait timed out is still sent later, and a call whose deadline has passed raises before it queues the reply (#496).
  * A compressor that raises does not crash the Cowboy handler process now. The client gets `INTERNAL` with a `grpc-status`. Before, it got no `grpc-status`. The adapter logs the error one time, with `crash_reason`.
  * A listen failure now logs which endpoint could not start and why, for example `Could not start MyApp.Endpoint: port 50051 is already in use (eaddrinuse)`. Before, the Cowboy adapter's `:eaddrinuse` clause never matched Ranch's `{:listen_error, ref, :eaddrinuse}`, and nothing named the port. Under a supervisor, the child's error is now `{:listen_failed, message, reason}`, with the old term as `reason`; match on `:eaddrinuse` anywhere in it. `GRPC.Server.start_endpoint/3` still returns the same `{:error, reason}`. `GRPC.Server.Adapters.Cowboy.start_link/5` replaces `start_link/4` and takes the port. With Ranch 2, the adapter now also logs `Running ... with Cowboy using ...` at startup (#330).
  * Request data that waits for its handler is credited to the client once, when the handler reads it. It was credited on arrival and again on read, so the receive window grew without bound and a client was never held back. A handler that stops reading now holds its client to the stream window (`initial_stream_window_size`).
  * Unless set, the Cowboy adapter's connection window is 16 stream windows (`initial_connection_window_size`, `connection_window_margin_size`), so streams whose handlers stop reading can't take the whole connection window from the other streams on it.
  * Servers started with `GRPC.Server.start/3` (no endpoint) now each keep their own routes. The last server started used to replace the routes of the others, and a call could fail with UNIMPLEMENTED or 404. Servers started with an endpoint are not changed (#649).
  * A request message that does not decode now gets `INTERNAL`, with a `grpc-message` that says what failed, and a unary call with a second message gets `UNIMPLEMENTED`. Before, the codec's exception raised in the RPC process: the client got `UNKNOWN` "Internal Server Error", and the exception reached the `exception_log_filter` as a crash, or for a request stream was also logged past it. It is now a `GRPC.RPCError`, which a filter can drop by status. This covers protobuf, JSON, Erlpack and gRPC-Web text bodies, a gzip message that does not decompress, a trailers frame or unknown flag byte in a request, and bytes after a unary message. A request stream that ends inside a message, which got no reply at all, now gets `INTERNAL` too. This needs `grpc_core` 1.0.7 (#650).

## v1.0.6 (2026-11-06)

### Security

  * Enforce `max_body_size` on request-streaming and bidirectional-streaming RPCs. Before this, a client could send an unbounded body across many small frames and exhaust server memory. See [GHSA-vx6q-cr5q-945g](https://github.com/elixir-grpc/grpc/security/advisories/GHSA-vx6q-cr5q-945g) (CVE-2026-71397).

## v1.0.4 (2026-0-15)

### Bug Fixes

  * Remove dangling typespecs

## v1.0.0 (2026-06-15)

### Enhancements

  * The client and server implementations were separated into two distinct packages to improve modularity and maintainability.
  * A t() typespec was added to GRPC.Server.Adapters.ReportException to improve type documentation and tooling support.
  * IPv6 support was added to the gRPC gun adapter, enabling connections over IPv6 networks.
  * Virtual channels can now be explicitly named, allowing clearer identification and management of channels.
  * Support was added for grpc-web trailers encoded in the message body, improving compatibility with grpc-web clients.
  * The inbound gRPC deadline is now surfaced on `GRPC.Server.Stream` via the `deadline` field (absolute monotonic time in ms, or `nil` if no deadline was set) and the `GRPC.Server.Stream.remaining_ms/1` helper function, enabling deadline budget propagation in interceptors and handlers.
  * `application/json` content-type matching in transcoding is now loosened to support additional content types with parameters.
  * Boolean query parameters are now correctly coerced to Elixir boolean types in transcoding.
  * Elixir 1.20 is now supported with updated dependency constraints.

### Bug Fixes

  * Fixed an issue in stream map_error where send_response handling could behave incorrectly.
  * Resolved a persistent_term memory leak in GRPC.Client.Connection that occurred when a connection was disconnected.
  * Corrected the cacertfile option name.
  * Added detection of name collisions and introduced a fallback mechanism for the call function.
  * Fixed broken benchmark links in the project.
  * Resolved two compiler warnings/errors in grpc.
  * Ensured $callers are properly propagated in GRPC.Stream Flow workers.
  * Fixed address key generation in handle_info(:refresh) to correctly support Unix domain sockets.
  * Fixed an issue where buffers were not fully drained when a single HTTP/2 frame carried multiple gRPC messages.
  * Resolved a client disconnection handling issue.
  * Fixed type system warnings related to Elixir 1.19.
  * Fixed an error handling issue in stream map_error.
  * Corrected the connection state initialization created by build_direct_state.
  * Added a configurable request body size limit to prevent potential resource exhaustion.
  * Fixed path parameters to correctly take precedence over query parameters in HTTP transcoding.
  * Fixed unsafe `binary_to_term` decoding in the erlpack codec to block dangerous terms.
  * Fixed a zip bomb vulnerability in the gzip compressor.

### Documentation Fixes

  * Corrected documentation related to unary responses and added clarification about the adapter_opts.cred option.

## v0.11.5 (2025-11-14)

### Enhancements

  * Feat add `exception_log_filter` option to server

### Bug fixes

  * Fix ensure thers is only one `GRPC.Client.Supervisor`.
  * Fix report `GRPC.Errors` as normal shutdowns

## v0.11.4 (2025-11-07)

### Enhancements

  * Feat added new function to handle side-effects.
  * Feat added error handler for unary and stream pipelines.
  * Docs adds a better explanation of the different types of input.
  * Docs improvements to module documentation.
  * Docs livebooks added directly to the documentation.

### Bug fixes

  * Fix refresh error spam on direct_state (no lb).
  * Fix correct return type in doc.
