# Changelog

## Unreleased

### Enhancements

  * Client connections can now be declared in an application's supervision tree, either as a `GRPC.Client.Connection` child spec or through `use GRPC.Client.Connection, otp_app: ...`. Supervised connections establish asynchronously and retry with exponential backoff when the backend is unreachable, instead of failing supervisor startup. New public API: `GRPC.Client.Connection.get_channel/1`, `get_channel!/1`, and `await_ready/2`.

### Behavior Changes

  * `GRPC.Client.Connection.connect/2` now waits at most `:connect_timeout` (default: 15000 ms) for the first establishment attempt instead of blocking indefinitely.
  * When all resolved addresses fail to dial, `connect/2` returns the first underlying dial error (e.g. `{:error, :connection_refused}`) instead of `{:error, :no_addresses}`.
  * Calling `connect/2` with a `:name` already registered to a different target now returns `{:error, {:already_started_with_different_target, target}}` instead of silently returning the existing connection's channel.

## v1.0.0 (2026-06-15)

### Enhancements

  * The client and server implementations were separated into two distinct packages to improve modularity and maintainability.
  * A t() typespec was added to GRPC.Server.Adapters.ReportException to improve type documentation and tooling support.
  * IPv6 support was added to the gRPC gun adapter, enabling connections over IPv6 networks.
  * Virtual channels can now be explicitly named, allowing clearer identification and management of channels.
  * Support was added for grpc-web trailers encoded in the message body, improving compatibility with grpc-web clients.
  * Periodic DNS re-resolution is now supported for client-side service discovery, enabling automatic detection of new backends and removal of stale ones. New connect options: `resolve_interval`, `max_resolve_interval`, `min_resolve_interval`. Public API: `GRPC.Client.Connection.resolve_now/1`.
  * `:gun` and `:mint` are now optional dependencies, allowing users to include only the HTTP client adapter they need.
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
  * Fixed IPv6 target normalization and parsing crashes in `GRPC.Client.Connection`. Bracketed IPv6 addresses and bare IPv6 addresses are now correctly parsed across all resolver scheme prefixes.
  * Fixed named Gun channels being terminated when the original caller process exits by moving connection ownership into adapter-owned processes.
  * Fixed named channels to be scoped to the local node, preventing cross-node name collisions.
  * Fixed a zip bomb vulnerability in the gzip compressor.
  * Fixed the `GRPC.RPCError` type specification.

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
