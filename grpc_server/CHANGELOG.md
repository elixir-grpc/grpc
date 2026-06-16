# Changelog

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
