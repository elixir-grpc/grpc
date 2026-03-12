# Changelog

## v1.0.0-rc.1 (2026-03-12)

### Enhancements

  * Feat: separating client and server into two distinct packages (#477)
  * Feat: add t() typespec to GRPC.Server.Adapters.ReportException (#506)
  * Feat: Add IPv6 support for gRPC gun adapter (#495)
  * Feat: Support naming a virtual channel (#491)
  * Feat: Support grpcweb trailers encoded in the message (#481)

### Bug fixes

  * Fix Stream map_error send_response handling (#487)
  * Fix: erase persistent_term leak in GRPC.Client.Connection on disconnect (#509)
  * Fix: cacertfile rename (#508)
  * Fix: detect name collisions and provide fallback call function (#497)
  * Fix: broken benchmark links (#505)
  * Fix: two compiler warnings/errors in grpc_client (#504)
  * Fix: propagate $callers in GRPC.Stream Flow workers (#502)
  * Fix: use build_address_key/2 in handle_info(:refresh) to support Unix domain sockets (#501)
  * Fix: fully drain buffer when an HTTP/2 frame carries multiple gRPC messages (#498)
  * Fix: client disconnect (#493)
  * Fix: Elixir 1.19 type system warnings (#490)
  * Fix: Stream map error  (#487)
  * Fix: Connection state created by build_direct_state (#480)

### Docs Fixes

  * Docs fixes: unary response correction, a note on adapter_opts.cred (#484)

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
