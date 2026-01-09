# Changelog

## v1.0.0-rc.1 (2025-12-02)

### Enhancements

  * Major restructuring: separated into distinct grpc_core, grpc_server, and grpc_client packages
  * Refactor GRPC client setup - start supervisor in application (#483)

### Bug fixes

  * Fix Connection state created by build_direct_state (#480)

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
