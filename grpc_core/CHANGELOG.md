# Changelog

## Unreleased

### Behavior Changes

  * `GRPC.Message.from_data/2` answers a buffer that does not hold a whole message, such as a body cut short, with `INTERNAL` instead of `INVALID_ARGUMENT` as per [the standard](https://github.com/grpc/grpc/blob/master/doc/statuscodes.md).

### Bug Fixes

  * `GRPC.Codec.Erlpack.decode/2` refuses compressed terms. The VM inflated them in full before any check ran, so a small request could exhaust server memory.
  * `GRPC.Message.from_data/2` no longer crashes on invalid trailers.
  * `GRPC.Message.from_data/2` now returns error code `INTERNAL` instead of crashing on data which fails to decompress.
