# Introduction

`GRPC` is a fully featured Elixir implementation of the gRPC protocol (grpc.io),
enabling efficient communication between services through a unified and
stream-oriented API. It supports all RPC types, friendly error handling, TLS,
interceptors, reflection, and optional HTTP transcoding.

Suitable for both server and client development in pure Elixir, enabling
scalable, efficient and type-safe distributed systems.

## Main features:

  * Unary, Server Streaming, Client Streaming, Bi-directional Streaming RPCs;
  * Streaming-first API for every call;
  * Interceptors (auth, logging, rate limiting, tracing);
  * Error handling with predictable propagation;
  * TLS authentication and message compression;
  * Connection load balancing strategies (Round Robin, Pick First);
  * gRPC Reflection;
  * HTTP Transcoding for REST ↔ gRPC compatibility;

---