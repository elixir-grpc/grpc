# Managing HTTP/2 Connections Efficiently

When managing large numbers of gRPC HTTP/2 connections, you may benefit from pooling of some sort.

Currently `elixir-grpc` does not offer pooling functionality, but [here is an excellent guide on using Elixir's `Registry` module to create your own resource pools](https://andrealeopardi.com/posts/process-pools-with-elixirs-registry/).

It's also worth noting that if using the `Mint` adapter for HTTP/2, you can choose which connection to use by checking the value of each connection's open request count with `open_request_count/1` .

If you'd prefer to use an existing pool implementation, [check out `conn_grpc` on Hex](https://hexdocs.pm/conn_grpc/ConnGRPC.Pool.html)!
