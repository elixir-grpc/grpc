# Installation

The package can be installed as:

```elixir
def deps do
  [
    {:grpc, "~> 0.11"},
    {:protobuf, "~> 0.14"}, # optional for importing well-known Google gRPC types
    {:grpc_reflection, "~> 0.2"}, # optional for enabling gRPC reflection
    {:protobuf_generate, "~> 0.1", only: :dev} # optional for Protobuf code generation with plugins
  ]
end
``` 