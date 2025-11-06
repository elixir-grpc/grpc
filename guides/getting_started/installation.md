# Installation

The package can be installed as:

```elixir
def deps do
  [
    {:grpc, "~> 0.11"},
    {:protobuf, "~> 0.14"}, # optional for import wellknown google types
    {:grpc_reflection, "~> 0.2"}, # optional enable grpc reflection
    {:protobuf_generate, "~> 0.1", only: :dev} # optional for protobuf codegen
  ]
end
``` 