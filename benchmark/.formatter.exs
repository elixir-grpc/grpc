[
  inputs: ["mix.exs", "{config,lib,test}/**/*.{ex,exs}"],
  import_deps: [:protobuf, :grpc_server, :grpc_client]
]
