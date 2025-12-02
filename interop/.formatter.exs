[
  inputs: [
    "{mix,.formatter}.exs",
    "{config,lib,test,src,script}/**/*.{ex,exs}"
  ],
  import_deps: [:grpc_core, :protobuf]
]
