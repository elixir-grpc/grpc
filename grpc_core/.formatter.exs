[
  inputs: [
    "{mix,.formatter}.exs",
    "{config,lib,test}/**/*.{ex,exs}"
  ],
  import_deps: [:protobuf],
  locals_without_parens: [rpc: 3],
  export: [
    locals_without_parens: [rpc: 3]
  ]
]
