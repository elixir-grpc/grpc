
[
  inputs: ["mix.exs", "{config,lib,test}/**/*.{ex,exs}", "examples/*/{config,lib,priv}/*.ex"],
  import_deps: [:protobuf],
  locals_without_parens: [rpc: 3, intercept: 1, intercept: 2, run: 1, run: 2, field: 2, field: 3],
  export: [
    locals_without_parens: [rpc: 3, intercept: 1, intercept: 2, run: 1, run: 2, field: 2, field: 3]
  ]
]
