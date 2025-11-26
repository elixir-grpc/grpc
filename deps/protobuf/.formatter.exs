locals_without_parens = [field: 2, field: 3, oneof: 2, extend: 4, extensions: 1]

[
  inputs: ["{mix,.formatter}.exs", "{config,lib,conformance,test}/**/*.{ex,exs}"],
  locals_without_parens: locals_without_parens,
  export: [locals_without_parens: locals_without_parens],
  import_deps: [:stream_data]
]
