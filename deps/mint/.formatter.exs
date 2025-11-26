# Used by "mix format"
[
  inputs: ["mix.exs", "{lib,test}/**/*.{ex,exs}"],
  import_deps: [:stream_data],
  locals_without_parens: [
    assert_round_trip: 1,
    assert_recv_frames: 1,
    assert_http2_error: 2,
    assert_transport_error: 2
  ]
]
