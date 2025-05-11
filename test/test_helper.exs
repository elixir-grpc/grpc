Code.require_file("./support/test_adapter.exs", __DIR__)

codecs = [
  GRPC.Codec.Erlpack,
  GRPC.Codec.WebText,
  GRPC.Codec.Proto
]

Enum.each(codecs, &Code.ensure_loaded/1)
ExUnit.start(capture_log: true, exclude: [:skip])
