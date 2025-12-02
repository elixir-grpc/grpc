# Ensure codecs are loaded for tests
codecs = [
  GRPC.Codec.Erlpack,
  GRPC.Codec.WebText,
  GRPC.Codec.Proto,
  GRPC.Codec.JSON
]

Enum.each(codecs, &Code.ensure_loaded/1)

ExUnit.start()
