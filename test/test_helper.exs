Code.require_file("./support/test_adapter.exs", __DIR__)
{:ok, _} = Application.ensure_all_started(:ex_machina)

codecs = [
  GRPC.Codec.Erlpack,
  GRPC.Codec.WebText,
  GRPC.Codec.Proto
]

Enum.each(codecs, &Code.ensure_loaded/1)
ExUnit.start(capture_log: true)
