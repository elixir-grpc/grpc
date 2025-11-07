Code.require_file("./support/test_adapter.exs", __DIR__)

codecs = [
  GRPC.Codec.Erlpack,
  GRPC.Codec.WebText,
  GRPC.Codec.Proto
]

Enum.each(codecs, &Code.ensure_loaded/1)

Mox.defmock(GRPC.Client.Resolver.DNS.MockAdapter,
  for: GRPC.Client.Resolver.DNS.Adapter
)

{:ok, _pid} =
  DynamicSupervisor.start_link(
    strategy: :one_for_one,
    name: GRPC.Client.Supervisor
  )

ExUnit.start(capture_log: true)
