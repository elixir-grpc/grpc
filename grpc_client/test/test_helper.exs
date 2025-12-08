Code.require_file("./support/test_adapter.exs", __DIR__)

# Mock for client tests (integration tests need client)
Mox.defmock(GRPC.Client.Resolver.DNS.MockAdapter,
  for: GRPC.Client.Resolver.DNS.Adapter
)

ExUnit.start(capture_log: true)
