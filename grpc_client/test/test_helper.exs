Code.require_file("./support/test_adapter.exs", __DIR__)

# Mock for client tests (integration tests need client)
Mox.defmock(GRPC.Client.Resolver.DNS.MockAdapter,
  for: GRPC.Client.Resolver.DNS.Adapter
)

# Start client supervisor for integration tests
{:ok, _pid} =
  DynamicSupervisor.start_link(
    strategy: :one_for_one,
    name: GRPC.Client.Supervisor
  )

ExUnit.start(capture_log: true)
