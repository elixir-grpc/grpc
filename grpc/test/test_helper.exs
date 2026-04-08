Code.require_file("./support/test_adapter.exs", __DIR__)

# Mock for client tests (integration tests need client)
Mox.defmock(GRPC.Client.Resolver.DNS.MockAdapter,
  for: GRPC.Client.Resolver.DNS.Adapter
)

{parsed, _, _} = OptionParser.parse(System.argv(), switches: [warnings_as_errors: :boolean])

if !parsed[:warnings_as_errors] do
  Application.put_env(:grpc, :run_warning_tests, true)
end

ExUnit.start(capture_log: true)
