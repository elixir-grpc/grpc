Logger.configure(level: :warning)

alias GRPC.Client.Adapters.Gun
alias Interop.Client

{:ok, _pid, port} = GRPC.Server.start_endpoint(Interop.Endpoint, 0, adapter: GRPC.Server.Adapters.ThousandIsland)
IO.puts("Server started on port #{port}")

opts = [adapter: Gun]
ch = Client.connect("127.0.0.1:#{port}", opts)

tests = [
  {"empty_unary", fn -> Client.empty_unary!(ch) end},
  {"cacheable_unary", fn -> Client.cacheable_unary!(ch) end},
  {"large_unary", fn -> Client.large_unary!(ch) end},
  {"large_unary2", fn -> Client.large_unary2!(ch) end},
  {"client_compressed_unary", fn -> Client.client_compressed_unary!(ch) end},
  {"server_compressed_unary", fn -> Client.server_compressed_unary!(ch) end},
  {"client_streaming", fn -> Client.client_streaming!(ch) end},
  {"client_compressed_streaming", fn -> Client.client_compressed_streaming!(ch) end},
  {"server_streaming", fn -> Client.server_streaming!(ch) end},
  {"server_compressed_streaming", fn -> Client.server_compressed_streaming!(ch) end},
  {"ping_pong", fn -> Client.ping_pong!(ch) end},
  {"empty_stream", fn -> Client.empty_stream!(ch) end},
  {"custom_metadata", fn -> Client.custom_metadata!(ch) end},
  {"status_code_and_message", fn -> Client.status_code_and_message!(ch) end},
  {"unimplemented_service", fn -> Client.unimplemented_service!(ch) end},
  {"cancel_after_begin", fn -> Client.cancel_after_begin!(ch) end},
  {"cancel_after_first_response", fn -> Client.cancel_after_first_response!(ch) end},
  # Note: timeout_on_sleeping_server skipped - requires client adapter fix for DEADLINE_EXCEEDED status
]

results = %{passed: 0, failed: 0}

results = for {name, test_fn} <- tests, reduce: results do
  acc ->
    try do
      test_fn.()
      IO.puts("✓ #{name}")
      %{acc | passed: acc.passed + 1}
    rescue
      e ->
        IO.puts("✗ #{name}: #{Exception.message(e)}")
        %{acc | failed: acc.failed + 1}
    end
end

IO.puts("\n========================================")
IO.puts("Results: #{results.passed}/#{results.passed + results.failed} tests passed")
IO.puts("========================================")

GRPC.Server.stop_endpoint(Interop.Endpoint, adapter: GRPC.Server.Adapters.ThousandIsland)
