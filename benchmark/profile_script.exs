# Script to profile gRPC server performance
# Run with: mix profile.eprof profile_script.exs
# Note: mix profile.eprof runs this script twice (warmup + profile)

# Disable logging
Logger.configure(level: :error)

# Start the server
{:ok, _} = Application.ensure_all_started(:grpc_server)
{:ok, _} = Application.ensure_all_started(:benchmark)

server = %Grpc.Testing.ServerConfig{
  async_server_threads: 1,
  port: 10000
}

_server = Benchmark.ServerManager.start_server(server, adapter: GRPC.Server.Adapters.ThousandIsland)
Process.sleep(500)

# Connect client
{:ok, ch} = GRPC.Stub.connect("localhost:10000")

# Prepare request
payload_type = Grpc.Testing.PayloadType.value(:COMPRESSABLE)
req = %Grpc.Testing.SimpleRequest{
  response_type: payload_type,
  response_size: 0,
  payload: %Grpc.Testing.Payload{type: payload_type, body: <<>>}
}

# This code will be profiled
Enum.each(1..500, fn _ ->
  Grpc.Testing.BenchmarkService.Stub.unary_call(ch, req)
end)
