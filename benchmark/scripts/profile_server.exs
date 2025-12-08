# Profile gRPC server to identify performance bottlenecks
# Run with: mix run scripts/profile_server.exs

require Logger

Logger.configure(level: :warning)

# Start server
{:ok, _pid, port} = GRPC.Server.start_endpoint(Benchmark.ServerManager, 0)

IO.puts("Server started on port #{port}")
IO.puts("Waiting 2 seconds for server to be ready...")
Process.sleep(2000)

# Prepare client connection
opts = [adapter: GRPC.Client.Adapters.Gun]
channel = GRPC.Client.Stub.connect("127.0.0.1:#{port}", opts)

# Prepare request
request = Grpc.Testing.SimpleRequest.new(
  response_type: :COMPRESSABLE,
  response_size: 314_159,
  payload: Grpc.Testing.Payload.new(
    type: :COMPRESSABLE,
    body: :binary.copy(<<0>>, 271_828)
  )
)

IO.puts("\n=== Starting profiling with :fprof ===")
IO.puts("Warmup: sending 100 requests...")

# Warmup
for _ <- 1..100 do
  {:ok, _response} = channel
  |> Grpc.Testing.BenchmarkService.Stub.unary_call(request)
end

IO.puts("Warmup complete. Starting profiling...")
Process.sleep(1000)

# Start profiling
:fprof.trace([:start, {:procs, Process.list()}])

# Run profiled requests
for _ <- 1..1000 do
  {:ok, _response} = channel
  |> Grpc.Testing.BenchmarkService.Stub.unary_call(request)
end

# Stop profiling
:fprof.trace(:stop)

IO.puts("Profiling complete. Analyzing results...")

# Analyze
:fprof.profile()
:fprof.analyse([
  totals: true,
  sort: :acc,
  dest: ~c"benchmark/profile_results.txt"
])

IO.puts("\nResults saved to benchmark/profile_results.txt")
IO.puts("\nTop functions by accumulated time:")
:fprof.analyse([totals: true, sort: :acc, dest: []])

# Cleanup
GRPC.Server.stop_endpoint(Benchmark.ServerManager)

IO.puts("\n=== Profiling complete ===")
