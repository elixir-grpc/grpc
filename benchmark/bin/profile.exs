#!/usr/bin/env elixir

# Profile script to find bottlenecks in gRPC server
Mix.install([
  {:grpc, path: Path.expand("../..", __DIR__)},
  {:thousand_island, "~> 1.0"}
])

# Start the server
server_config =
  Grpc.Testing.ServerConfig.new(
    async_server_threads: 1,
    port: 10000
  )

{:ok, _server_pid} = Benchmark.ServerManager.start_server(server_config)
Process.sleep(1000)

# Connect client
{:ok, ch} = GRPC.Stub.connect("localhost:10000")

payload_type = Grpc.Testing.PayloadType.value(:COMPRESSABLE)
req = Grpc.Testing.SimpleRequest.new(
  response_type: payload_type,
  response_size: 0,
  payload: Grpc.Testing.Payload.new(type: payload_type, body: <<>>)
)

# Warm up
IO.puts("Warming up...")
Enum.each(1..100, fn _ ->
  Grpc.Testing.BenchmarkService.Stub.unary_call(ch, req)
end)

IO.puts("\nStarting profiling with :fprof...")

# Profile with fprof
:fprof.trace([:start, {:procs, :all}])

# Run workload
Enum.each(1..1000, fn _ ->
  Grpc.Testing.BenchmarkService.Stub.unary_call(ch, req)
end)

:fprof.trace(:stop)
:fprof.profile()
:fprof.analyse([:totals, {:sort, :acc}, {:dest, ~c"fprof_analysis.txt"}])

IO.puts("fprof analysis written to fprof_analysis.txt")

# Now profile with eprof for better overview
IO.puts("\nStarting profiling with :eprof...")

:eprof.start()
:eprof.start_profiling([self()])

Enum.each(1..1000, fn _ ->
  Grpc.Testing.BenchmarkService.Stub.unary_call(ch, req)
end)

:eprof.stop_profiling()
:eprof.analyze([:total, {:sort, :time}])
:eprof.log(~c"eprof_analysis.txt")

IO.puts("\neprof analysis written to eprof_analysis.txt")

# Analyze specific modules
IO.puts("\nAnalyzing ThousandIsland adapter modules...")
:eprof.start_profiling([self()])

Enum.each(1..1000, fn _ ->
  Grpc.Testing.BenchmarkService.Stub.unary_call(ch, req)
end)

:eprof.stop_profiling()
:eprof.analyze([
  :total,
  {:sort, :time},
  {:filter, [
    {GRPC.Server.Adapters.ThousandIsland.Handler, :_, :_},
    {GRPC.Server.HTTP2.Connection, :_, :_},
    {GRPC.Server.HTTP2.Dispatcher, :_, :_},
    {GRPC.Server.Cache, :_, :_}
  ]}
])

IO.puts("\nDone! Check fprof_analysis.txt and eprof_analysis.txt for details.")
