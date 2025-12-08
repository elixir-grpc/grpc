defmodule Mix.Tasks.Benchmark.Profile do
  use Mix.Task
  require Logger

  @shortdoc "Profile the gRPC benchmark to find bottlenecks"
  
  @moduledoc """
  Profiles the gRPC benchmark using Erlang's profiling tools.
  
  Usage:
    mix benchmark.profile --adapter=thousand_island --requests=500
    mix benchmark.profile --adapter=cowboy --requests=500
  """

  def run(args) do
    # Disable logging to reduce noise
    Logger.configure(level: :error)
    
    Mix.Task.run("app.start")
    
    {opts, _, _} = OptionParser.parse(args, 
      switches: [adapter: :string, requests: :integer, port: :integer],
      aliases: [a: :adapter, r: :requests]
    )
    
    adapter_name = opts[:adapter] || "thousand_island"
    num_requests = opts[:requests] || 500
    port = opts[:port] || 10000
    
    adapter = case String.downcase(adapter_name) do
      "thousand_island" -> GRPC.Server.Adapters.ThousandIsland
      "cowboy" -> GRPC.Server.Adapters.Cowboy
      _ -> GRPC.Server.Adapters.ThousandIsland
    end
    
    # Start server
    IO.puts("Starting #{adapter_name} server on port #{port}...")
    
    server = %Grpc.Testing.ServerConfig{
      async_server_threads: 1,
      port: port
    }
    
    _server = Benchmark.ServerManager.start_server(server, adapter: adapter)
    Process.sleep(500)
    
    # Connect client
    {:ok, ch} = GRPC.Stub.connect("localhost:#{port}")
    
    # Prepare request
    payload_type = Grpc.Testing.PayloadType.value(:COMPRESSABLE)
    req = %Grpc.Testing.SimpleRequest{
      response_type: payload_type,
      response_size: 0,
      payload: %Grpc.Testing.Payload{type: payload_type, body: <<>>}
    }
    
    # Warm up
    IO.puts("Warming up with 100 requests...")
    Enum.each(1..100, fn _ ->
      Grpc.Testing.BenchmarkService.Stub.unary_call(ch, req)
    end)
    
    IO.puts("\n=== Profiling with :eprof (#{num_requests} requests) ===")
    IO.puts("This will show time spent in each function...\n")
    
    # Get the handler process pid - we need to profile the server side
    Process.sleep(100)
    
    # Profile using :eprof - it measures time, not just call counts
    spawn(fn ->
      Process.sleep(50)
      
      # Start profiling all processes
      :eprof.start()
      :eprof.start_profiling(:processes)
      
      # Let it run for the benchmark
      Process.sleep(div(num_requests * 2, 1))
      
      :eprof.stop_profiling()
      IO.puts("\n\n=== EPROF Analysis ===")
      :eprof.analyze([:total])
      :eprof.stop()
    end)
    
    # Run benchmark
    start_time = System.monotonic_time(:millisecond)
    Enum.each(1..num_requests, fn _ ->
      Grpc.Testing.BenchmarkService.Stub.unary_call(ch, req)
    end)
    end_time = System.monotonic_time(:millisecond)
    
    elapsed = end_time - start_time
    req_per_sec = num_requests / (elapsed / 1000)
    
    IO.puts("\nPerformance: #{Float.round(req_per_sec, 2)} req/s (#{num_requests} requests in #{elapsed}ms)")
    
    # Wait for profiling to complete
    Process.sleep(2000)
    
    IO.puts("\n=== Done! ===")
    IO.puts("Look for GRPC.Server.* modules above to see where time is spent.")
  end
end
