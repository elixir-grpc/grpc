defmodule Mix.Tasks.Benchmark.Test do
  @moduledoc """
  Runs a simple gRPC benchmark test.
  
  This task starts a benchmark server and client, runs performance tests,
  and reports statistics.
  
  ## Usage
  
      mix benchmark.test
  
  ## Options
  
    * `--port` - Server port (default: 10000)
    * `--requests` - Number of requests to send (default: 1000)
  
  """
  use Mix.Task

  require Logger

  @shortdoc "Runs a gRPC benchmark test"

  @impl Mix.Task
  def run(args) do
    Mix.Task.run("app.start")

    {parsed, _remaining, _invalid} =
      OptionParser.parse(args,
        strict: [port: :integer, requests: :integer]
      )

    port = Keyword.get(parsed, :port, 10000)
    num_requests = Keyword.get(parsed, :requests, 1000)

    Logger.info("Starting benchmark test on port #{port}")

    # Configure and start server
    server = %Grpc.Testing.ServerConfig{
      async_server_threads: 1,
      port: port,
      channel_args: [
        %Grpc.Testing.ChannelArg{
          name: "grpc.optimization_target",
          value: {:str_value, "latency"}
        }
      ]
    }

    Logger.info("Starting server...")
    server = Benchmark.ServerManager.start_server(server)
    Logger.info("Server started: #{inspect(server)}")

    # Configure client
    client = %Grpc.Testing.ClientConfig{
      async_client_threads: 1,
      channel_args: [
        %Grpc.Testing.ChannelArg{
          name: "grpc.optimization_target",
          value: {:str_value, "latency"}
        }
      ],
      client_channels: 1,
      histogram_params: %Grpc.Testing.HistogramParams{
        max_possible: 6.0e10,
        resolution: 0.01
      },
      load_params: %Grpc.Testing.LoadParams{
        load: {:closed_loop, %Grpc.Testing.ClosedLoopParams{}}
      },
      outstanding_rpcs_per_channel: 1,
      payload_config: %Grpc.Testing.PayloadConfig{
        payload: {:simple_params, %Grpc.Testing.SimpleProtoParams{}}
      },
      server_targets: ["localhost:#{port}"]
    }

    Logger.info("Starting client manager...")
    manager = Benchmark.ClientManager.start_client(client)
    Logger.info("Client manager started")

    # Prepare request
    payload_type = Grpc.Testing.PayloadType.value(:COMPRESSABLE)
    resp_size = 0
    req_size = 0

    payload = %Grpc.Testing.Payload{
      type: payload_type,
      body: List.duplicate(<<0>>, req_size)
    }

    req = %Grpc.Testing.SimpleRequest{
      response_type: payload_type,
      response_size: resp_size,
      payload: payload
    }

    # Connect and warm up
    Logger.info("Connecting to server...")
    {:ok, ch} = GRPC.Stub.connect("localhost:#{port}")
    
    Logger.info("Warming up...")
    Grpc.Testing.BenchmarkService.Stub.unary_call(ch, req)

    # Run benchmark
    Logger.info("Running #{num_requests} requests...")
    start_time = System.monotonic_time(:microsecond)

    Enum.each(1..num_requests, fn _ ->
      Grpc.Testing.BenchmarkService.Stub.unary_call(ch, req)
    end)

    end_time = System.monotonic_time(:microsecond)
    elapsed_us = end_time - start_time
    elapsed_ms = elapsed_us / 1000
    elapsed_s = elapsed_ms / 1000

    # Get stats
    Process.sleep(500)
    stats = Benchmark.ClientManager.get_stats(manager, false)

    # Report results
    Logger.info("=" |> String.duplicate(60))
    Logger.info("Benchmark Results")
    Logger.info("=" |> String.duplicate(60))
    Logger.info("Total requests: #{num_requests}")
    Logger.info("Total time: #{Float.round(elapsed_s, 3)} seconds")
    Logger.info("Requests per second: #{Float.round(num_requests / elapsed_s, 2)}")
    Logger.info("Average latency: #{Float.round(elapsed_ms / num_requests, 3)} ms")
    Logger.info("")
    Logger.info("Client Stats:")
    IO.inspect(stats, label: "Stats", pretty: true)
    Logger.info("=" |> String.duplicate(60))

    :ok
  end
end
