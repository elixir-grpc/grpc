defmodule Mix.Tasks.Benchmark.Worker do
  @moduledoc """
  Starts a gRPC worker server for benchmarking.
  
  ## Usage
  
      mix benchmark.worker --port=10000
  
  ## Options
  
    * `--port` - Port to listen on (required)
  
  """
  use Mix.Task

  require Logger

  @shortdoc "Starts a gRPC worker server"

  @impl Mix.Task
  def run(args) do
    Mix.Task.run("app.start")

    {parsed, _remaining, _invalid} =
      OptionParser.parse(args, strict: [port: :integer])

    port = Keyword.get(parsed, :port)

    unless port do
      Mix.raise("Port is required. Usage: mix benchmark.worker --port=10000")
    end

    Logger.info("Starting worker server on port #{port}")

    {:ok, _pid, actual_port} =
      GRPC.Server.start(Grpc.Testing.WorkerService.Server, port, local: %{main_pid: self()})

    Logger.info("Worker server started on port #{actual_port}")

    # Keep the process alive and wait for messages
    loop()
  end

  defp loop do
    receive do
      {:quit, _} ->
        Logger.info("Received quit signal, stopping worker server...")
        Process.sleep(1000)
        GRPC.Server.stop(Grpc.Testing.WorkerService.Server)
        :ok

      msg ->
        Logger.debug("Received message: #{inspect(msg)}")
        Process.sleep(100)
        loop()
    end
  end
end
