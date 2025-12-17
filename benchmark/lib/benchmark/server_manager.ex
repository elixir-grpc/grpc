defmodule Benchmark.ServerManager do
  def start_server(%Grpc.Testing.ServerConfig{} = config, opts \\ []) do
    # get security
    payload_type = Benchmark.Manager.payload_type(config.payload_config)
    start_server(payload_type, config, opts)
  end

  def start_server(:protobuf, config, opts) do
    cores = Benchmark.Manager.set_cores(config.core_limit)

    # Extract adapter option, default to Cowboy
    adapter = Keyword.get(opts, :adapter, GRPC.Server.Adapters.Cowboy)
    adapter_name = adapter |> Module.split() |> List.last()

    IO.puts("Starting server with #{adapter_name} adapter on port #{config.port}...")

    {:ok, pid, port} =
      GRPC.Server.start(
        Grpc.Testing.BenchmarkService.Server,
        config.port,
        adapter: adapter
      )

    %Benchmark.Server{
      cores: cores,
      port: port,
      pid: pid,
      init_time: Time.utc_now(),
      init_rusage: Benchmark.Syscall.getrusage()
    }
  end

  def start_server(_, _, _), do: raise(GRPC.RPCError, status: :unimplemented)

  def stop_server(%Benchmark.Server{} = _server, opts \\ []) do
    adapter = Keyword.get(opts, :adapter, GRPC.Server.Adapters.Cowboy)
    GRPC.Server.stop(Grpc.Testing.BenchmarkService.Server, adapter: adapter)
  end
end
