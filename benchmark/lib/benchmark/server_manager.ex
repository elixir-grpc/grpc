defmodule Benchmark.ServerManager do
  def start_server(%Grpc.Testing.ServerConfig{} = config) do
    # get security
    payload_type = Benchmark.Manager.payload_type(config.payload_config)
    start_server(payload_type, config)
  end

  def start_server(:protobuf, config) do
    cores = Benchmark.Manager.set_cores(config.core_limit)
    {:ok, pid, port} = GRPC.Server.start(Grpc.Testing.BenchmarkService.Server, config.port)

    # relative util will be returned next time calling
    :cpu_sup.util()

    %Benchmark.Server{
      cores: cores,
      port: port,
      pid: pid,
      init_time: Time.utc_now()
    }
  end

  def start_server(_, _), do: raise(GRPC.RPCError, status: :unimplemented)
end
