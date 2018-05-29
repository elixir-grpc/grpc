defmodule Benchmark.ServerManager do
  def start_server(%Grpc.Testing.ServerConfig{} = config) do
    # get security
    payload_type = Benchmark.Manager.payload_type(config.payload_config)
    start_server(payload_type, config)
  end

  def start_server(:protobuf, config) do
    {:ok, pid, port} = GRPC.Server.start(Grpc.Testing.BenchmarkService.Server, config.port)
    %{port: port, pid: pid}
  end

  def start_server(_, _), do: raise(GRPC.RPCError, status: :unimplemented)
end
