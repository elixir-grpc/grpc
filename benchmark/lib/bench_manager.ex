defmodule BenchManager do
  def start_server(%Grpc.Testing.ServerConfig{} = config) do
    cores = set_cores(config.core_limit)
    # get security
    payload_type = payload_type(config.payload_config)
    server = start_server(payload_type, config)
    Map.put(server, :cores, cores)
  end

  def start_server(:protobuf, config) do
    {:ok, pid, port} = GRPC.Server.start(Grpc.Testing.BenchmarkService.Server, config.port)
    %{port: port, pid: pid}
  end
  def start_server(_, config), do: raise GRPC.RPCError, status: :unimplemented

  def payload_type(nil), do: :protobuf
  def payload_type(config) do
    case config.payload do
      {:bytebuf_params, _} -> :bytebuf
      {:simple_params, _} -> :protobuf
      {:complex_params, _} -> :complex
      _ -> raise GRPC.RPCError, status: :invalid_argument
    end
  end

  def get_cores() do
    :erlang.system_info(:schedulers_online)
  end

  defp set_cores(core_limit) do
    if core_limit > 0 do
      :erlang.system_flag(:schedulers_online, core_limit)
      core_limit
    else
      get_cores()
    end
  end
end
