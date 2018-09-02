defmodule Benchmark.Manager do
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

  def set_cores(core_limit) do
    if core_limit > 0 do
      :erlang.system_flag(:schedulers_online, core_limit)
      core_limit
    else
      get_cores()
    end
  end
end
