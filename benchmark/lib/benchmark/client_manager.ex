defmodule Benchmark.ClientManager do
  use GenServer

  def start_client(%Grpc.Testing.ClientConfig{} = config) do
    {:ok, pid} = GenServer.start_link(__MODULE__, config)
    %{pid: pid}
  end

  def init(config) do
    # get security
    _payload_type = Benchmark.Manager.payload_type(config.payload_config)

    channels =
      0..(config.client_channels - 1)
      |> Enum.zip(config.server_targets)
      |> Enum.map(fn {_, server} -> new_client(server) end)

    workers = Enum.map(channels, &start_worker(config, &1)) |> List.flatten()

    {:ok, %{workers: workers}}
  end

  def handle_call(:get_stats, _from, s) do
    {:reply, nil, s}
  end

  def handle_cast({:track_rpc, dur}, s) do
    {:noreply, s}
  end

  defp new_client(addr) do
    {:ok, ch} = GRPC.Stub.connect(addr)
    ch
  end

  defp start_worker(%{rpc_type: rpc_type} = config, channel) do
    if elem(config.load_params.load, 0) != :closed_loop,
      do:
        raise(
          GRPC.RPCError,
          status: :unimplemented,
          message: "load #{inspect(config.load_params.load)} not support"
        )

    rpc_type = Grpc.Testing.RpcType.key(rpc_type)

    if rpc_type != :UNARY,
      do:
        raise(
          GRPC.RPCError,
          status: :unimplemented,
          message: "rpc_type #{inspect(rpc_type)} not support"
        )

    rpcs_per_conn = config.outstanding_rpcs_per_channel

    payload =
      client_payload(config.payload_config)
      |> Map.put(:rpc_type, rpc_type)

    Enum.map(1..rpcs_per_conn, fn _idx ->
      {:ok, pid} = GenServer.start_link(Benchmark.ClientWorker, {channel, payload, self()})
      pid
    end)
  end

  def client_payload(nil), do: raise(GRPC.RPCError, status: :unimplemented)

  def client_payload(config) do
    case config.payload do
      {:simple_params, payload} ->
        %{req_size: payload.req_size, resp_size: payload.resp_size, type: :protobuf}

      _ ->
        raise(GRPC.RPCError, status: :unimplemented)
    end
  end
end
