defmodule Benchmark.ClientManager do
  alias Benchmark.ClientWorker

  def start_client(%Grpc.Testing.ClientConfig{} = config) do
    # get security
    _payload_type = Benchmark.Manager.payload_type(config.payload_config)

    channels =
      0..(config.client_channels - 1)
      |> Enum.zip(config.server_targets)
      |> Enum.map(fn {_, server} -> new_client(server) end)

    perform_rpcs(config, channels)
  end

  defp new_client(addr) do
    {:ok, conn} = GRPC.Stub.connect(addr)
    conn
  end

  def perform_rpcs(config, channels) do
    payload = client_payload(config.payload_config)

    case config.load_params.load do
      {:closed_loop, _} ->
        :ok

      _ ->
        raise GRPC.RPCError, status: :unimplemented
    end

    rpcs_per_conn = config.outstanding_rpcs_per_channel

    case Grpc.Testing.RpcType.key(config.rpc_type) do
      :UNARY ->
        close_loop_unary(channels, rpcs_per_conn, payload)

      _ ->
        :ok
    end
  end

  def client_payload(nil), do: %{}

  def client_payload(config) do
    case config.payload do
      {:simple_params, payload} ->
        %{req_size: payload.req_size, resp_size: payload.resp_size, type: :protobuf}

      _ ->
        %{}
    end
  end

  def close_loop_unary(channels, rpcs_per_conn, payload) do
    Enum.each(channels, fn conn ->
      Enum.each(0..(rpcs_per_conn - 1), fn _idx ->
        Task.async(ClientWorker, :unary_loop, [conn, payload])
      end)
    end)
  end
end
