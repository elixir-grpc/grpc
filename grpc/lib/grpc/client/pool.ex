defmodule GRPC.Client.Pool do
  @moduledoc false

  alias GRPC.Channel
  alias GRPC.Client.Pool.Config

  @call_timeout 7_000

  @spec start_for_address(Channel.t(), term(), non_neg_integer(), keyword()) ::
          {:ok, Channel.t()} | {:error, any()}
  def start_for_address(%Channel{} = vc, host, port, norm_opts) do
    pool_opts = norm_opts[:pool]
    address_pool_ref = make_ref()

    config = %Config{
      pool_ref: address_pool_ref,
      channel: %Channel{vc | host: host, port: port},
      pool_size: Map.get(pool_opts, :size, 1),
      max_pool_overflow: Map.get(pool_opts, :max_overflow, 0),
      max_client_streams_per_connection: Map.get(pool_opts, :max_streams),
      adapter_opts: norm_opts[:adapter_opts] || [],
      health_check_enabled: Map.get(pool_opts, :health_check_enabled, false)
    }

    case DynamicSupervisor.start_child(
           GRPC.Client.Supervisor,
           {GRPC.Client.Pool.Supervisor, config}
         ) do
      {:ok, _} ->
        {:ok, %Channel{vc | host: host, port: port, pool: address_pool_ref}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec stop_for_address(reference()) :: :ok
  def stop_for_address(pool_ref) do
    case Registry.lookup(GRPC.Client.Pool.Registry, {GRPC.Client.Pool.Supervisor, pool_ref}) do
      [{sup_pid, _}] -> Supervisor.stop(sup_pid, :normal)
      [] -> :ok
    end
  rescue
    _ -> :ok
  end

  @spec checkout(reference()) :: {GRPC.Client.Pool.Server.State.Channel.t(), Channel.t()} | nil
  def checkout(pool_ref) do
    case Registry.lookup(GRPC.Client.Pool.Registry, {GRPC.Client.Pool.Server, pool_ref}) do
      [{pool_pid, _}] ->
        case GenServer.call(pool_pid, :take_channel, @call_timeout) do
          nil -> nil
          %GRPC.Client.Pool.Server.State.Channel{channel: channel} = wrapped -> {wrapped, channel}
        end

      [] ->
        nil
    end
  end

  @spec checkin(reference(), GRPC.Client.Pool.Server.State.Channel.t()) :: :ok
  def checkin(pool_ref, wrapped_channel) do
    case Registry.lookup(GRPC.Client.Pool.Registry, {GRPC.Client.Pool.Server, pool_ref}) do
      [{pool_pid, _}] ->
        GenServer.cast(pool_pid, {:return_channel, wrapped_channel, self()})

      [] ->
        :ok
    end
  end
end
