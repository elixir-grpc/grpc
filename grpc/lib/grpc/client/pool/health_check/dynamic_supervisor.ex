defmodule GRPC.Client.Pool.HealthCheck.DynamicSupervisor do
  @moduledoc """
  Supervises health-check GenServers, restarts them in case of crush,
  allows starting new ones dynamically
  """

  use DynamicSupervisor

  alias GRPC.Client.Pool.Config
  alias GRPC.Client.Pool.HealthCheck.Server
  alias GRPC.Client.Pool.Server.State.Channel

  @spec start_link(Config.t()) :: Supervisor.on_start()
  def start_link(%Config{} = config) do
    DynamicSupervisor.start_link(__MODULE__, config, name: via_tuple(config.pool_ref))
  end

  @impl DynamicSupervisor
  def init(_args), do: DynamicSupervisor.init(strategy: :one_for_one)

  @spec start(Channel.id(), Process.dest(), reference()) :: DynamicSupervisor.on_start_child()
  def start(channel_id, conn_pid, pool_ref) do
    [{dynamic_supervisor_pid, _value}] =
      Registry.lookup(GRPC.Client.Pool.Registry, {__MODULE__, pool_ref})

    DynamicSupervisor.start_child(
      dynamic_supervisor_pid,
      {Server, %{channel_id: channel_id, conn_pid: conn_pid, pool_ref: pool_ref}}
    )
  end

  defp via_tuple(pool_ref) do
    {:via, Registry, {GRPC.Client.Pool.Registry, {__MODULE__, pool_ref}}}
  end
end
