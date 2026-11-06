defmodule GRPC.Client.Pool.Supervisor do
  @moduledoc false

  use Supervisor

  alias GRPC.Client.Pool.Config

  def child_spec(%Config{} = config) do
    %{
      id: {__MODULE__, config.pool_ref},
      start: {__MODULE__, :start_link, [config]},
      type: :supervisor,
      restart: :transient
    }
  end

  @spec start_link(Config.t()) :: :ignore | {:error, any()} | {:ok, pid()}
  def start_link(%Config{} = config) do
    Supervisor.start_link(__MODULE__, config, name: via_tuple(config.pool_ref))
  end

  @impl Supervisor
  def init(%Config{} = config) do
    pool_ref = config.pool_ref

    Supervisor.init(
      [
        Supervisor.child_spec(
          {GRPC.Client.Pool.HealthCheck.DynamicSupervisor, config},
          id: {GRPC.Client.Pool.HealthCheck.DynamicSupervisor, pool_ref}
        ),
        Supervisor.child_spec(
          {GRPC.Client.Pool.Server, config},
          id: {GRPC.Client.Pool.Server, pool_ref}
        )
      ],
      strategy: :one_for_all
    )
  end

  defp via_tuple(pool_ref) do
    {:via, Registry, {GRPC.Client.Pool.Registry, {__MODULE__, pool_ref}}}
  end
end
