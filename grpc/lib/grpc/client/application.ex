defmodule GRPC.Client.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    supervisor_config = Application.get_env(:grpc, __MODULE__, [])

    children = [
      {Registry, [keys: :unique, name: GRPC.Client.Registry]},
      {DynamicSupervisor,
       [
         name: GRPC.Client.Supervisor,
         hibernate_after: Keyword.get(supervisor_config, :hibernate_after, 15_000),
         spawn_opt: [fullsweep_after: Keyword.get(supervisor_config, :fullsweep_after, 20)]
       ]}
    ]

    opts = [strategy: :one_for_one, name: GRPC.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
