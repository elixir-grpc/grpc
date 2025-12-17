defmodule GRPC.Server.Adapters.Cowboy.Supervisor do
  @moduledoc """
  Supervisor for Cowboy adapter.

  This supervisor manages the lifecycle of the Cowboy/Ranch server and
  provides isolation from other adapters. It ensures the Task.Supervisor
  is started before the Cowboy server.

  ## Supervision Tree

  ```
  GRPC.Server.Supervisor
  └── Cowboy.Supervisor (this module)
      ├── Task.Supervisor (for stream tasks)
      └── Ranch Listener (Cowboy server)
  ```
  """

  use Supervisor
  require Logger

  @doc """
  Starts the Cowboy supervisor.

  ## Options

    * `:endpoint` - The endpoint module (optional)
    * `:servers` - Map of service name => server modules
    * `:port` - The port to listen on
    * `:adapter_opts` - Cowboy-specific options
    * `:cred` - SSL credentials (optional, for HTTPS)
  """
  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(opts) do
    endpoint = opts[:endpoint]
    servers = opts[:servers]
    port = opts[:port]

    # Get the cowboy child spec
    cowboy_child_spec = GRPC.Server.Adapters.Cowboy.child_spec(endpoint, servers, port, opts)

    children = [
      {Task.Supervisor, name: GRPC.Server.StreamTaskSupervisor},
      cowboy_child_spec
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
