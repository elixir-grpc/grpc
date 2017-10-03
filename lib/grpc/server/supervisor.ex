defmodule GRPC.Server.Supervisor do
  use Supervisor

  @default_adapter GRPC.Adapter.Cowboy

  def start_link(servers) do
    Supervisor.start_link(__MODULE__, servers, name: __MODULE__)
  end

  @spec init({module | [module], integer}) :: {:ok, tuple}
  @spec init({module | [module], integer, Keyword.t}) :: {:ok, tuple}
  def init({servers, port}) do
    init({servers, port, []})
  end
  def init({servers, port, opts}) do
    adapter = Keyword.get(opts, :adapter, @default_adapter)
    servers = GRPC.Server.servers_to_map(servers)
    children =
      if Application.get_env(:grpc, :start_server, false) do
        [adapter.child_spec(servers, port, opts)]
      else
        []
      end
    supervise(children, strategy: :one_for_one)
  end
end
