defmodule GRPC.Server.Supervisor do
  use Supervisor

  @default_adapter GRPC.Adapter.Cowboy

  def start_link(server) do
    Supervisor.start_link(__MODULE__, server, name: __MODULE__)
  end

  def init({server, port}) do
    init({server, port, []})
  end
  def init({server, port, opts}) do
    adapter = Keyword.get(opts, :adapter, @default_adapter)
    children =
      if Application.get_env(:grpc, :start_server, false) do
        [adapter.child_spec(server, port, opts)]
      else
        []
      end
    supervise(children, strategy: :one_for_one)
  end
end
