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
    children = [adapter.child_spec(server, port, opts)]
    supervise(children, strategy: :one_for_one)
  end
end
