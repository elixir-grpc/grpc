defmodule Interop.App do
  use Application

  def start(_type, _args) do
    children = [{GRPC.Server.Supervisor, endpoint: Interop.Endpoint, port: 10000}]

    opts = [strategy: :one_for_one, name: __MODULE__]
    Supervisor.start_link(children, opts)
  end
end
