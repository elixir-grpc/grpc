defmodule Routeguide.App do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      supervisor(RouteGuide.Data, []),
      supervisor(GRPC.Server.Supervisor, [{Routeguide.RouteGuide.Server, 10000}])
    ]

    opts = [strategy: :one_for_one, name: Routeguide]
    Supervisor.start_link(children, opts)
  end
end
