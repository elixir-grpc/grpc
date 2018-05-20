defmodule Interop.App do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      supervisor(GRPC.Server.Supervisor, [{Interop.Endpoint, 10000}])
    ]

    GRPCPrometheus.ServerInterceptor.setup()
    GRPCPrometheus.ClientInterceptor.setup()
    :prometheus_httpd.start()

    opts = [strategy: :one_for_one, name: __MODULE__]
    Supervisor.start_link(children, opts)
  end
end
