defmodule HelloworldStreams.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      HelloworldStreams.Utils.Transformer,
      {
        GRPC.Server.Supervisor,
        endpoint: HelloworldStreams.Endpoint, port: 50053, start_server: true
      }
    ]

    opts = [strategy: :one_for_one, name: HelloworldStreams.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
