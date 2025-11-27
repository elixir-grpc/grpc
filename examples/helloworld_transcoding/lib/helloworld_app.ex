defmodule HelloworldApp do
  use Application

  def start(_type, _args) do
    children = [
      {GRPC.Server.Supervisor, endpoint: Helloworld.Endpoint, port: 50051, start_server: true}
    ]

    opts = [strategy: :one_for_one, name: HelloworldApp]
    Supervisor.start_link(children, opts)
  end
end
