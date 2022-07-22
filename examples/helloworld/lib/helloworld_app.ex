defmodule HelloworldApp do
  use Application

  def start(_type, _args) do
    children = [
      {GRPC.Server.Supervisor, {Helloworld.Endpoint, 50051}}
    ]

    opts = [strategy: :one_for_one, name: HelloworldApp]
    Supervisor.start_link(children, opts)
  end
end
