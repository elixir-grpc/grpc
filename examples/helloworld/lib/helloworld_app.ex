defmodule HelloworldApp do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children =
      if Application.get_env(:helloworld, :start_server, false) do
        [supervisor(GRPC.Server.Supervisor, [{Helloworld.Greeter.Server, 50051}])]
      else
        []
      end

    opts = [strategy: :one_for_one, name: HelloworldApp]
    Supervisor.start_link(children, opts)
  end
end
