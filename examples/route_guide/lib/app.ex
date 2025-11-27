defmodule Routeguide.App do
  use Application

  @cert_path Path.expand("./tls/server1.pem", :code.priv_dir(:route_guide))
  @key_path Path.expand("./tls/server1.key", :code.priv_dir(:route_guide))

  def start(_type, _args) do
    children = [
      RouteGuide.Data,
      {GRPC.Server.Supervisor, start_args()}
    ]

    opts = [strategy: :one_for_one, name: Routeguide]
    Supervisor.start_link(children, opts)
  end

  defp start_args do
    opts = [endpoint: Routeguide.Endpoint, port: 10000, start_server: true]

    if System.get_env("TLS") do
      cred = GRPC.Credential.new(ssl: [certfile: @cert_path, keyfile: @key_path])
      Keyword.put(opts, :cred, cred)
    else
      opts
    end
  end
end
