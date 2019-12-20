defmodule HelloworldApp do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    ca_cert_path = "/Users/justinyen/tmp/server_a_cacert.pem"
    cert_path = "/Users/justinyen/tmp/server_a_cert.pem"
    key_path = "/Users/justinyen/tmp/server_a_key.key"

    cred = GRPC.Credential.new(ssl: [certfile: cert_path,
                              keyfile: key_path,
                              cacertfile: ca_cert_path,
                              verify: :verify_peer,
                              fail_if_no_peer_cert: true
                              ])

    children = [
      supervisor(GRPC.Server.Supervisor, [{Helloworld.Endpoint, 50051, cred: cred}])
    ]
    
    IO.inspect cred
    opts = [strategy: :one_for_one, name: HelloworldApp]
    Supervisor.start_link(children, opts)
  end
end
