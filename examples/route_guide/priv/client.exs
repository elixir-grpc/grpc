opts =
  if System.get_env("TLS") do
    ca_path = Path.expand("./tls/ca.pem", :code.priv_dir(:route_guide))
    cred = GRPC.Credential.client_tls(ca_path)
    [cred: cred]
  else
    []
  end
{:ok, channel} = GRPC.Stub.connect("localhost:10000", opts)
RouteGuide.Client.main(channel)
