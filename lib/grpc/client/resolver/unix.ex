defmodule GRPC.Client.Resolver.Unix do
  @behaviour GRPC.Client.Resolver

  @impl GRPC.Client.Resolver
  def resolve(target) do
    # E.g.: "unix:///var/run/my.sock"
    uri = URI.parse(target)
    path = uri.path

    {:ok, %{addresses: [%{address: path, port: nil, socket: :unix}], service_config: nil}}
  end
end
