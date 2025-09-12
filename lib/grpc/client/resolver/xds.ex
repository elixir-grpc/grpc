defmodule GRPC.Client.Resolver.XDS do
  @behaviour GRPC.Client.Resolver

  @impl GRPC.Client.Resolver
  def resolve(_target) do
    # E.g.: "xds:///myservice"
    {:error, :not_implemented}
  end
end
