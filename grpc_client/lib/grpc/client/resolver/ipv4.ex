defmodule GRPC.Client.Resolver.IPv4 do
  @moduledoc """
  Resolver for gRPC clients connecting to one or more IPv4 addresses.

  This resolver handles target strings using the `ipv4` URI scheme, which
  allows specifying one or multiple IPv4 addresses with explicit ports.

  ## Target format

      ipv4:addr:port[,addr:port,...]

  - IPv4 addresses must include a port.
  - Multiple addresses can be comma-separated.
  - `service_config` is always `nil` as literal IPv4 addresses do not support DNS TXT or xDS.

  ## Examples

      # Single IPv4
      target = "ipv4:10.0.0.1:50051"
      {:ok, %{addresses: addresses, service_config: nil}} =
        GRPC.Client.Resolver.IPv4.resolve(target)
      addresses
      # => [%{address: "10.0.0.1", port: 50051}]

      # Multiple IPv4 addresses
      target = "ipv4:10.0.0.1:50051,10.0.0.2:50052"
      {:ok, %{addresses: addresses, service_config: nil}} =
        GRPC.Client.Resolver.IPv4.resolve(target)
      addresses
      # => [
      #   %{address: "10.0.0.1", port: 50051},
      #   %{address: "10.0.0.2", port: 50052}
      # ]

  See the gRPC naming documentation for more information: 
  https://github.com/grpc/grpc/blob/master/doc/naming.md
  """

  @behaviour GRPC.Client.Resolver

  @impl GRPC.Client.Resolver
  def resolve(target) do
    uri = URI.parse(target)
    addrs_str = uri.path

    addresses =
      String.split(addrs_str, ",", trim: true)
      |> Enum.map(fn addr ->
        [ip, port] = String.split(addr, ":", trim: true, parts: 2)
        %{address: ip, port: String.to_integer(port)}
      end)

    {:ok, %{addresses: addresses, service_config: nil}}
  end
end
