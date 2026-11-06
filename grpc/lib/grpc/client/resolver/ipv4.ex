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
  - A host name that has AAAA records and no A records resolves to its IPv6 address.
    The name stays in the `:hostname` key.

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
        resolve_host(ip, String.to_integer(port))
      end)

    {:ok, %{addresses: addresses, service_config: nil}}
  end

  # The adapters open IPv4 sockets for names. If the name has no A record but
  # has an AAAA record, hand over the IPv6 address and keep the name for TLS.
  defp resolve_host(host, port) do
    charlist = String.to_charlist(host)

    with {:error, _} <- :inet.parse_address(charlist),
         {:error, _} <- :inet.getaddr(charlist, :inet),
         {:ok, ipv6} <- :inet.getaddr(charlist, :inet6) do
      %{address: :inet.ntoa(ipv6) |> to_string(), port: port, hostname: host}
    else
      _ -> %{address: host, port: port}
    end
  end
end
