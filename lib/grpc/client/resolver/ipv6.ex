defmodule GRPC.Client.Resolver.IPv6 do
  @moduledoc """
  Resolver for gRPC clients connecting to one or more IPv6 addresses.

  This resolver handles target strings using the `ipv6` URI scheme, which
  allows specifying one or multiple IPv6 addresses with optional ports.

  ## Target format

      ipv6:[addr][:port][,[addr][:port],...]

  - IPv6 addresses **must** be enclosed in square brackets (`[...]`).
  - The port is optional; if not provided, the default port is `443`.
  - Multiple addresses can be comma-separated.
  - `service_config` is always `nil` as IPv6 literals do not support DNS TXT or xDS.

  ## Examples

      # Single IPv6 with explicit port
      target = "ipv6:[2607:f8b0:400e:c00::ef]:443"
      {:ok, %{addresses: addresses, service_config: nil}} =
        GRPC.Client.Resolver.IPv6.resolve(target)
      addresses
      # => [%{address: "2607:f8b0:400e:c00::ef", port: 443}]

      # Multiple IPv6 addresses, some with default port
      target = "ipv6:[2607:f8b0:400e:c00::ef]:443,[::1]:50051,[::2]"
      {:ok, %{addresses: addresses, service_config: nil}} =
        GRPC.Client.Resolver.IPv6.resolve(target)
      addresses
      # => [
      #   %{address: "2607:f8b0:400e:c00::ef", port: 443},
      #   %{address: "::1", port: 50051},
      #   %{address: "::2", port: 443}
      # ]

  See the gRPC naming documentation for more information: 
  https://github.com/grpc/grpc/blob/master/doc/naming.md
  """

  @behaviour GRPC.Client.Resolver

  @impl GRPC.Client.Resolver
  def start(_args), do: :ok

  @impl GRPC.Client.Resolver
  def resolve(target) do
    uri = URI.parse(target)
    addresses_str = uri.path

    addresses =
      String.split(addresses_str, ",")
      |> Enum.map(fn entry ->
        case Regex.run(~r/\[(?<ip>.*?)\](?::(?<port>\d+))?/, entry) do
          [_, ip, port] ->
            %{address: ip, port: String.to_integer(port)}

          [_, ip] ->
            %{address: ip, port: 443}

          _ ->
            {:error, :invalid_ipv6}
        end
      end)

    {:ok, %{addresses: addresses, service_config: nil}}
  end
end
