defmodule GRPC.Client.Resolver do
  @moduledoc """
  Behaviour for gRPC client resolvers.
  """
  @type service_config :: GRPC.Client.ServiceConfig.t() | nil

  @callback resolve(String.t()) ::
              {:ok, %{addresses: list(map()), service_config: service_config()}}
              | {:error, term()}

  @behaviour GRPC.Client.Resolver

  @doc """
  Resolves a gRPC target string into a list of connection endpoints and an optional ServiceConfig.

  The `target` string can use one of the supported URI schemes:

    * `dns://[authority/]host[:port]` – resolves via DNS; looks up both A/AAAA records and optional `_grpc_config.<host>` TXT record.
    * `ipv4:addr[:port][,addr[:port],...]` – uses a fixed list of IPv4 addresses.
    * `ipv6:[addr][:port][,[addr][:port],...]` – uses a fixed list of IPv6 addresses.
    * `unix:/absolute_path` – connects via Unix domain socket.
    * `unix-abstract:name` – connects via abstract Unix socket (Linux only).
    * `vsock:cid:port` – connects via VSOCK (Linux only).
    * `xds:///name` – resolves via xDS control plane (Envoy/Istio/Traffic Director).

  If no scheme is specified, `dns` is assumed. Default ports:

    * `dns`, `ipv4`, `ipv6` → 50051
    * `xds` → 443

  Returns:

    * `{:ok, %{addresses: list(map()), service_config: GRPC.Client.ServiceConfig.t() | nil}}` on success
    * `{:error, reason}` on failure

  Each `address` map includes at least:

    * `:address` – host, IP, or socket path
    * `:port` – TCP port (if applicable)
    * additional fields may be present depending on the scheme (e.g., `:socket`, `:cid` for vsock).

  This function abstracts the resolution mechanism, allowing the gRPC client to obtain endpoints and service configuration regardless of the underlying target type.
  """
  @impl GRPC.Client.Resolver
  @spec resolve(String.t()) ::
          {:ok, %{addresses: list(map()), service_config: GRPC.Client.ServiceConfig.t()}}
          | {:error, term()}
  def resolve(target) do
    uri = URI.parse(target)
    scheme = uri.scheme || "dns"

    case scheme do
      "dns" ->
        GRPC.Client.Resolver.DNS.resolve(target)

      "ipv4" ->
        GRPC.Client.Resolver.IPv4.resolve(target)

      "ipv6" ->
        GRPC.Client.Resolver.IPv6.resolve(target)

      "unix" ->
        GRPC.Client.Resolver.Unix.resolve(target)

      "xds" ->
        GRPC.Client.Resolver.XDS.resolve(target)

      _ ->
        {:error, {:unknown_scheme, scheme}}
    end
  end
end
