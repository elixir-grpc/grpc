defmodule GRPC.Client.Resolver do
  @moduledoc """
  Behaviour for gRPC client resolvers.

  A gRPC resolver is responsible for translating a **target string** into
  a list of connection endpoints (addresses) and an optional `ServiceConfig`.

  gRPC supports multiple naming schemes, allowing clients to connect
  to servers via DNS, fixed IPs, Unix domain sockets, or through
  service discovery/control planes like xDS.

  ## Target Syntax

  The gRPC target string uses URI-like syntax:

      <scheme>://<authority>/<path>   or   <scheme>:<path>

  ### Supported schemes

    * `dns://[authority/]host[:port]` – resolves via DNS, including:
      * A/AAAA records for IP addresses
      * Optional TXT record `_grpc_config.<host>` containing JSON ServiceConfig
    * `ipv4:addr[:port][,addr[:port],...]` – fixed list of IPv4 addresses
    * `ipv6:[addr][:port][,[addr][:port],...]` – fixed list of IPv6 addresses
    * `unix:/absolute_path` – Unix domain socket
    * `unix-abstract:name` – abstract Unix socket (Linux only)
    * `vsock:cid:port` – VSOCK endpoint (Linux only)
    * `xds:///name` – resolve via xDS control plane (Envoy/Istio/Traffic Director)

  If no scheme is specified, `dns` is assumed.

  ### Default ports

    * `dns`, `ipv4`, `ipv6` → 50051
    * `xds` → 443

  ## Resolver Output

  Returns:

    * `{:ok, %{addresses: list(map()), service_config: GRPC.Client.ServiceConfig.t() | nil}}`
      - `addresses` – list of endpoint maps containing the keys:
        - `:address` – host, IP, or socket path
        - `:port` – TCP port (if applicable)
        - may include additional scheme-specific fields, e.g., `:cid` for vsock
      - `service_config` – optional `ServiceConfig` parsed from DNS TXT or xDS

    * `{:error, reason}` on failure

  ## Purpose

  The resolver abstracts the underlying naming and service discovery mechanisms,
  allowing the gRPC client to obtain endpoints and service configuration consistently,
  regardless of whether the target is DNS, static IPs, a socket, or xDS.

  ## Reference

  For the official gRPC naming and resolver specification, see:

  [gRPC Naming Documentation](https://github.com/grpc/grpc/blob/master/doc/naming.md)
  """

  alias GRPC.Client.Resolver.DNS
  alias GRPC.Client.Resolver.IPv4
  alias GRPC.Client.Resolver.IPv6
  alias GRPC.Client.Resolver.Unix
  alias GRPC.Client.Resolver.XDS

  @type service_config :: GRPC.Client.ServiceConfig.t() | nil

  @callback resolve(String.t()) ::
              {:ok, %{addresses: list(map()), service_config: service_config()}}
              | {:error, term()}

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
  @spec resolve(String.t()) ::
          {:ok, %{addresses: list(map()), service_config: GRPC.Client.ServiceConfig.t()}}
          | {:error, term()}
  def resolve(target) do
    uri = URI.parse(target)
    scheme = uri.scheme || "dns"

    case scheme do
      "dns" ->
        DNS.resolve(target)

      "ipv4" ->
        IPv4.resolve(target)

      "ipv6" ->
        IPv6.resolve(target)

      "unix" ->
        Unix.resolve(target)

      "xds" ->
        XDS.resolve(target)

      "localhost" ->
        IPv4.resolve("ipv4:#{target}")

      nil ->
        IPv4.resolve("ipv4:#{target}")

      _ ->
        {:error, {:unknown_scheme, scheme}}
    end
  end
end
