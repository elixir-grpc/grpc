defmodule GRPC.Client.Resolver do
  @moduledoc """
  Behaviour for gRPC client resolvers.

  A gRPC resolver is responsible for translating a target string into
  a list of connection endpoints (addresses) and an optional `GRPC.Client.ServiceConfig`.

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

  @callback init(target :: String.t(), opts :: keyword()) ::
              {:ok, state :: term()} | {:error, term()}

  @callback update(state :: term(), event :: term()) ::
              {:ok, state :: term()}

  @callback shutdown(state :: term()) :: :ok

  @optional_callbacks [init: 2, update: 2, shutdown: 1]

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
  def resolve(target) do
    uri = URI.parse(target)

    case uri.scheme do
      "localhost" ->
        IPv4.resolve("ipv4:#{target}")

      _ ->
        case resolver_for(target) do
          {:ok, mod} -> mod.resolve(target)
          {:error, reason} -> {:error, reason}
        end
    end
  end

  @doc """
  Initializes background re-resolution for `target`, delegating to the
  scheme-specific resolver when it implements the optional `c:init/2`
  callback (e.g. `GRPC.Client.Resolver.DNS` for periodic DNS refresh).

  Returns `{:ok, state}` where `state` is `nil` when the scheme has no
  background resolution. The state must be passed back to `update/2` and
  `shutdown/1`.
  """
  def init(target, opts) do
    case resolver_for(target) do
      {:ok, mod} ->
        if Code.ensure_loaded?(mod) and function_exported?(mod, :init, 2) do
          case apply(mod, :init, [target, opts]) do
            {:ok, nil} -> {:ok, nil}
            {:ok, inner_state} -> {:ok, {mod, inner_state}}
            {:error, reason} -> {:error, reason}
          end
        else
          {:ok, nil}
        end

      {:error, _reason} ->
        {:ok, nil}
    end
  end

  @doc """
  Forwards an event (e.g. `:resolve_now`) to the underlying resolver's
  `c:update/2` callback.
  """
  def update({mod, inner_state}, event) do
    {:ok, new_inner_state} = mod.update(inner_state, event)
    {:ok, {mod, new_inner_state}}
  end

  def update(state, _event), do: {:ok, state}

  @doc """
  Shuts down background re-resolution started by `init/2`.
  """
  def shutdown({mod, inner_state}) do
    if Code.ensure_loaded?(mod) and function_exported?(mod, :shutdown, 1) do
      apply(mod, :shutdown, [inner_state])
    else
      :ok
    end
  end

  def shutdown(_state), do: :ok

  defp resolver_for(target) do
    uri = URI.parse(target)
    scheme = uri.scheme || "dns"

    case scheme do
      "dns" -> {:ok, DNS}
      "ipv4" -> {:ok, IPv4}
      "ipv6" -> {:ok, IPv6}
      "unix" -> {:ok, Unix}
      "xds" -> {:ok, XDS}
      "localhost" -> {:ok, IPv4}
      _ -> {:error, {:unknown_scheme, scheme}}
    end
  end
end
