defmodule GRPC.Client.Resolver.DNS do
  @moduledoc """
  DNS Resolver for gRPC targets, supporting dynamic updates via a GenServer.

  Resolves `dns://host[:port]` targets. Fetches A/AAAA records and optional
  `_grpc_config.<host>` TXT records for ServiceConfig.

  This implementation maintains an internal cache of addresses and service config,
  and refreshes them periodically.
  """
  @behaviour GRPC.Client.Resolver

  alias GRPC.Client.ServiceConfig

  @impl GRPC.Client.Resolver
  def resolve(target) do
    uri = URI.parse(target)
    host = uri.host || target
    port = uri.port || 50051

    with {:ok, addresses} <- lookup_addresses(host) do
      addrs =
        Enum.map(addresses, fn ip ->
          %{address: :inet.ntoa(ip) |> to_string(), port: port}
        end)

      case lookup_service_config(host) do
        {:ok, txt_records} ->
          service_config_json = extract_service_config(txt_records)

          {:ok,
           %{
             addresses: addrs,
             service_config: ServiceConfig.parse(service_config_json)
           }}

        :no_config ->
          {:ok, %{addresses: addrs, service_config: nil}}

        {:error, reason} ->
          {:error, {:dns_error, reason}}
      end
    else
      {:error, reason} -> {:error, {:dns_error, reason}}
    end
  end

  defp lookup_addresses(host) do
    case lookup_addresses(host, :a) do
      {:ok, [_ | _] = addrs} -> {:ok, addrs}
      {:ok, []} -> lookup_addresses(host, :aaaa)
      other -> other
    end
  end

  defp lookup_addresses(host, type) do
    case adapter().lookup(host, type) do
      {:ok, addrs} when is_list(addrs) -> {:ok, addrs}
      addrs when is_list(addrs) -> {:ok, addrs}
      other -> other
    end
  end

  defp lookup_service_config(host) do
    name = "_grpc_config." <> host

    case adapter().lookup(name, :txt) do
      {:ok, txt_records} -> {:ok, txt_records}
      {:error, reason} -> {:error, reason}
      _ -> :no_config
    end
  end

  defp extract_service_config(txt_records) do
    Enum.find_value(txt_records, fn txt ->
      txt
      |> List.to_string()
      |> String.split("grpc_config=", parts: 2)
      |> case do
        [_, json] -> json
        _ -> nil
      end
    end)
  end

  @impl GRPC.Client.Resolver
  def init(target, opts) do
    if dns_target?(target) do
      connect_opts = Keyword.get(opts, :connect_opts, [])

      {:ok, pid} =
        GRPC.Client.DNSResolver.start_link(
          connection_pid: Keyword.fetch!(opts, :connection_pid),
          resolver: __MODULE__,
          target: target,
          resolve_interval: Keyword.get(connect_opts, :resolve_interval, 30_000),
          max_resolve_interval: Keyword.get(connect_opts, :max_resolve_interval, 300_000),
          min_resolve_interval: Keyword.get(connect_opts, :min_resolve_interval, 5_000)
        )

      {:ok, %{worker_pid: pid}}
    else
      {:ok, nil}
    end
  end

  defp dns_target?(target) do
    URI.parse(target).scheme == "dns"
  end

  @impl GRPC.Client.Resolver
  def update(%{worker_pid: pid}, :resolve_now) do
    send(pid, :resolve_now)
    {:ok, %{worker_pid: pid}}
  end

  @impl GRPC.Client.Resolver
  def update(state, _event), do: {:ok, state}

  @impl GRPC.Client.Resolver
  def shutdown(%{worker_pid: _pid}) do
    # Worker is linked to Connection, dies automatically
    :ok
  end

  def shutdown(nil), do: :ok

  defp adapter() do
    Application.get_env(:grpc_client, :dns_adapter, GRPC.Client.Resolver.DNS.Adapter)
  end
end
