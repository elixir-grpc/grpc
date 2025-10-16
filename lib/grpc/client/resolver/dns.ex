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
    case adapter().lookup(host, :a) do
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

  defp adapter() do
    Application.get_env(:grpc, :dns_adapter, GRPC.Client.Resolver.DNS.Adapter)
  end
end
