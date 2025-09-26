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

    with {:ok, addresses} <- lookup_addresses(host),
         {:ok, txt_records} <- lookup_service_config(host) do
      addrs = Enum.map(addresses, &%{address: :inet.ntoa(&1) |> to_string(), port: port})
      service_config_json = extract_service_config(txt_records)

      {:ok, %{addresses: addrs, service_config: ServiceConfig.parse(service_config_json)}}
    else
      {:error, reason} ->
        {:error, {:dns_error, reason}}

      :no_config ->
        {:ok,
         %{
           addresses: Enum.map(addresses, &%{address: :inet.ntoa(&1) |> to_string(), port: port}),
           service_config: nil
         }}
    end
  end

  defp lookup_addresses(host) do
    adapter().lookup(host, :a)
  end

  defp lookup_service_config(host) do
    case adapter().lookup(~c"_grpc_config." ++ host, :txt) do
      {:ok, txt_records} -> {:ok, txt_records}
      _ -> :no_config
    end
  end

  defp extract_service_config(txt_records) do
    Enum.find_value(txt_records, fn txt ->
      str = List.to_string(txt)

      case String.split(str, "grpc_config=") do
        [_, json] -> json
        _ -> nil
      end
    end)
  end

  defp adapter() do
    Application.get_env(:grpc, :dns_adapter, GRPC.Client.Resolver.DNS.Adapter)
  end
end
