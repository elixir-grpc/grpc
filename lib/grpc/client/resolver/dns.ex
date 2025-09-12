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
    host_charlist = String.to_charlist(host)

    case adapter().lookup(host_charlist, :a) do
      {:ok, addresses} ->
        addrs =
          Enum.map(addresses, fn ip ->
            %{address: ip |> :inet.ntoa() |> to_string(), port: port}
          end)

        case adapter().lookup(~c"_grpc_config." ++ host_charlist, :txt) do
          {:ok, txt_records} ->
            service_config_json =
              txt_records
              |> Enum.map(&List.to_string/1)
              |> Enum.find_value(fn str ->
                case String.split(str, "grpc_config=") do
                  [_, json] -> json
                  _ -> nil
                end
              end)

            {:ok, %{addresses: addrs, service_config: ServiceConfig.parse(service_config_json)}}

          _ ->
            {:ok, %{addresses: addrs, service_config: nil}}
        end

      {:error, reason} ->
        {:error, {:dns_error, reason}}
    end
  end

  defp adapter() do
    Application.get_env(:grpc, :dns_adapter, GRPC.Client.Resolver.DNS.Adapter)
  end
end
