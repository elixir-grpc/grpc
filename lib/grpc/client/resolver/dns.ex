defmodule GRPC.Client.Resolver.DNS do
  @moduledoc """
  DNS Resolver for gRPC targets, supporting dynamic updates via a GenServer.

  Resolves `dns://host[:port]` targets. Fetches A/AAAA records and optional
  `_grpc_config.<host>` TXT records for ServiceConfig.

  This implementation maintains an internal cache of addresses and service config,
  and refreshes them periodically.
  """
  use GenServer
  @behaviour GRPC.Client.Resolver

  @refresh_interval 5_000

  alias GRPC.Client.ServiceConfig

  @impl GRPC.Client.Resolver
  def start(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl GRPC.Client.Resolver
  def resolve(target) do
    case GenServer.call(__MODULE__, {:resolve, target}) do
      {:ok, result} -> result
      {:error, reason} -> {:error, reason}
    end
  end

  def start_link(_) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @impl GenServer
  def init(state) do
    {:ok, state}
  end

  @impl GenServer
  def handle_call({:resolve, target}, _from, state) do
    key = target

    case Map.get(state, key) do
      nil ->
        value = fetch(target)
        Process.send_after(self(), {:refresh, target}, @refresh_interval)
        {:reply, {:ok, value}, Map.put(state, key, value)}

      cached ->
        {:reply, {:ok, cached}, state}
    end
  end

  @impl GenServer
  def handle_info({:refresh, target}, state) do
    value = fetch(target)
    Process.send_after(self(), {:refresh, target}, @refresh_interval)
    {:noreply, Map.put(state, target, value)}
  end

  def fetch(target) do
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
