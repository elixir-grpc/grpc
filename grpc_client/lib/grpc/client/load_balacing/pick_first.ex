defmodule GRPC.Client.LoadBalancing.PickFirst do
  @behaviour GRPC.Client.LoadBalancing

  @impl true
  def init(opts) do
    case Keyword.get(opts, :addresses, []) do
      [] -> {:error, :no_addresses}
      addresses -> {:ok, %{addresses: addresses, current: hd(addresses)}}
    end
  end

  @impl true
  def pick(%{current: %{address: host, port: port}} = state) do
    {:ok, {host, port}, state}
  end
end
