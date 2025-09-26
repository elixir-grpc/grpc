defmodule GRPC.Client.LoadBalancing.RoundRobin do
  @behaviour GRPC.Client.LoadBalancing

  @impl true
  def init(opts) do
    addresses = Keyword.get(opts, :addresses, [])

    if addresses == [] do
      {:error, :no_addresses}
    else
      {:ok, %{addresses: addresses, index: 0, n: length(addresses)}}
    end
  end

  @impl true
  def pick(%{addresses: addresses, index: idx} = state) do
    count = length(addresses)
    next_idx = rem(idx, count)
    %{address: host, port: port} = Enum.at(addresses, next_idx)

    new_state = %{state | index: idx + 1}
    {:ok, {host, port}, new_state}
  end
end
