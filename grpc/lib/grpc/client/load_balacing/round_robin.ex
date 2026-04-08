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
  def pick(%{addresses: addresses, index: idx, n: n} = state) do
    %{address: host, port: port} = Enum.fetch!(addresses, idx)

    new_state = %{state | index: rem(idx + 1, n)}
    {:ok, {host, port}, new_state}
  end
end
