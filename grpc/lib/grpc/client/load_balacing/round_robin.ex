defmodule GRPC.Client.LoadBalancing.RoundRobin do
  @moduledoc "Round-robin load balancer (ETS for the channel tuple, `:atomics` for the cursor)."

  @behaviour GRPC.Client.LoadBalancing

  @channels_key :channels

  @impl true
  def init(opts) do
    case Keyword.get(opts, :channels, []) do
      [] ->
        {:error, :no_channels}

      channels ->
        tid = :ets.new(:grpc_lb_round_robin, [:set, :public, read_concurrency: true])
        aref = :atomics.new(1, signed: false)

        :ets.insert(tid, {@channels_key, List.to_tuple(channels)})

        {:ok, %{tid: tid, atomics: aref}}
    end
  end

  @impl true
  def pick(%{tid: tid, atomics: aref} = state) do
    case :ets.lookup(tid, @channels_key) do
      [{@channels_key, channels}] when tuple_size(channels) > 0 ->
        idx = :atomics.add_get(aref, 1, 1)
        channel = elem(channels, rem(idx - 1, tuple_size(channels)))
        {:ok, channel, state}

      _ ->
        {:error, :no_channels}
    end
  rescue
    ArgumentError -> {:error, :no_channels}
  end

  @impl true
  def update(%{tid: tid, atomics: aref} = state, new_channels) do
    :ets.insert(tid, {@channels_key, List.to_tuple(new_channels)})
    :atomics.put(aref, 1, 0)
    {:ok, state}
  end
end
