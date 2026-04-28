defmodule GRPC.Client.LoadBalancing.RoundRobin do
  @moduledoc """
  Round-robin load balancer backed by an ETS table.

  The pick path is lock-free: `:ets.update_counter/3` atomically advances the
  cursor and `elem/2` indexes the channel tuple in constant time. No GenServer
  sits in front of the pick, so RPC-time picks are a single atomic increment
  plus a lookup.

  The ETS table is owned by whichever process calls `init/1` (normally the
  `GRPC.Client.Connection` GenServer), so when that process dies the table
  is reclaimed automatically.
  """
  @behaviour GRPC.Client.LoadBalancing

  @channels_key :channels
  @index_key :index

  @impl true
  def init(opts) do
    case Keyword.get(opts, :channels, []) do
      [] ->
        {:error, :no_channels}

      channels ->
        tid = :ets.new(:grpc_lb_round_robin, [:set, :public, read_concurrency: true])

        :ets.insert(tid, {@channels_key, List.to_tuple(channels)})
        :ets.insert(tid, {@index_key, -1})

        {:ok, %{tid: tid}}
    end
  end

  @impl true
  # `disconnect/1` may delete the table between a caller's registry lookup and pick.
  def pick(%{tid: tid} = state) do
    case :ets.lookup(tid, @channels_key) do
      [{@channels_key, channels}] when tuple_size(channels) > 0 ->
        idx = :ets.update_counter(tid, @index_key, {2, 1})
        channel = elem(channels, rem(idx, tuple_size(channels)))
        {:ok, channel, state}

      _ ->
        {:error, :no_channels}
    end
  rescue
    ArgumentError -> {:error, :no_channels}
  end

  @impl true
  def update(%{tid: tid} = state, new_channels) do
    :ets.insert(tid, {@channels_key, List.to_tuple(new_channels)})
    :ets.insert(tid, {@index_key, -1})
    {:ok, state}
  end
end
