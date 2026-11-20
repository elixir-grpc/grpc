defmodule GRPC.Client.LoadBalancing.RoundRobin do
  @moduledoc "Round-robin load balancer: cycles through the channels, one per pick."

  @behaviour GRPC.Client.LoadBalancing

  @channels_key :channels

  @impl true
  def init(opts) do
    case Keyword.get(opts, :channels, []) do
      [] ->
        {:error, :no_addresses}

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
        {:error, :no_addresses}
    end
  rescue
    ArgumentError -> {:error, :no_addresses}
  end

  # The cursor is deliberately NOT reset. `GRPC.Client.Connection` calls update/2 on
  # every DNS re-resolution, whether or not the address set changed. Resetting the
  # cursor there made the first pick after every resolve go to the first channel, so
  # a client making fewer than one call per channel per resolve interval sent nearly
  # all of its traffic to one backend. pick/1 takes the index modulo the current
  # size, so a cursor carried over a grown or shrunk list stays valid.
  @impl true
  def update(%{tid: tid} = state, new_channels) do
    :ets.insert(tid, {@channels_key, List.to_tuple(new_channels)})
    {:ok, state}
  end

  @impl true
  def terminate(%{tid: tid}) do
    :ets.delete(tid)
    :ok
  rescue
    ArgumentError -> :ok
  end
end
