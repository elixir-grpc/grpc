defmodule GRPC.Client.LoadBalancing.RoundRobin do
  @moduledoc """
  Round-robin load balancer.

  The pick path is fully lock-free:

    * `:atomics.add_get/3` advances the cursor with a hardware CAS — no
      table-level or key-level lock, unlike `:ets.update_counter/3` which
      serialises writers on a single key's hash bucket.
    * The channel tuple lives in a `read_concurrency: true` ETS table and
      is replaced wholesale on reconcile, so concurrent picks never see a
      torn list.

  The ETS table is owned by whichever process calls `init/1` (normally the
  `GRPC.Client.Connection` GenServer), so when that process dies the table
  is reclaimed automatically. The atomics ref has no owner — it is reclaimed
  by the GC when the last reference is dropped.
  """
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
  # `disconnect/1` may delete the table between a caller's persistent_term
  # lookup and pick — the rescue turns the BIF crash into a tagged error.
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
