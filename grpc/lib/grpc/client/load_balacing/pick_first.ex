defmodule GRPC.Client.LoadBalancing.PickFirst do
  @moduledoc """
  Pick-first load balancer: always returns the first channel in the list.

  Stores the current pick in an ETS table so `update/2` can swap it without
  returning a new state — callers that captured `lb_state` (the Connection
  GenServer, the LB registry) keep seeing the right channel.
  """
  @behaviour GRPC.Client.LoadBalancing

  @current_key :current

  @impl true
  def init(opts) do
    case Keyword.get(opts, :channels, []) do
      [] ->
        {:error, :no_channels}

      [first | _] ->
        tid = :ets.new(:grpc_lb_pick_first, [:set, :public, read_concurrency: true])
        :ets.insert(tid, {@current_key, first})
        {:ok, %{tid: tid}}
    end
  end

  @impl true
  def pick(%{tid: tid} = state) do
    case :ets.lookup(tid, @current_key) do
      [{@current_key, nil}] -> {:error, :no_channels}
      [{@current_key, channel}] -> {:ok, channel, state}
      [] -> {:error, :no_channels}
    end
  rescue
    ArgumentError -> {:error, :no_channels}
  end

  @impl true
  def update(%{tid: tid} = state, [first | _]) do
    :ets.insert(tid, {@current_key, first})
    {:ok, state}
  end

  def update(%{tid: tid} = state, []) do
    :ets.insert(tid, {@current_key, nil})
    {:ok, state}
  end

  @impl true
  def shutdown(%{tid: tid}) do
    :ets.delete(tid)
    :ok
  rescue
    ArgumentError -> :ok
  end
end
