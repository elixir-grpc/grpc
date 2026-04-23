defmodule GRPC.Client.LoadBalancing.PickFirst do
  @moduledoc """
  Pick-first load balancer: always returns the first channel in the list.

  Stateless and process-local — no ETS, no GenServer. `update/2` swaps the
  current pick when the channel list changes.
  """
  @behaviour GRPC.Client.LoadBalancing

  @impl true
  def init(opts) do
    case Keyword.get(opts, :channels, []) do
      [] -> {:error, :no_channels}
      [first | _] -> {:ok, %{current: first}}
    end
  end

  @impl true
  def pick(%{current: nil}), do: {:error, :no_channels}
  def pick(%{current: channel} = state), do: {:ok, channel, state}

  @impl true
  def update(_state, [first | _]), do: {:ok, %{current: first}}
  def update(_state, []), do: {:ok, %{current: nil}}

  @impl true
  def shutdown(_state), do: :ok
end
