defmodule GRPC.Client.LoadBalancing do
  @moduledoc """
  Load balancing behaviour for gRPC clients.

  A load-balancing strategy owns the per-request pick decision over a set of
  `GRPC.Channel` structs. It operates on already-connected channels handed to
  it by `GRPC.Client.Connection` — it is not responsible for establishing or
  tearing down transport.

  Required callbacks:

    * `init/1` — build initial state from `[channels: [Channel.t()]]`.
    * `pick/1` — choose a `Channel` for the next request.

  Optional callbacks (used by strategies that maintain live state, e.g. ETS):

    * `update/2` — reconcile the state with a new channel list in place,
      without tearing it down. Called by `Connection` on DNS re-resolution.
    * `shutdown/1` — release any resources held by the strategy. Called on
      disconnect.
  """
  alias GRPC.Channel

  @callback init(opts :: keyword()) :: {:ok, state :: any()} | {:error, reason :: any()}

  @callback pick(state :: any()) ::
              {:ok, Channel.t(), new_state :: any()} | {:error, reason :: any()}

  @callback update(state :: any(), new_channels :: [Channel.t()]) ::
              {:ok, new_state :: any()} | {:error, reason :: any()}

  @callback shutdown(state :: any()) :: :ok

  @optional_callbacks update: 2, shutdown: 1
end
