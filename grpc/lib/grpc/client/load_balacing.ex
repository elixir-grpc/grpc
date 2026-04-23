defmodule GRPC.Client.LoadBalancing do
  @moduledoc """
  Load balancing behaviour for gRPC clients.

  A load-balancing strategy owns the per-request pick decision over a set of
  `GRPC.Channel` structs. It operates on already-connected channels handed to
  it by `GRPC.Client.Connection` — it is not responsible for establishing or
  tearing down transport.

  Callbacks:

    * `init/1` — build initial state from `[channels: [Channel.t()]]`.
    * `pick/1` — choose a `Channel` for the next request.
    * `update/2` — swap in a new channel list when DNS re-resolution
      discovers new or removed backends.

  `init/1` runs inside the `GRPC.Client.Connection` GenServer. Any ETS
  tables or other process-owned resources the strategy creates are owned
  by that GenServer and are released automatically when it exits.
  """
  alias GRPC.Channel

  @callback init(opts :: keyword()) :: {:ok, state :: any()} | {:error, reason :: any()}

  @callback pick(state :: any()) ::
              {:ok, Channel.t(), new_state :: any()} | {:error, reason :: any()}

  @callback update(state :: any(), new_channels :: [Channel.t()]) ::
              {:ok, new_state :: any()} | {:error, reason :: any()}
end
