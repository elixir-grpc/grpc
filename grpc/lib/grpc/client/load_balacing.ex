defmodule GRPC.Client.LoadBalancing do
  @moduledoc "Load balancing behaviour for gRPC clients."

  @callback init(opts :: keyword()) :: {:ok, state :: any()} | {:error, reason :: any()}

  @callback pick(state :: any()) ::
              {:ok, struct(), new_state :: any()} | {:error, reason :: any()}

  @callback update(state :: any(), new_channels :: [struct()]) ::
              {:ok, new_state :: any()} | {:error, reason :: any()}

  @doc """
  Releases any resources held by the balancer state (e.g. ETS tables).

  Called when a connection replaces its balancer with a different policy.
  """
  @callback terminate(state :: any()) :: :ok

  @optional_callbacks terminate: 1
end
