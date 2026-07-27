defmodule GRPC.Client.LoadBalancing do
  @moduledoc "Load balancing behaviour for gRPC clients."

  @callback init(opts :: keyword()) :: {:ok, state :: any()} | {:error, reason :: any()}

  @callback pick(state :: any()) ::
              {:ok, struct(), new_state :: any()} | {:error, reason :: any()}

  @callback update(state :: any(), new_channels :: [struct()]) ::
              {:ok, new_state :: any()} | {:error, reason :: any()}
end
