defmodule GRPC.Client.LoadBalancing do
  @moduledoc "Load balancing behaviour for gRPC clients."

  alias GRPC.Channel

  @callback init(opts :: keyword()) :: {:ok, state :: any()} | {:error, reason :: any()}

  @callback pick(state :: any()) ::
              {:ok, Channel.t(), new_state :: any()} | {:error, reason :: any()}

  @callback update(state :: any(), new_channels :: [Channel.t()]) ::
              {:ok, new_state :: any()} | {:error, reason :: any()}
end
