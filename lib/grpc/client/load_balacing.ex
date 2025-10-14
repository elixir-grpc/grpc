defmodule GRPC.Client.LoadBalancing do
  @moduledoc """
  Load balancing behaviour for gRPC clients.

  This module defines the behaviour that load balancing strategies must implement.
  """
  @callback init(opts :: keyword()) :: {:ok, state :: any()} | {:error, reason :: any()}

  @callback pick(state :: any()) ::
              {:ok, {host :: String.t(), port :: non_neg_integer()}, new_state :: any()}
              | {:error, reason :: any()}
end
