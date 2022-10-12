defmodule GRPC.Client.Adapter do
  @moduledoc """
  HTTP client adapter for GRPC.
  """

  alias GRPC.Client.Stream
  alias GRPC.Channel

  @typedoc "Determines if the headers have finished being read."
  @type fin :: :fin | :nofin

  @callback connect(channel :: Channel.t(), opts :: keyword()) ::
              {:ok, Channel.t()} | {:error, any()}

  @callback disconnect(channel :: Channel.t()) :: {:ok, Channel.t()} | {:error, any()}

  @callback send_request(stream :: Stream.t(), contents :: binary(), opts :: keyword()) ::
              Stream.t()

  @doc """
  Check `GRPC.Stub.recv/2` for more context about the return types
  """
  @callback receive_data(stream :: Stream.t(), opts :: keyword()) ::
              GRPC.Stub.receive_data_return() | {:error, any()}
end
