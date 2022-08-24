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

  @callback receive_data(stream :: Stream.t(), opts :: keyword()) ::
              {:ok, struct()}
              | {:ok, struct(), map()}
              | {:ok, Enumerable.t()}
              | {:ok, Enumerable.t(), map()}
              | {:error, any()}
end
