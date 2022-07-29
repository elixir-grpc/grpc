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

  @callback recv_headers(stream :: map(), headers :: map(), opts :: keyword()) ::
              {:ok, %{String.t() => String.t()}, fin()} | {:error, GRPC.RPCError.t()}

  @callback recv_data_or_trailers(
              stream :: map(),
              trailers_or_metadata :: map(),
              opts :: keyword()
            ) ::
              {:data, binary()} | {:trailers, binary()} | {:error, GRPC.RPCError.t()}
end
