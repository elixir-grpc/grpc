defmodule GRPC.Client.Adapter do
  @moduledoc """
  HTTP client adapter for GRPC.
  """

  alias GRPC.Client.Stream
  alias GRPC.Channel

  @typedoc "Determines if the headers have finished being read."
  @type fin :: :fin | :nofin

  @callback connect(Channel.t(), map()) :: {:ok, Channel.t()} | {:error, any}

  @callback disconnect(Channel.t()) :: {:ok, Channel.t()} | {:error, any}

  @callback send_request(Stream.t(), binary(), map()) :: Stream.t()

  @callback recv_headers(map(), map(), map()) ::
              {:ok, %{String.t() => String.t()}, fin} | {:error, GRPC.RPCError.t()}

  @callback recv_data_or_trailers(map(), map(), map()) ::
              {:data, binary()} | {:trailers, binary()} | {:error, GRPC.RPCError.t()}
end
