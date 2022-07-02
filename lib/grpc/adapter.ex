defmodule GRPC.Client do
  @moduledoc """
  The behaviour which a client adapter should follow.

  A default implementation is provided in `GRPC.Adapter.Gun`.
  """

  alias GRPC.Channel
  alias GRPC.Client.Stream

  @typedoc "gRPC message content"
  @type message :: binary()

  @typedoc "Collection of HTTP headers"
  @type headers :: %{optional(String.t()) => any()}

  @typedoc "Represents whether the stream is ended (:fin) or not (:nofin)"
  @type fin :: :fin | :nofin

  @doc """
  """
  @callback connect(Channel.t(), opts :: keyword()) :: {:ok, Channel.t()} | {:error, any()}

  @doc """
  """
  @callback disconnect(Channel.t()) :: {:ok, Channel.t()} | {:error, any()}

  @doc """
  """
  @callback send_request(Stream.t(), message, opts :: keyword()) :: Stream.t()

  @doc """
  """
  @callback send_headers(Stream.t(), opts :: keyword()) :: Stream.t()

  @doc """
  """
  @callback send_data(Stream.t(), message, opts :: keyword()) :: Stream.t()

  @doc """
  """
  @callback end_stream(Stream.t()) :: Stream.t()

  @doc """
  """
  @callback cancel(Channel.adapter_payload(), Stream.payload()) :: :ok

  @doc """
  """
  @callback recv_headers(Channel.adapter_payload(), Stream.payload(), opts :: keyword()) ::
              {:ok, headers(), fin()} | {:error, GRPC.RPCError.exception() | any()}

  @doc """
  """
  @callback recv_data_or_trailers(Channel.adapter_payload(), Stream.payload(), opts :: keyword()) ::
              {:data, binary()}
              | {:trailers, binary()}
              | {:error, GRPC.RPCError.exception() | any()}
end
