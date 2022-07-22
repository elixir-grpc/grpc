defmodule GRPC.Telemetry.Client do
  @moduledoc """
  Telemetry interceptor for GRPC client.

  ## Events

  TBD.
  """

  @behaviour GRPC.ClientInterceptor

  @prefix [:grpc, :client, :request]
  @default_metadata %{}

  @impl true
  def init(opts) do
    # TODO: clarify opts for this interceptor.
    opts
  end

  @impl true
  def call(%{grpc_type: :unary} = stream, req, next, _opts) do
    # TODO: attach stream information to metadata.
    metadata = Map.merge(@default_metadata, %{stream: stream})

    :telemetry.span(@prefix, metadata, fn ->
      reply = next.(stream, req)
      {reply, %{reply: reply}}
    end)
  end

  @impl true
  def call(stream, req, next, _opts) do
    next.(stream, req)
  end
end
