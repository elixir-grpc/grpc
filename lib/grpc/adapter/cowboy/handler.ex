defmodule GRPC.Adapter.Cowboy.Handler do
  @moduledoc """
  A cowboy handler accepting all requests and calls corresponding functions
  defined by users.
  """

  alias GRPC.Transport.HTTP2

  @adapter GRPC.Adapter.Cowboy

  @spec init(any, {atom, keyword}) :: {:ok, any, {atom, keyword}}
  def init(req, {server, _opts} = state) do
    stream = %GRPC.Server.Stream{server: server, adapter: @adapter}
    req = :cowboy_req.stream_reply(200, HTTP2.server_headers, req)
    stream = %{stream | payload: req}
    trailers = HTTP2.server_trailers
    case server.__call_rpc__(:cowboy_req.path(req), stream) do
      {:ok, %{payload: req} = stream, response} ->
        GRPC.Server.stream_send(stream, response)
        :cowboy_req.stream_trailers(trailers, req)
        {:ok, req, state}
      {:ok, %{payload: req}} ->
        :cowboy_req.stream_trailers(trailers, req)
        {:ok, req, state}
      {:error, %{payload: req}, _reason} ->
        # TODO handle error branch
        {:ok, req, state}
    end
  end
end
