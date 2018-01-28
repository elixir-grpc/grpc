defmodule GRPC.Adapter.Cowboy.Handler do
  @moduledoc """
  A cowboy handler accepting all requests and calls corresponding functions
  defined by users.
  """

  alias GRPC.Transport.HTTP2
  alias GRPC.RPCError

  @adapter GRPC.Adapter.Cowboy

  @spec init(any, {GRPC.Server.servers_map(), keyword}) :: {:ok, any, {atom, keyword}}
  def init(req, {servers, _opts} = state) do
    req = :cowboy_req.stream_reply(200, HTTP2.server_headers(), req)
    path = :cowboy_req.path(req)
    server = Map.get(servers, GRPC.Server.service_name(path))
    stream = %GRPC.Server.Stream{server: server, adapter: @adapter}
    stream = %{stream | payload: req}
    trailers = HTTP2.server_trailers()

    case server.__call_rpc__(path, stream) do
      {:ok, %{payload: req} = stream, response} ->
        GRPC.Server.stream_send(stream, response)
        :cowboy_req.stream_trailers(trailers, req)
        {:ok, req, state}

      {:ok, %{payload: req}} ->
        :cowboy_req.stream_trailers(trailers, req)
        {:ok, req, state}

      {:error, %{payload: req}, %RPCError{} = error} ->
        trailers = HTTP2.server_trailers(error.status, error.message)
        :cowboy_req.stream_trailers(trailers, req)
        {:ok, req, state}

      {:error, %{payload: req}, _reason} ->
        # TODO handle error branch
        {:ok, req, state}
    end
  end
end
