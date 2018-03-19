defmodule GRPC.Adapter.Cowboy.Handler do
  @moduledoc false

  # A cowboy handler accepting all requests and calls corresponding functions
  # defined by users.

  alias GRPC.Transport.HTTP2
  alias GRPC.RPCError

  @adapter GRPC.Adapter.Cowboy
  @default_trailers HTTP2.server_trailers()

  @spec init(any, {GRPC.Server.servers_map(), keyword}) :: {:ok, any, {atom, keyword}}
  def init(req, {servers, _opts} = state) do
    path = :cowboy_req.path(req)
    server = Map.get(servers, GRPC.Server.service_name(path))
    stream = %GRPC.Server.Stream{server: server, adapter: @adapter, payload: req}
    stream = @adapter.set_headers(stream, HTTP2.server_headers())

    case server.__call_rpc__(path, stream) do
      {:ok, stream, response} ->
        stream = stream
        |> GRPC.Server.send_reply(response)
        |> GRPC.Server.send_trailers(@default_trailers)
        {:ok, stream.payload, state}

      {:ok, stream} ->
        stream = GRPC.Server.send_trailers(stream, @default_trailers)
        {:ok, stream.payload, state}

      {:error, stream, %RPCError{} = error} ->
        trailers = HTTP2.server_trailers(error.status, error.message)
        stream = GRPC.Server.send_trailers(stream, trailers)
        {:ok, stream.payload, state}
    end
  end
end
