defmodule GRPC.Handler do
  def init(req, {server, _opts} = state) do
    conn = %GRPC.Server.Conn{server: server}
    req = :cowboy_req.stream_reply(200, headers, req)
    conn = %{conn | state: req}
    case server.__call_rpc__(:cowboy_req.path(req), conn) do
      {:ok, %{state: req} = conn, response} ->
        GRPC.Server.stream_send(conn, response)
        :cowboy_req.stream_trailers(trailers, req)
        {:ok, req, state}
      {:ok, %{state: req}} ->
        :cowboy_req.stream_trailers(trailers, req)
        {:ok, req, state}
      {:error, %{state: req}, _reason} ->
        # TODO handle error branch
        {:ok, req, state}
    end
  end

  def headers do
    %{
      ":status" => 200
    }
  end

  def trailers do
    %{
      "grpc-status" => "0",
      "grpc-message" => ""
    }
  end
end
