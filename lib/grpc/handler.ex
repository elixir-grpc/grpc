defmodule GRPC.Handler do
  @moduledoc """
    GRPC.Handler 
  """
  def init(req, {server, _opts} = state) do
    {:ok, data, req} = :cowboy_req.read_body(req)
    message = GRPC.Message.from_data(data)
    # TODO handle error branch
    {:ok, response} = server.__call_rpc__(:cowboy_req.path(req), message)
    {:ok, data} = GRPC.Message.to_data(response, iolist: true)
    req = :cowboy_req.stream_reply(200, headers, req)
    :cowboy_req.stream_body(data, :nofin, req)
    :cowboy_req.stream_trailers(trailers, req)
    {:ok, req, state}
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
