defmodule GRPC.Handler do
  def init(req, {server, _opts} = state) do
    {:ok, data, req} = :cowboy_req.read_body(req)
    message = GRPC.Message.from_data(data)
    # TODO handle error branch
    {:ok, response} = server.__call_rpc__(:cowboy_req.path(req), message)
    {:ok, data} = GRPC.Message.to_data(response)
    req = :cowboy_req.reply(200, headers, data, req)
    {:ok, req, state}
  end

  def headers do
    %{
      ":status" => 200
    }
  end

  # TODO
  # https://github.com/ninenines/cowboy/pull/1020
  def trailers do
    %{
      "grpc-status" => 0,
      "grpc-message" => ""
    }
  end
end
