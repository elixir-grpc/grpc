defmodule Grpc.Testing.BenchmarkService.Server do
  use GRPC.Server, service: Grpc.Testing.BenchmarkService.Service
  require Logger
  alias Grpc.Testing.{SimpleResponse}

  def unary_call(req, _) do
    Logger.debug("got unary_call #{inspect(req)}")
    %SimpleResponse{payload: new_payload(req.response_type, req.response_size)}
  end

  def streaming_call(req_enum, stream) do
    Enum.each(req_enum, fn req ->
      Logger.debug("got streaming_call #{inspect(req)}")
      out = %SimpleResponse{payload: new_payload(req.response_type, req.response_size)}
      GRPC.Server.send_reply(stream, out)
    end)
  end

  def streaming_from_client(_req, _stream) do
    Logger.debug("got streaming_from_client")
  end

  def streaming_from_server(_req, _stream) do
    Logger.debug("got streaming_from_server")
  end

  def streaming_both_ways(_req, _stream) do
    Logger.debug("got streaming_both_ways")
  end

  def new_payload(type, 0) do
    %Grpc.Testing.Payload{type: type}
  end

  def new_payload(type, size) do
    %Grpc.Testing.Payload{
      type: type,
      body: String.duplicate(<<0>>, size)
    }
  end
end
