defmodule Grpc.Testing.BenchmarkService.Server do
  use GRPC.Server, service: Grpc.Testing.BenchmarkService.Service
  require Logger
  alias Grpc.Testing.{SimpleRequest, SimpleResponse}

  def unary_call(req, _) do
    Logger.debug("got unary_call #{inspect(req)}")
    SimpleResponse.new(payload: new_payload(req.response_type, req.response_size))
  end

  def streaming_call(req_enum, stream) do
    Enum.each(req_enum, fn req ->
      Logger.debug("got streaming_call #{inspect(req)}")
      out = SimpleResponse.new(payload: new_payload(req.response_type, req.response_size))
      GRPC.Server.send_reply(stream, out)
    end)
  end

  def streaming_from_client(req, stream) do
    Logger.debug("got streaming_from_client #{inspect(req)}")
  end

  def streaming_from_server(req, stream) do
    Logger.debug("got streaming_from_server #{inspect(req)}")
  end

  def streaming_both_ways(req, stream) do
    Logger.debug("got streaming_both_ways #{inspect(req)}")
  end

  def new_payload(type, size) do
    Grpc.Testing.Payload.new(
      type: type,
      body: String.duplicate(<<0>>, size)
    )
  end
end
