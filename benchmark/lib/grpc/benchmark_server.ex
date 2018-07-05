defmodule Grpc.Testing.BenchmarkService.Server do
  use GRPC.Server, service: Grpc.Testing.BenchmarkService.Service
  require Logger

  def unary_call(req, _) do
    Logger.debug("got unary_call #{inspect(req)}")
  end

  def streaming_call(req, stream) do
    Logger.debug("got streaming_call #{inspect(req)}")
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
end
