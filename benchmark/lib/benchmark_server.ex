defmodule Grpc.Testing.BenchmarkService.Server do
  use GRPC.Server, service: Grpc.Testing.BenchmarkService.Service

  def unary_call(req, _) do
  end

  def streaming_call(req, stream) do
  end

  def streaming_from_client(req, stream) do
  end

  def streaming_from_server(req, stream) do
  end

  def streaming_both_ways(req, stream) do
  end
end
