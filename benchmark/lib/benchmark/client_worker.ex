defmodule Benchmark.ClientWorker do
  require Logger

  def unary_loop(channel, input) do
    unary_call(channel, input)
  end

  def unary_call(channel, input) do
    payload =
      Grpc.Testing.Payload.new(
        type: Grpc.Testing.PayloadType.value(:COMPRESSABLE),
        body: String.duplicate(<<0>>, input.req_size)
      )

    req =
      Grpc.Testing.SimpleRequest.new(
        response_type: payload.type,
        response_size: input.resp_size,
        payload: payload
      )

    Logger.debug("Sending rpc #{req}")
    Grpc.Testing.BenchmarkService.Stub.unary_call(channel, req)
  end
end
