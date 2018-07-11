defmodule Benchmark.ClientWorker do
  require Logger
  use GenServer

  def init(args) do
    send(self(), :start)
    {:ok, args}
  end

  def handle_info(:start, {ch, %{rpc_type: rpc_type} = payload, manager}) do
    case rpc_type do
      :UNARY ->
        unary_loop(ch, payload, manager)
    end
  end

  def unary_loop(ch, %{req_size: req_size, resp_size: resp_size} = payload, manager) do
    start = Time.utc_now()
    unary_call(ch, req_size, resp_size)
    dur = Time.diff(Time.utc_now(), start, :microsecond)
    GenServer.call(manager, {:track_rpc, dur})
    unary_loop(ch, payload, manager)
  end

  def unary_call(ch, req_size, resp_size) do
    payload =
      Grpc.Testing.Payload.new(
        type: Grpc.Testing.PayloadType.value(:COMPRESSABLE),
        body: String.duplicate(<<0>>, req_size)
      )

    req =
      Grpc.Testing.SimpleRequest.new(
        response_type: payload.type,
        response_size: resp_size,
        payload: payload
      )

    Logger.debug("Sending rpc #{req}")
    Grpc.Testing.BenchmarkService.Stub.unary_call(ch, req)
  end
end
