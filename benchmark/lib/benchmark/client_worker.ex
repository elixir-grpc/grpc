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

      :STREAMING ->
        before_streaming_loop(ch, payload, manager)
    end
  end

  def unary_loop(ch, %{req_size: req_size, resp_size: resp_size} = payload, {pid, no} = manager) do
    start = System.monotonic_time(:nanosecond)
    unary_call(ch, req_size, resp_size)
    dur = System.monotonic_time(:nanosecond) - start
    GenServer.cast(pid, {:track_rpc, no, dur})
    unary_loop(ch, payload, manager)
  end

  def unary_call(ch, req_size, resp_size) do
    payload_type = Grpc.Testing.PayloadType.value(:COMPRESSABLE)

    payload =
      Grpc.Testing.Payload.new(
        type: payload_type,
        body: List.duplicate(<<0>>, req_size)
      )

    req =
      Grpc.Testing.SimpleRequest.new(
        response_type: payload_type,
        response_size: resp_size,
        payload: payload
      )

    Grpc.Testing.BenchmarkService.Stub.unary_call(ch, req)
  end

  def before_streaming_loop(ch, %{req_size: req_size, resp_size: resp_size}, manager) do
    stream = Grpc.Testing.BenchmarkService.Stub.streaming_call(ch, timeout: :infinity)

    payload_type = Grpc.Testing.PayloadType.value(:COMPRESSABLE)

    payload =
      Grpc.Testing.Payload.new(
        type: payload_type,
        body: List.duplicate(<<0>>, req_size)
      )

    req =
      Grpc.Testing.SimpleRequest.new(
        response_type: payload_type,
        response_size: resp_size,
        payload: payload
      )

    streaming_loop(stream, req, manager, nil)
  end

  def streaming_loop(stream, req, {pid, no} = manager, res_enum) do
    start = System.monotonic_time(:nanosecond)
    GRPC.Stub.send_request(stream, req)

    res_enum =
      if res_enum do
        res_enum
      else
        {:ok, enum} = GRPC.Stub.recv(stream)
        enum
      end

    {:ok, _} = Stream.take(res_enum, 1) |> Enum.to_list() |> List.first()

    dur = System.monotonic_time(:nanosecond) - start
    GenServer.cast(pid, {:track_rpc, no, dur})
    streaming_loop(stream, req, manager, res_enum)
  end
end
