defmodule Interop.Client do
  def connect(host, port) do
    {:ok, ch} = GRPC.Stub.connect(host, port, [])
    ch
  end

  def empty_unary!(ch) do
    IO.puts("Run empty_unary!")
    empty = Grpc.Testing.Empty.new()
    {:ok, ^empty} = Grpc.Testing.TestService.Stub.empty_call(ch, empty)
  end

  def cacheable_unary!(ch) do
    # TODO
  end

  def large_unary!(ch) do
    IO.puts("Run large_unary!")
    req = Grpc.Testing.SimpleRequest.new(response_size: 314159, payload: payload(271828))
    reply = Grpc.Testing.SimpleResponse.new(payload: payload(314159))
    {:ok, ^reply} = Grpc.Testing.TestService.Stub.unary_call(ch, req)
  end

  def client_compressed_unary!(ch) do
    # TODO
  end

  def server_compressed_unary!(ch) do
    # TODO
  end

  def client_streaming!(ch) do
    IO.puts("Run client_streaming!")
    stream = Grpc.Testing.TestService.Stub.streaming_input_call(ch)
    GRPC.Stub.stream_send(stream, Grpc.Testing.StreamingInputCallRequest.new(payload: payload(27182)))
    GRPC.Stub.stream_send(stream, Grpc.Testing.StreamingInputCallRequest.new(payload: payload(8)))
    GRPC.Stub.stream_send(stream, Grpc.Testing.StreamingInputCallRequest.new(payload: payload(1828)))
    GRPC.Stub.stream_send(stream, Grpc.Testing.StreamingInputCallRequest.new(payload: payload(45904)), end_stream: true)
    reply = Grpc.Testing.StreamingInputCallResponse.new(aggregated_payload_size: 74922)
    {:ok, ^reply} = GRPC.Stub.recv(stream)
  end

  def client_compressed_streaming!(ch) do
    # TODO
  end

  def server_streaming!(ch) do
    IO.puts("Run server_streaming!")
    params = Enum.map([31415, 9, 2653, 58979], &res_param(&1))
    req = Grpc.Testing.StreamingOutputCallRequest.new(response_parameters: params)
    stream = ch |> Grpc.Testing.TestService.Stub.streaming_output_call(req)
    result = Enum.map([31415, 9, 2653, 58979], &String.duplicate("0", &1))
    ^result = Enum.map(stream, fn res ->
      res.payload.body
    end)
  end

  def server_compressed_streaming!(ch) do
    # TODO
  end

  def ping_pong!(ch) do
    IO.puts("Run ping_pong!")
    stream = Grpc.Testing.TestService.Stub.full_duplex_call(ch)
    req = fn(size1, size2) ->
      Grpc.Testing.StreamingOutputCallRequest.new(response_parameters: [res_param(size1)], payload: payload(size2))
    end

    GRPC.Stub.stream_send(stream, req.(31415, 27182))
    res_enum = GRPC.Stub.recv(stream)
    reply = String.duplicate("0", 31415)
    %{payload: %{body: ^reply}} = Stream.take(res_enum, 1) |> Enum.to_list |> List.first

    GRPC.Stub.stream_send(stream, req.(9, 8))
    reply = String.duplicate("0", 9)
    %{payload: %{body: ^reply}} = Stream.take(res_enum, 1) |> Enum.to_list |> List.first

    GRPC.Stub.stream_send(stream, req.(2653, 1828))
    reply = String.duplicate("0", 2653)
    %{payload: %{body: ^reply}} = Stream.take(res_enum, 1) |> Enum.to_list |> List.first

    GRPC.Stub.stream_send(stream, req.(58979, 45904), end_stream: true)
    reply = String.duplicate("0", 58979)
    %{payload: %{body: ^reply}} = Stream.take(res_enum, 1) |> Enum.to_list |> List.first
  end

  def empty_stream!(ch) do
    IO.puts("Run empty_stream!")
    stream = Grpc.Testing.TestService.Stub.full_duplex_call(ch)
    :ok = GRPC.Stub.end_stream(stream)
    res_enum = GRPC.Stub.recv(stream)
    [] = Enum.to_list(res_enum)
  end

  defp res_param(size) do
    Grpc.Testing.ResponseParameters.new(size: size)
  end

  defp payload(n) do
    Grpc.Testing.Payload.new(body: String.duplicate("0", n))
  end
end
