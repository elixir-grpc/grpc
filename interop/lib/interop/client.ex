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
    stream = ch
      |> Grpc.Testing.TestService.Stub.streaming_input_call()
      |> GRPC.Stub.stream_send(Grpc.Testing.StreamingInputCallRequest.new(payload: payload(27182)))
      |> GRPC.Stub.stream_send(Grpc.Testing.StreamingInputCallRequest.new(payload: payload(8)))
      |> GRPC.Stub.stream_send(Grpc.Testing.StreamingInputCallRequest.new(payload: payload(1828)))
      |> GRPC.Stub.stream_send(Grpc.Testing.StreamingInputCallRequest.new(payload: payload(45904)), end_stream: true)
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
    {:ok, res_enum} = ch |> Grpc.Testing.TestService.Stub.streaming_output_call(req)
    result = Enum.map([31415, 9, 2653, 58979], &String.duplicate("0", &1))
    ^result = Enum.map(res_enum, fn {:ok, res} ->
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
    {:ok, res_enum} = GRPC.Stub.recv(stream)
    reply = String.duplicate("0", 31415)
    {:ok, %{payload: %{body: ^reply}}} = Stream.take(res_enum, 1) |> Enum.to_list |> List.first

    Enum.each([{9, 8}, {2653, 1828}, {58979, 45904}], fn ({res, payload}) ->
      GRPC.Stub.stream_send(stream, req.(res, payload))
      reply = String.duplicate("0", res)
      {:ok, %{payload: %{body: ^reply}}} = Stream.take(res_enum, 1) |> Enum.to_list |> List.first
    end)
    GRPC.Stub.end_stream(stream)
  end

  def empty_stream!(ch) do
    IO.puts("Run empty_stream!")
    {:ok, res_enum} =
      ch
      |> Grpc.Testing.TestService.Stub.full_duplex_call()
      |> GRPC.Stub.end_stream()
      |> GRPC.Stub.recv()
    [] = Enum.to_list(res_enum)
  end

  def custom_metadata!(ch) do
    IO.puts("Run custom_metadata!")
    # UnaryCall
    req = Grpc.Testing.SimpleRequest.new(response_size: 314159, payload: payload(271828))
    reply = Grpc.Testing.SimpleResponse.new(payload: payload(314159))
    headers = %{"x-grpc-test-echo-initial" => "test_initial_metadata_value"}
    trailers = %{"x-grpc-test-echo-trailing-bin" => 0xababab} # 11250603
    metadata = Map.merge(headers, trailers)
    {:ok, ^reply, %{headers: new_headers, trailers: new_trailers}} = Grpc.Testing.TestService.Stub.unary_call(ch, req, metadata: metadata, return_headers: true)
    validate_headers!(new_headers, new_trailers)

    # FullDuplexCall
    req = Grpc.Testing.StreamingOutputCallRequest.new(response_parameters: [res_param(314159)], payload: payload(271828))
    {:ok, res_enum, %{headers: new_headers}} =
      ch
      |> Grpc.Testing.TestService.Stub.full_duplex_call(metadata: metadata)
      |> GRPC.Stub.stream_send(req, end_stream: true)
      |> GRPC.Stub.recv(return_headers: true)

    reply = String.duplicate("0", 314159)
    {:ok, %{payload: %{body: ^reply}}} = Stream.take(res_enum, 1) |> Enum.to_list |> List.first
    {:trailers, new_trailers} = Stream.take(res_enum, 1) |> Enum.to_list |> List.first
    validate_headers!(new_headers, new_trailers)
  end

  def status_code_and_message!(ch) do
    IO.puts("Run status_code_and_message!")

    code = 2
    msg = "test status message"
    status = Grpc.Testing.EchoStatus.new(code: code, message: msg)
    error = GRPC.RPCError.exception(code, msg)

    # UnaryCall
    req = Grpc.Testing.SimpleRequest.new(response_status: status)
    {:error, ^error} = Grpc.Testing.TestService.Stub.unary_call(ch, req)

    # FullDuplexCall
    req = Grpc.Testing.StreamingOutputCallRequest.new(response_status: status)
    {:ok, res_enum} =
      ch
      |> Grpc.Testing.TestService.Stub.full_duplex_call()
      |> GRPC.Stub.stream_send(req, end_stream: true)
      |> GRPC.Stub.recv()

    {:error, ^error} = Stream.take(res_enum, 1) |> Enum.to_list |> List.first
  end

  defp validate_headers!(headers, trailers) do
    %{"x-grpc-test-echo-initial" => "test_initial_metadata_value"} = headers
    %{"x-grpc-test-echo-trailing-bin" => "11250603"} = trailers
  end

  defp res_param(size) do
    Grpc.Testing.ResponseParameters.new(size: size)
  end

  defp payload(n) do
    Grpc.Testing.Payload.new(body: String.duplicate("0", n))
  end
end
