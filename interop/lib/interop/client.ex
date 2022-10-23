defmodule Interop.Client do
  import ExUnit.Assertions, only: [refute: 1]

  require Logger

  def connect(host, port, opts \\ []) do
    {:ok, ch} = GRPC.Stub.connect(host, port, opts)
    ch
  end

  def empty_unary!(ch) do
    Logger.info("Run empty_unary!")
    empty = Grpc.Testing.Empty.new()
    {:ok, ^empty} = Grpc.Testing.TestService.Stub.empty_call(ch, empty)
  end

  def cacheable_unary!(_ch) do
    # TODO
  end

  def large_unary!(ch) do
    Logger.info("Run large_unary!")
    req = Grpc.Testing.SimpleRequest.new(response_size: 314_159, payload: payload(271_828))
    reply = Grpc.Testing.SimpleResponse.new(payload: payload(314_159))
    {:ok, ^reply} = Grpc.Testing.TestService.Stub.unary_call(ch, req)
  end

  def large_unary2!(ch) do
    Logger.info("Run large_unary2!")
    req = Grpc.Testing.SimpleRequest.new(response_size: 1024*1024*8, payload: payload(1024*1024*8))
    reply = Grpc.Testing.SimpleResponse.new(payload: payload(1024*1024*8))
    {:ok, ^reply} = Grpc.Testing.TestService.Stub.unary_call(ch, req)
  end

  def client_compressed_unary!(ch) do
    Logger.info("Run client_compressed_unary!")
    # "Client calls UnaryCall with the feature probe, an uncompressed message" is not supported

    req = Grpc.Testing.SimpleRequest.new(expect_compressed: %{value: true}, response_size: 314_159, payload: payload(271_828))
    reply = Grpc.Testing.SimpleResponse.new(payload: payload(314_159))
    {:ok, ^reply} = Grpc.Testing.TestService.Stub.unary_call(ch, req, compressor: GRPC.Compressor.Gzip)

    req = Grpc.Testing.SimpleRequest.new(expect_compressed: %{value: false}, response_size: 314_159, payload: payload(271_828))
    reply = Grpc.Testing.SimpleResponse.new(payload: payload(314_159))
    {:ok, ^reply} = Grpc.Testing.TestService.Stub.unary_call(ch, req)
  end

  def server_compressed_unary!(ch) do
    Logger.info("Run server_compressed_unary!")

    req = Grpc.Testing.SimpleRequest.new(response_compressed: %{value: true}, response_size: 314_159, payload: payload(271_828))
    reply = Grpc.Testing.SimpleResponse.new(payload: payload(314_159))
    {:ok, ^reply, %{headers: %{"grpc-encoding" => "gzip"}}} = Grpc.Testing.TestService.Stub.unary_call(ch, req, compressor: GRPC.Compressor.Gzip, return_headers: true)

    req = Grpc.Testing.SimpleRequest.new(response_compressed: %{value: false}, response_size: 314_159, payload: payload(271_828))
    reply = Grpc.Testing.SimpleResponse.new(payload: payload(314_159))
    {:ok, ^reply, headers} = Grpc.Testing.TestService.Stub.unary_call(ch, req, return_headers: true)
    refute headers[:headers]["grpc-encoding"]
  end

  def client_streaming!(ch) do
    Logger.info("Run client_streaming!")

    stream =
      ch
      |> Grpc.Testing.TestService.Stub.streaming_input_call()
      |> GRPC.Stub.send_request(
        Grpc.Testing.StreamingInputCallRequest.new(payload: payload(27182))
      )
      |> GRPC.Stub.send_request(Grpc.Testing.StreamingInputCallRequest.new(payload: payload(8)))
      |> GRPC.Stub.send_request(
        Grpc.Testing.StreamingInputCallRequest.new(payload: payload(1828))
      )
      |> GRPC.Stub.send_request(
        Grpc.Testing.StreamingInputCallRequest.new(payload: payload(45904)),
        end_stream: true
      )

    reply = Grpc.Testing.StreamingInputCallResponse.new(aggregated_payload_size: 74922)
    {:ok, ^reply} = GRPC.Stub.recv(stream)
  end

  def client_compressed_streaming!(ch) do
    Logger.info("Run client_compressed_streaming!")

    # INVALID_ARGUMENT testing is not supported

    stream =
      ch
      |> Grpc.Testing.TestService.Stub.streaming_input_call(compressor: GRPC.Compressor.Gzip)
      |> GRPC.Stub.send_request(Grpc.Testing.StreamingInputCallRequest.new(payload: payload(27182), expect_compressed: %{value: true}))
      |> GRPC.Stub.send_request(
        Grpc.Testing.StreamingInputCallRequest.new(payload: payload(45904), expect_compressed: %{value: false}),
        end_stream: true, compress: false
      )

    reply = Grpc.Testing.StreamingInputCallResponse.new(aggregated_payload_size: 73086)
    {:ok, ^reply} = GRPC.Stub.recv(stream)
  end

  def server_streaming!(ch) do
    Logger.info("Run server_streaming!")
    params = Enum.map([31415, 9, 2653, 58979], &res_param(&1))
    req = Grpc.Testing.StreamingOutputCallRequest.new(response_parameters: params)
    {:ok, res_enum} = ch |> Grpc.Testing.TestService.Stub.streaming_output_call(req)
    result = Enum.map([31415, 9, 2653, 58979], &String.duplicate(<<0>>, &1)) |> Enum.sort()

    ^result = res_enum |> Enum.map(fn {:ok, res} -> res.payload.body end) |> Enum.sort()
  end

  def server_compressed_streaming!(ch) do
    Logger.info("Run server_compressed_streaming!")
    req = Grpc.Testing.StreamingOutputCallRequest.new(response_parameters: [
      %{compressed: %{value: true},
        size: 31415},
      %{compressed: %{value: false},
        size: 92653}
    ])
    {:ok, res_enum} = ch |> Grpc.Testing.TestService.Stub.streaming_output_call(req)
    result = Enum.map([31415, 92653], &String.duplicate(<<0>>, &1)) |> Enum.sort()

    ^result = res_enum |> Enum.map(fn {:ok, res} -> res.payload.body end) |> Enum.sort()
  end

  def ping_pong!(ch) do
    Logger.info("Run ping_pong!")
    stream = Grpc.Testing.TestService.Stub.full_duplex_call(ch)

    req = fn size1, size2 ->
      Grpc.Testing.StreamingOutputCallRequest.new(
        response_parameters: [res_param(size1)],
        payload: payload(size2)
      )
    end

    GRPC.Stub.send_request(stream, req.(31415, 27182))
    {:ok, res_enum} = GRPC.Stub.recv(stream)
    reply = String.duplicate(<<0>>, 31415)

    {:ok, %{payload: %{body: ^reply}}} =
      Stream.take(res_enum, 1) |> Enum.to_list() |> List.first()

    Enum.each([{9, 8}, {2653, 1828}, {58979, 45904}], fn {res, payload} ->
      GRPC.Stub.send_request(stream, req.(res, payload))
      reply = String.duplicate(<<0>>, res)

      {:ok, %{payload: %{body: ^reply}}} =
        Stream.take(res_enum, 1) |> Enum.to_list() |> List.first()
    end)

    GRPC.Stub.end_stream(stream)
  end

  def empty_stream!(ch) do
    Logger.info("Run empty_stream!")

    {:ok, res_enum} =
      ch
      |> Grpc.Testing.TestService.Stub.full_duplex_call()
      |> GRPC.Stub.end_stream()
      |> GRPC.Stub.recv()

    [] = Enum.to_list(res_enum)
  end

  def custom_metadata!(ch) do
    Logger.info("Run custom_metadata!")
    # UnaryCall
    req = Grpc.Testing.SimpleRequest.new(response_size: 314_159, payload: payload(271_828))
    reply = Grpc.Testing.SimpleResponse.new(payload: payload(314_159))
    headers = %{"x-grpc-test-echo-initial" => "test_initial_metadata_value"}
    # 11250603
    trailers = %{"x-grpc-test-echo-trailing-bin" => 0xABABAB}
    metadata = Map.merge(headers, trailers)

    {:ok, ^reply, %{headers: new_headers, trailers: new_trailers}} =
      Grpc.Testing.TestService.Stub.unary_call(ch, req, metadata: metadata, return_headers: true)

    validate_headers!(new_headers, new_trailers)

    # FullDuplexCall
    req =
      Grpc.Testing.StreamingOutputCallRequest.new(
        response_parameters: [res_param(314_159)],
        payload: payload(271_828)
      )

    {headers, data, trailers} =
      ch
      |> Grpc.Testing.TestService.Stub.full_duplex_call(metadata: metadata, return_headers: true)
      |> GRPC.Stub.send_request(req, end_stream: true)
      |> GRPC.Stub.recv(return_headers: true)
      |> process_full_duplex_response()

    reply = String.duplicate(<<0>>, 314_159)

    %{payload: %{body: ^reply}} = data

    validate_headers!(headers, trailers)
  end

  defp process_full_duplex_response({:ok, res_enum, %{headers: new_headers}}) do
    {:ok, data} = Stream.take(res_enum, 1) |> Enum.to_list() |> List.first()
    {:trailers, new_trailers} = Stream.take(res_enum, 1) |> Enum.to_list() |> List.first()
    {new_headers, data, new_trailers}
  end

  defp process_full_duplex_response({:ok, res_enum}) do
    {:headers, headers} = Stream.take(res_enum, 1) |> Enum.to_list() |> List.first()
    {:ok, data} = Stream.take(res_enum, 1) |> Enum.to_list() |> List.first()
    {:trailers, trailers} = Stream.take(res_enum, 1) |> Enum.to_list() |> List.first()
    {headers, data, trailers}
  end

  def status_code_and_message!(ch) do
    Logger.info("Run status_code_and_message!")

    code = 2
    msg = "test status message"
    status = Grpc.Testing.EchoStatus.new(code: code, message: msg)
    error = GRPC.RPCError.exception(code, msg)

    # UnaryCall
    req = Grpc.Testing.SimpleRequest.new(response_status: status)
    {:error, ^error} = Grpc.Testing.TestService.Stub.unary_call(ch, req)

    # FullDuplexCall
    req = Grpc.Testing.StreamingOutputCallRequest.new(response_status: status)

    {:error, ^error} =
      ch
      |> Grpc.Testing.TestService.Stub.full_duplex_call()
      |> GRPC.Stub.send_request(req, end_stream: true)
      |> GRPC.Stub.recv()
      |> case do
        {:ok, stream} -> Stream.take(stream, 1) |> Enum.to_list() |> List.first()
        error -> error
      end
  end

  def unimplemented_service!(ch) do
    Logger.info("Run unimplemented_service!")
    req = Grpc.Testing.Empty.new()

    {:error, %GRPC.RPCError{status: 12}} =
      Grpc.Testing.TestService.Stub.unimplemented_call(ch, req)
  end

  def cancel_after_begin!(ch) do
    Logger.info("Run cancel_after_begin!")
    stream = Grpc.Testing.TestService.Stub.streaming_input_call(ch)
    stream = GRPC.Stub.cancel(stream)
    error = GRPC.RPCError.exception(1, "The operation was cancelled")
    {:error, ^error} = GRPC.Stub.recv(stream)
  end

  def cancel_after_first_response!(ch) do
    Logger.info("Run cancel_after_first_response!")

    req =
      Grpc.Testing.StreamingOutputCallRequest.new(
        response_parameters: [res_param(31415)],
        payload: payload(27182)
      )

    stream = Grpc.Testing.TestService.Stub.full_duplex_call(ch)

    {:ok, res_enum} =
      stream
      |> GRPC.Stub.send_request(req)
      |> GRPC.Stub.recv()

    {:ok, _} = Stream.take(res_enum, 1) |> Enum.to_list() |> List.first()
    stream = GRPC.Stub.cancel(stream)
    {:error, %GRPC.RPCError{status: 1}} = GRPC.Stub.recv(stream)
  end

  def timeout_on_sleeping_server!(ch) do
    Logger.info("Run timeout_on_sleeping_server!")

    req =
      Grpc.Testing.StreamingOutputCallRequest.new(
        payload: payload(27182),
        response_parameters: [res_param(31415)]
      )

    stream = Grpc.Testing.TestService.Stub.full_duplex_call(ch, timeout: 1)
    resp = stream |> GRPC.Stub.send_request(req) |> GRPC.Stub.recv()

    case resp do
      {:error, %GRPC.RPCError{status: 4}} ->
        :ok

      {:ok, enum} ->
        Enum.each(enum, fn
          {:ok, _msg} ->
            :ok

          {:error, %GRPC.RPCError{status: 4}} ->
            :ok
        end)
    end
  end

  defp validate_headers!(headers, trailers) do
    %{"x-grpc-test-echo-initial" => "test_initial_metadata_value"} = headers
    %{"x-grpc-test-echo-trailing-bin" => "11250603"} = trailers
  end

  defp res_param(size) do
    Grpc.Testing.ResponseParameters.new(size: size)
  end

  defp payload(n) do
    Grpc.Testing.Payload.new(body: String.duplicate(<<0>>, n))
  end
end
