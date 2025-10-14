defmodule Interop.Server do
  @moduledoc false
  use GRPC.Server, service: Grpc.Testing.TestService.Service, compressors: [GRPC.Compressor.Gzip]

  import ExUnit.Assertions, only: [assert: 1, refute: 1]

  def empty_call(_, _stream) do
    Grpc.Testing.Empty.new()
  end

  def unary_call(req, stream) do
    if req.expect_compressed do
      headers = GRPC.Stream.get_headers(stream)
      if req.expect_compressed.value do
        assert %{"grpc-encoding" => "gzip"} = headers
      else
        refute headers["grpc-encoding"]
      end
    end

    case req do
      %{response_compressed: %{value: true}} ->
        GRPC.Server.set_compressor(stream, GRPC.Compressor.Gzip)
      _ -> :ok
    end

    handle_metadata(stream)
    status = req.response_status

    if status && status.code != 0 do
      raise GRPC.RPCError, status: status.code, message: status.message
    end

    payload = Grpc.Testing.Payload.new(body: String.duplicate(<<0>>, req.response_size))
    Grpc.Testing.SimpleResponse.new(payload: payload)
  end

  def streaming_input_call(req_enum, _stream) do
    size = Enum.reduce(req_enum, 0, fn req, acc ->
      acc + byte_size(req.payload.body)
    end)
    Grpc.Testing.StreamingInputCallResponse.new(aggregated_payload_size: size)
  end

  def streaming_output_call(req, stream) do
    GRPC.Server.set_compressor(stream, GRPC.Compressor.Gzip)
    Enum.map(req.response_parameters, fn params ->
      resp = Grpc.Testing.StreamingOutputCallResponse.new(payload: %{body: String.duplicate(<<0>>, params.size)})
      opts = if params.compressed == false do
        [compress: false]
      else
        []
      end
      GRPC.Server.send_reply(stream, resp, opts)
    end)
    # req.response_parameters
    # |> Enum.map(&Grpc.Testing.Payload.new(body: String.duplicate(<<0>>, &1.size)))
    # |> Enum.map(&Grpc.Testing.StreamingOutputCallResponse.new(payload: &1))
    # |> Enum.each(&GRPC.Server.send_reply(stream, &1))
  end

  def full_duplex_call(req_enum, stream) do
    handle_metadata(stream)

    Enum.each(req_enum, fn req ->
      status = req.response_status

      if status && status.code != 0 do
        raise GRPC.RPCError, status: status.code, message: status.message
      end

      resp_param = List.first(req.response_parameters)

      if resp_param do
        size = resp_param.size
        payload = Grpc.Testing.Payload.new(body: String.duplicate(<<0>>, size))
        res = Grpc.Testing.StreamingOutputCallResponse.new(payload: payload)
        GRPC.Server.send_reply(stream, res)
      end
    end)
  end

  defp handle_metadata(stream) do
    headers = GRPC.Stream.get_headers(stream)

    if header = headers["x-grpc-test-echo-initial"] do
      GRPC.Server.send_headers(stream, %{"x-grpc-test-echo-initial" => header})
    end

    if header = headers["x-grpc-test-echo-trailing-bin"] do
      GRPC.Server.set_trailers(stream, %{"x-grpc-test-echo-trailing-bin" => header})
    end
  end
end
