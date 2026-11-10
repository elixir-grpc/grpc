defmodule GRPC.Server.Adapters.Cowboy.MalformedRequestTest do
  @moduledoc """
  Tests which exercise malformed request behavior.
  """

  use ExUnit.Case, async: false

  defmodule Greeter do
    use GRPC.Server,
      service: Helloworld.Greeter.Service,
      codecs: [GRPC.Codec.Proto, GRPC.Codec.WebText, GRPC.Codec.JSON, GRPC.Codec.Erlpack],
      compressors: [GRPC.Compressor.Gzip]

    def say_hello(_request, _stream), do: %Helloworld.HelloReply{message: "ok"}
  end

  defmodule RouteGuide do
    use GRPC.Server, service: Routeguide.RouteGuide.Service, compressors: [GRPC.Compressor.Gzip]

    def record_route(points, _stream),
      do: %Routeguide.RouteSummary{point_count: Enum.count(points)}
  end

  @servers [Greeter, RouteGuide]
  @reply_timeout_ms 1_000

  frame = fn payload, flag -> <<flag, byte_size(payload)::32, payload::binary>> end
  hello = Protobuf.encode(%Helloworld.HelloRequest{name: "hi"})
  point = Protobuf.encode(%Routeguide.Point{latitude: 1, longitude: 2})

  say_hello = "/helloworld.Greeter/SayHello"
  record_route = "/routeguide.RouteGuide/RecordRoute"
  gzip = [{"grpc-encoding", "gzip"}]

  # {claim, path, content-type, headers, body, status}
  rows = [
    {"a unary field with wire type 7", say_hello, "application/grpc", [], frame.(<<0x0F>>, 0),
     :internal},
    {"a unary fixed32 field cut short", say_hello, "application/grpc", [],
     frame.(<<0x1D, 0x01>>, 0), :internal},
    {"a unary trailers frame", say_hello, "application/grpc", [], frame.("grpc-status: 0", 0x80),
     :internal},
    {"a unary trailers line without a colon", say_hello, "application/grpc", [],
     frame.("nocolon", 0x80), :internal},
    {"a unary message followed by bytes that are not a frame", say_hello, "application/grpc", [],
     frame.(hello, 0) <> <<1, 2>>, :internal},
    {"a unary body shorter than the 5-byte prefix", say_hello, "application/grpc", [], <<0, 0>>,
     :internal},
    {"a grpc-web-text body that is not base64", say_hello, "application/grpc-web-text", [],
     "!!!!", :internal},
    {"a grpc+json message that is not JSON", say_hello, "application/grpc+json", [],
     frame.(<<0>>, 0), :internal},
    {"a grpc+erlpack message that is not an external term", say_hello, "application/grpc+erlpack",
     [], frame.(<<1, 2, 3>>, 0), :internal},
    {"a unary gzip message that does not decompress", say_hello, "application/grpc", gzip,
     frame.("not gzip", 1), :internal},

    {"a unary call with a second message", say_hello, "application/grpc", [],
     frame.(hello, 0) <> frame.(hello, 0), :unimplemented},
    {"a unary call with no message", say_hello, "application/grpc", [], <<>>, :unimplemented},

    {"a client-stream field with wire type 7", record_route, "application/grpc", [],
     frame.(point, 0) <> frame.(<<0x0F>>, 0), :internal},
    {"a client-stream trailers frame", record_route, "application/grpc", [],
     frame.(point, 0) <> frame.("", 0x80), :internal},
    {"a client-stream compressed flag without grpc-encoding", record_route, "application/grpc",
     [], frame.(point, 1), :internal},
    {"a client-stream gzip message that does not decompress", record_route, "application/grpc",
     gzip, frame.("not gzip", 1), :internal},
    {"a client stream that ends inside a message", record_route, "application/grpc", [],
     frame.(point, 0) <> <<0, 5::32, "ab">>, :internal}
  ]

  setup_all do
    {:ok, _pid, port} =
      GRPC.Server.start(@servers, 0, exception_log_filter: {__MODULE__, :report})

    on_exit(fn -> :ok = GRPC.Server.stop(@servers) end)
    %{port: port}
  end

  setup do
    :persistent_term.put({__MODULE__, :test_pid}, self())
    handler_id = :"malformed_request_test_#{System.unique_integer([:positive])}"
    :ok = :logger.add_handler(handler_id, __MODULE__, %{config: self()})

    on_exit(fn ->
      :logger.remove_handler(handler_id)
      :persistent_term.erase({__MODULE__, :test_pid})
    end)
  end

  for {claim, path, content_type, headers, body, status} <- rows do
    test "#{claim} answers #{status |> Atom.to_string() |> String.upcase()}", %{port: port} do
      code = apply(GRPC.Status, unquote(status), [])
      headers = [{"content-type", unquote(content_type)} | unquote(headers)]
      reply = call(port, unquote(path), headers, unquote(Macro.escape(body)))

      assert reply["grpc-status"] == Integer.to_string(code), "replied #{inspect(reply)}"

      assert URI.decode(reply["grpc-message"]) not in ["", "Internal Server Error"],
             "the reply does not say what failed: #{inspect(reply)}"

      assert_receive {:reported, %GRPC.RPCError{status: ^code}}, 1_000
      refute_receive {:log, _event}, 100
    end
  end

  @doc false
  def report(%{reason: reason}) do
    if pid = :persistent_term.get({__MODULE__, :test_pid}, nil),
      do: send(pid, {:reported, reason})

    false
  end

  @doc false
  def log(%{level: :error} = event, %{config: pid}), do: send(pid, {:log, event})
  def log(_event, _config), do: :ok

  defp call(port, path, headers, body) do
    {:ok, conn} = :gun.open(~c"localhost", port, %{protocols: [:http2]})
    {:ok, :http2} = :gun.await_up(conn, 5_000)
    ref = :gun.post(conn, path, [{"te", "trailers"} | headers], body)
    reply = await_grpc_headers(conn, ref, %{})
    :gun.close(conn)
    reply
  end

  defp await_grpc_headers(conn, ref, acc) do
    case :gun.await(conn, ref, @reply_timeout_ms) do
      {:response, :fin, _status, headers} ->
        Map.merge(acc, grpc_headers(headers))

      {:response, :nofin, _status, headers} ->
        await_grpc_headers(conn, ref, Map.merge(acc, grpc_headers(headers)))

      {:data, :fin, _data} ->
        acc

      {:data, :nofin, _data} ->
        await_grpc_headers(conn, ref, acc)

      {:trailers, trailers} ->
        Map.merge(acc, grpc_headers(trailers))

      {:error, :timeout} ->
        flunk("no reply within #{@reply_timeout_ms} ms")

      {:error, reason} ->
        flunk("gun error: #{inspect(reason)}")
    end
  end

  defp grpc_headers(headers) do
    for {"grpc-" <> _ = key, value} <- headers, into: %{}, do: {key, value}
  end
end
