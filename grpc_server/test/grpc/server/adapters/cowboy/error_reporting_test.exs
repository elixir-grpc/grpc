defmodule GRPC.Server.Adapters.Cowboy.ErrorReportingTest do
  # Every way a server RPC can fail must publish `[:grpc, :server, :rpc, :exception]`
  # with `kind`, `reason` and `stacktrace`, without changing the reply or the logs.
  use ExUnit.Case, async: false

  import GRPC.DataCase, only: [attach_telemetry: 1]

  alias GRPC.Server.Adapters.ReportException

  @exception [:grpc, :server, :rpc, :exception]
  @path "/helloworld.Greeter/SayHello"

  defmodule ErrorServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(%{name: "raise"}, _stream), do: raise("boom")
    def say_hello(%{name: "throw"}, _stream), do: throw(:thrown)
    def say_hello(%{name: "exit"}, _stream), do: exit(:bye)

    def say_hello(%{name: "rpc_error"}, _stream),
      do: raise(GRPC.RPCError, status: :not_found, message: "nope")

    # The codec rejects a string field that holds an integer.
    def say_hello(%{name: "encode"}, _stream), do: %Helloworld.HelloReply{message: 123}

    # The client below accepts `identity` only, so the adapter refuses to send gzip.
    def say_hello(%{name: "send"}, stream) do
      GRPC.Server.set_compressor(stream, GRPC.Compressor.Gzip)
      %Helloworld.HelloReply{message: "ok"}
    end

    def say_hello(req, _stream), do: %Helloworld.HelloReply{message: req.name}
  end

  # `:logger` handler that forwards error events to the test process.
  def log(%{level: :error} = event, %{config: pid}), do: send(pid, {:log, event})
  def log(_event, _config), do: :ok

  setup do
    handler_id = :"error_reporting_test_#{System.unique_integer([:positive])}"
    :ok = :logger.add_handler(handler_id, __MODULE__, %{config: self()})
    on_exit(fn -> :logger.remove_handler(handler_id) end)

    attach_telemetry(@exception)
  end

  describe "handler fails" do
    test "raise" do
      assert %{"grpc-status" => "2", "grpc-message" => "Internal%20Server%20Error"} =
               call("raise")

      assert_receive {:telemetry, @exception, %{duration: duration}, metadata}
      assert duration > 0

      assert %{kind: :error, reason: %RuntimeError{message: "boom"}, stacktrace: [_ | _]} =
               metadata

      assert metadata.server == ErrorServer
    end

    test "throw" do
      assert %{"grpc-status" => "2"} = call("throw")

      assert_receive {:telemetry, @exception, _, metadata}
      assert %{kind: :throw, reason: :thrown, stacktrace: [_ | _]} = metadata
    end

    test "exit" do
      assert %{"grpc-status" => "2"} = call("exit")

      assert_receive {:telemetry, @exception, _, metadata}
      assert %{kind: :exit, reason: :bye, stacktrace: [_ | _]} = metadata
    end

    test "RPCError keeps its status" do
      assert %{"grpc-status" => "5", "grpc-message" => "nope"} = call("rpc_error")

      assert_receive {:telemetry, @exception, _, metadata}

      assert %{kind: :error, reason: %GRPC.RPCError{message: "nope"}, stacktrace: [_ | _]} =
               metadata
    end
  end

  describe "encoding the reply fails" do
    test "publishes :exception and replies UNKNOWN" do
      assert %{"grpc-status" => "2", "grpc-message" => "Internal%20Server%20Error"} =
               call("encode")

      assert_receive {:telemetry, @exception, %{duration: duration}, metadata}
      assert duration > 0

      assert %{kind: :error, reason: %Protobuf.EncodeError{}, stacktrace: [_ | _]} = metadata
      assert metadata.server == ErrorServer
      assert metadata.path == @path
      assert metadata.stream.method_name == "SayHello"
      refute_receive {:telemetry, @exception, _, _}, 200
    end
  end

  describe "sending the reply fails" do
    test "publishes :exception and replies INTERNAL" do
      assert %{"grpc-status" => "13", "grpc-message" => message} = call("send")
      assert URI.decode(message) =~ "unaccepted encoding gzip"

      assert_receive {:telemetry, @exception, %{duration: duration}, metadata}
      assert duration > 0

      assert %{kind: :error, reason: %GRPC.RPCError{status: 13}, stacktrace: [_ | _]} = metadata
      assert metadata.server == ErrorServer
      assert metadata.path == @path
      refute_receive {:telemetry, @exception, _, _}, 200
    end
  end

  test "a call that succeeds publishes no :exception" do
    assert %{"grpc-status" => "0"} = call("fine")
    refute_receive {:telemetry, @exception, _, _}, 200
  end

  # The default config logs what it logged before telemetry covered these paths:
  # one `crash_reason` report per failure the adapter already logged, and
  # nothing for a failed send.
  describe "log output with the default config" do
    test "raise logs the handler error and the adapter report" do
      call("raise")

      assert_receive {:log, %{meta: meta}}, 1_000
      refute Map.has_key?(meta, :crash_reason)

      assert_receive {:log,
                      %{
                        meta: %{
                          crash_reason: {%ReportException{reason: %RuntimeError{}}, [_ | _]}
                        }
                      }},
                     1_000

      refute_receive {:log, _}, 200
    end

    test "an encoding failure logs one adapter report" do
      call("encode")

      assert_receive {:log,
                      %{
                        meta: %{
                          crash_reason:
                            {%ReportException{reason: %Protobuf.EncodeError{}}, [_ | _]}
                        }
                      }},
                     1_000

      refute_receive {:log, _}, 200
    end

    test "a send failure logs nothing" do
      call("send")

      refute_receive {:log, _}, 200
    end
  end

  # Calls SayHello over HTTP/2 and returns the `grpc-*` headers of the reply.
  defp call(name) do
    {:ok, _pid, port} =
      start_supervised(%{
        id: {GRPC.Server, System.unique_integer([:positive])},
        start: {GRPC.Server, :start, [ErrorServer, 0, []]},
        type: :worker,
        restart: :permanent,
        shutdown: 500
      })

    try do
      call_port(port, name)
    after
      GRPC.Server.stop(ErrorServer)
    end
  end

  defp call_port(port, name) do
    {:ok, conn} = :gun.open(~c"localhost", port, %{protocols: [:http2]})
    {:ok, :http2} = :gun.await_up(conn, 5_000)

    message = Protobuf.encode(%Helloworld.HelloRequest{name: name})
    body = <<0::8, byte_size(message)::32, message::binary>>

    headers = [
      {"content-type", "application/grpc+proto"},
      {"te", "trailers"},
      {"grpc-accept-encoding", "identity"}
    ]

    ref = :gun.post(conn, @path, headers, body)
    grpc_headers = await_grpc_headers(conn, ref, %{})
    :gun.close(conn)
    grpc_headers
  end

  defp await_grpc_headers(conn, ref, acc) do
    case :gun.await(conn, ref, 5_000) do
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

      {:error, reason} ->
        flunk("gun error: #{inspect(reason)}")
    end
  end

  defp grpc_headers(headers) do
    for {"grpc-" <> _ = key, value} <- headers, into: %{}, do: {key, value}
  end
end
