defmodule GRPC.Server.Adapters.Cowboy.ErrorReportingTest do
  # Each failure must publish a signal with `kind`, `reason` and `stacktrace`.
  # The reply and the logs must not change.
  use ExUnit.Case, async: false

  import GRPC.DataCase, only: [attach_telemetry: 1]

  alias GRPC.Server.Adapters.ReportException

  @exception [:grpc, :server, :rpc, :exception]
  @send_error [:grpc, :server, :rpc, :send_error]
  @abort [:grpc, :server, :rpc, :abort]
  @stop [:grpc, :server, :rpc, :stop]
  @path "/helloworld.Greeter/SayHello"

  # Compressors that fail. The client accepts both names.
  defmodule RaisingCompressor do
    @behaviour GRPC.Compressor
    def name, do: "raising"
    def compress(_data), do: raise("compress failed")
    def decompress(data), do: data
  end

  # Gives iodata a little larger than the 2 GiB limit. It uses only 1 MiB of memory.
  defmodule HugeCompressor do
    @behaviour GRPC.Compressor
    def name, do: "huge"
    def compress(_data), do: List.duplicate(:binary.copy(<<0>>, 1_048_576), 2049)
    def decompress(data), do: data
  end

  defmodule ErrorServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(%{name: "raise"}, _stream), do: raise("boom")
    def say_hello(%{name: "throw"}, _stream), do: throw(:thrown)
    def say_hello(%{name: "exit"}, _stream), do: exit(:bye)

    def say_hello(%{name: "rpc_error"}, _stream),
      do: raise(GRPC.RPCError, status: :not_found, message: "nope")

    # The codec cannot encode an integer in a string field.
    def say_hello(%{name: "encode"}, _stream), do: %Helloworld.HelloReply{message: 123}

    # The client does not accept gzip, so the adapter does not send the reply.
    def say_hello(%{name: "send"}, stream) do
      GRPC.Server.set_compressor(stream, GRPC.Compressor.Gzip)
      %Helloworld.HelloReply{message: "ok"}
    end

    def say_hello(%{name: "raising_compressor"}, stream) do
      GRPC.Server.set_compressor(stream, RaisingCompressor)
      %Helloworld.HelloReply{message: "ok"}
    end

    def say_hello(%{name: "oversize"}, stream) do
      GRPC.Server.set_compressor(stream, HugeCompressor)
      %Helloworld.HelloReply{message: "ok"}
    end

    def say_hello(req, _stream), do: %Helloworld.HelloReply{message: req.name}
  end

  # Sends replies until the adapter stops it. Sends its pid to the test.
  defmodule StreamServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def list_features(_rectangle, stream) do
      send(:persistent_term.get({__MODULE__, :test_pid}), {:handler, self()})

      # The limit stops the handler if a test fails.
      Enum.each(1..500, fn i ->
        GRPC.Server.send_reply(stream, %Routeguide.Feature{name: "feature #{i}"})

        receive do
        after
          10 -> :ok
        end
      end)
    end
  end

  # `:logger` handler. It sends error events to the test process.
  def log(%{level: :error} = event, %{config: pid}), do: send(pid, {:log, event})
  def log(_event, _config), do: :ok

  setup do
    handler_id = :"error_reporting_test_#{System.unique_integer([:positive])}"
    :ok = :logger.add_handler(handler_id, __MODULE__, %{config: self()})
    on_exit(fn -> :logger.remove_handler(handler_id) end)

    attach_telemetry(@exception)
    attach_telemetry(@send_error)
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
    test "publishes :send_error and replies UNKNOWN" do
      assert %{"grpc-status" => "2", "grpc-message" => "Internal%20Server%20Error"} =
               call("encode")

      assert_receive {:telemetry, @send_error, %{duration: duration}, metadata}
      assert duration > 0

      assert %{kind: :error, reason: %Protobuf.EncodeError{}, stacktrace: [_ | _]} = metadata
      assert metadata.server == ErrorServer
      assert metadata.path == @path
      assert metadata.stream.method_name == "SayHello"
      refute_receive {:telemetry, @send_error, _, %{server: ErrorServer}}, 200
      refute_receive {:telemetry, @exception, _, %{server: ErrorServer}}, 10
    end
  end

  describe "sending the reply fails" do
    test "an unaccepted compressor publishes :send_error and replies INTERNAL" do
      assert %{"grpc-status" => "13", "grpc-message" => message} = call("send")
      assert URI.decode(message) =~ "unaccepted encoding gzip"

      assert_receive {:telemetry, @send_error, %{duration: duration}, metadata}
      assert duration > 0

      assert %{kind: :error, reason: %GRPC.RPCError{status: 13}, stacktrace: [_ | _]} = metadata
      assert metadata.server == ErrorServer
      assert metadata.path == @path
      refute_receive {:telemetry, @send_error, _, %{server: ErrorServer}}, 200
      refute_receive {:telemetry, @exception, _, %{server: ErrorServer}}, 10
    end

    # The compressor runs in the Cowboy handler process, not in the RPC process.
    test "a compressor that raises publishes :send_error and replies INTERNAL" do
      assert %{"grpc-status" => "13", "grpc-message" => "Internal%20Server%20Error"} =
               call("raising_compressor")

      assert_receive {:telemetry, @send_error, %{duration: duration}, metadata}
      assert duration > 0

      assert %{kind: :error, reason: %RuntimeError{message: "compress failed"}} = metadata
      assert [_ | _] = metadata.stacktrace
      assert metadata.server == ErrorServer
      assert metadata.path == @path
      refute_receive {:telemetry, @send_error, _, %{server: ErrorServer}}, 200
      refute_receive {:telemetry, @exception, _, %{server: ErrorServer}}, 10
    end

    # The adapter uses the 2 GiB default limit, so the reply must be larger than 2 GiB.
    test "a reply over the message limit publishes :send_error and replies RESOURCE_EXHAUSTED" do
      assert %{"grpc-status" => "8", "grpc-message" => message} = call("oversize")
      assert URI.decode(message) =~ "Encoded message is too large"

      assert_receive {:telemetry, @send_error, %{duration: duration}, metadata}
      assert duration > 0

      assert %{kind: :error, reason: %GRPC.RPCError{status: 8}, stacktrace: [_ | _]} = metadata
      assert metadata.reason.message =~ "Encoded message is too large"
      assert metadata.server == ErrorServer
      assert metadata.path == @path
      refute_receive {:telemetry, @send_error, _, %{server: ErrorServer}}, 200
      refute_receive {:telemetry, @exception, _, %{server: ErrorServer}}, 10
    end
  end

  # Nothing raises when the client disconnects. The adapter publishes only `:abort`.
  describe "the client disconnects while the reply is sent" do
    setup do
      attach_telemetry(@abort)
      attach_telemetry(@stop)
      :persistent_term.put({StreamServer, :test_pid}, self())
      on_exit(fn -> :persistent_term.erase({StreamServer, :test_pid}) end)
    end

    for how <- [:socket_closed, :connection_closed, :stream_cancelled] do
      test "#{how} publishes :abort and nothing else" do
        with_server(StreamServer, fn port ->
          {:ok, conn} = :gun.open(~c"localhost", port, %{protocols: [:http2]})
          {:ok, :http2} = :gun.await_up(conn, 5_000)

          message = Protobuf.encode(%Routeguide.Rectangle{})
          body = <<0::8, byte_size(message)::32, message::binary>>

          headers = [{"content-type", "application/grpc+proto"}, {"te", "trailers"}]
          ref = :gun.post(conn, "/routeguide.RouteGuide/ListFeatures", headers, body)

          assert_receive {:handler, handler}, 5_000
          monitor = Process.monitor(handler)

          # The server has started to send the reply.
          assert {:response, :nofin, 200, _headers} = :gun.await(conn, ref, 5_000)
          assert {:data, :nofin, _data} = :gun.await(conn, ref, 5_000)

          disconnect(unquote(how), conn, ref)

          assert_receive {:telemetry, @abort, %{duration: duration},
                          %{server: StreamServer} = metadata},
                         5_000

          assert duration > 0
          assert metadata.path == "/routeguide.RouteGuide/ListFeatures"
          assert metadata.server == StreamServer
          assert metadata.pid == handler
          assert metadata.reason == :shutdown

          assert_receive {:DOWN, ^monitor, :process, ^handler, _reason}, 5_000

          refute_receive {:telemetry, @abort, _, %{server: StreamServer}}, 200
          refute_receive {:telemetry, @stop, _, %{server: StreamServer}}, 10
          refute_receive {:telemetry, @exception, _, %{server: StreamServer}}, 10
          refute_receive {:telemetry, @send_error, _, %{server: StreamServer}}, 10

          # The default config logs nothing.
          assert [] = error_logs_mentioning(StreamServer)
        end)
      end
    end
  end

  test "a call that succeeds publishes no :exception" do
    assert %{"grpc-status" => "0"} = call("fine")
    refute_receive {:telemetry, @exception, _, %{server: ErrorServer}}, 200
  end

  # Each logged failure gives one `crash_reason` report. A failed send logs nothing.
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

    test "a reply over the message limit logs nothing" do
      call("oversize")

      refute_receive {:log, _}, 200
    end

    # Before, Cowboy logged the crash of its handler process. Now the adapter logs
    # it one time, with `crash_reason`.
    test "a raising compressor logs one adapter report" do
      call("raising_compressor")

      assert_receive {:log,
                      %{
                        meta: %{
                          crash_reason:
                            {%ReportException{reason: %RuntimeError{message: "compress failed"}},
                             [_ | _]}
                        }
                      }},
                     1_000

      refute_receive {:log, _}, 200
    end
  end

  # Gives the error logs that name `module`. RPCs from other tests can also log errors.
  defp error_logs_mentioning(module) do
    receive do
      {:log, event} ->
        if event |> inspect() |> String.contains?(inspect(module)),
          do: [event | error_logs_mentioning(module)],
          else: error_logs_mentioning(module)
    after
      10 -> []
    end
  end

  # `:socket_closed` stops the Gun process. The socket closes without an HTTP/2 GOAWAY.
  defp disconnect(:socket_closed, conn, _ref), do: Process.exit(conn, :kill)
  defp disconnect(:connection_closed, conn, _ref), do: :gun.close(conn)
  defp disconnect(:stream_cancelled, conn, ref), do: :gun.cancel(conn, ref)

  # Calls SayHello over HTTP/2 and returns the `grpc-*` headers of the reply.
  defp call(name), do: with_server(ErrorServer, &call_port(&1, name))

  defp with_server(server, fun) do
    {:ok, _pid, port} =
      start_supervised(%{
        id: {GRPC.Server, System.unique_integer([:positive])},
        start: {GRPC.Server, :start, [server, 0, []]},
        type: :worker,
        restart: :permanent,
        shutdown: 500
      })

    try do
      fun.(port)
    after
      GRPC.Server.stop(server)
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
      {"grpc-accept-encoding", "identity,raising,huge"}
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
