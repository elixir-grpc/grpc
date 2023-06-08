defmodule GRPC.Integration.ServerTest do
  use GRPC.Integration.TestCase

  defmodule FeatureServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def get_feature(point, _stream) do
      Routeguide.Feature.new(location: point, name: "#{point.latitude},#{point.longitude}")
    end
  end

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(%{name: "raise", duration: duration}, _stream) do
      Process.sleep(duration)
      raise ArgumentError, "exception raised"
    end

    def say_hello(%{name: "delay", duration: duration}, _stream) do
      Process.sleep(duration)
      Helloworld.HelloReply.new(message: "Hello")
    end

    def say_hello(%{name: "large response"}, _stream) do
      name = String.duplicate("a", round(:math.pow(2, 14)))
      Helloworld.HelloReply.new(message: "Hello, #{name}")
    end

    def say_hello(%{name: "get peer"}, stream) do
      {ip, _port} = stream.adapter.get_peer(stream.payload)
      name = to_string(:inet_parse.ntoa(ip))
      Helloworld.HelloReply.new(message: "Hello, #{name}")
    end

    def say_hello(%{name: "get cert"}, stream) do
      case stream.adapter.get_cert(stream.payload) do
        :undefined ->
          Helloworld.HelloReply.new(message: "Hello, unauthenticated")

        _ ->
          Helloworld.HelloReply.new(message: "Hello, authenticated")
      end
    end

    def say_hello(req, _stream) do
      Helloworld.HelloReply.new(message: "Hello, #{req.name}")
    end

    def check_headers(_req, stream) do
      token = GRPC.Stream.get_headers(stream)["authorization"]
      Helloworld.HeaderReply.new(authorization: token)
    end
  end

  defmodule HelloErrorServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(%{name: "unknown error"}, _stream) do
      raise "unknown error(This is a test, please ignore it)"
    end

    def say_hello(_req, _stream) do
      raise GRPC.RPCError, status: GRPC.Status.unauthenticated(), message: "Please authenticate"
    end
  end

  defmodule FeatureErrorServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service
    alias GRPC.Server

    def list_features(rectangle, stream) do
      raise GRPC.RPCError, status: GRPC.Status.unauthenticated(), message: "Please authenticate"

      Enum.each([rectangle.lo, rectangle.hi], fn point ->
        feature = simple_feature(point)
        Server.send_reply(stream, feature)
      end)
    end

    defp simple_feature(point) do
      Routeguide.Feature.new(location: point, name: "#{point.latitude},#{point.longitude}")
    end
  end

  defmodule TimeoutServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def list_features(_rectangle, _stream) do
      Process.sleep(600)
    end
  end

  defmodule SlowServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def list_features(rectangle, stream) do
      Process.sleep(400)

      Enum.each([rectangle.lo, rectangle.hi], fn point ->
        feature = simple_feature(point)
        GRPC.Server.send_reply(stream, feature)
      end)
    end

    defp simple_feature(point) do
      Routeguide.Feature.new(location: point, name: "#{point.latitude},#{point.longitude}")
    end
  end

  defmodule HTTP1Server do
    def init(req, state) do
      {:ok, :cowboy_req.reply(200, %{}, "OK", req), state}
    end
  end

  test "multiple servers works" do
    run_server([FeatureServer, HelloServer], fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      point = Routeguide.Point.new(latitude: 409_146_138, longitude: -746_188_906)
      {:ok, feature} = channel |> Routeguide.RouteGuide.Stub.get_feature(point)
      assert feature == Routeguide.Feature.new(location: point, name: "409146138,-746188906")

      req = Helloworld.HelloRequest.new(name: "Elixir")
      {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
      assert reply.message == "Hello, Elixir"
    end)
  end

  test "HTTP/1 status handler can be started along a gRPC server" do
    status_handler = {"/status", HTTP1Server, []}

    run_server(
      [HelloServer],
      fn port ->
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
        req = Helloworld.HelloRequest.new(name: "Elixir")
        {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
        assert reply.message == "Hello, Elixir"

        {:ok, conn_pid} = :gun.open('localhost', port)
        stream_ref = :gun.get(conn_pid, "/status")

        assert_receive {:gun_response, ^conn_pid, ^stream_ref, :nofin, 200, _headers}
      end,
      0,
      adapter_opts: [status_handler: status_handler]
    )
  end

  test "returns appropriate error for unary requests" do
    run_server([HelloErrorServer], fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      req = Helloworld.HelloRequest.new(name: "Elixir")
      {:error, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)

      assert %GRPC.RPCError{
               status: GRPC.Status.unauthenticated(),
               message: "Please authenticate"
             } == reply
    end)
  end

  test "return errors for unknown errors" do
    run_server([HelloErrorServer], fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      req = Helloworld.HelloRequest.new(name: "unknown error")

      assert {:error,
              %GRPC.RPCError{message: "Internal Server Error", status: GRPC.Status.unknown()}} ==
               channel |> Helloworld.Greeter.Stub.say_hello(req)
    end)
  end

  test "returns appropriate error for stream requests" do
    run_server([FeatureErrorServer], fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      rect = Routeguide.Rectangle.new()
      error = %GRPC.RPCError{message: "Please authenticate", status: 16}
      assert {:error, ^error} = channel |> Routeguide.RouteGuide.Stub.list_features(rect)
    end)
  end

  test "return large response(more than MAX_FRAME_SIZE 16384)" do
    run_server([HelloServer], fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      req = Helloworld.HelloRequest.new(name: "large response")
      {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
      name = String.duplicate("a", round(:math.pow(2, 14)))
      assert "Hello, #{name}" == reply.message
    end)
  end

  test "return deadline error for slow server" do
    run_server([TimeoutServer], fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      rect = Routeguide.Rectangle.new()
      error = %GRPC.RPCError{message: "Deadline expired", status: 4}

      assert {:error, ^error} =
               channel |> Routeguide.RouteGuide.Stub.list_features(rect, timeout: 500)
    end)
  end

  test "return normally for a little slow server" do
    run_server([SlowServer], fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      low = Routeguide.Point.new(latitude: 400_000_000, longitude: -750_000_000)
      high = Routeguide.Point.new(latitude: 420_000_000, longitude: -730_000_000)
      rect = Routeguide.Rectangle.new(lo: low, hi: high)
      {:ok, stream} = channel |> Routeguide.RouteGuide.Stub.list_features(rect, timeout: 500)

      Enum.each(stream, fn {:ok, feature} ->
        assert feature
      end)
    end)
  end

  test "headers set on channel are present in receiving server" do
    run_server([HelloServer], fn port ->
      token = "Bearer TOKEN"

      {:ok, channel} =
        GRPC.Stub.connect("localhost:#{port}",
          headers: [{"authorization", token}]
        )

      {:ok, reply} =
        channel |> Helloworld.Greeter.Stub.check_headers(Helloworld.HeaderRequest.new())

      assert reply.authorization == token
    end)
  end

  test "get peer returns correct IP address" do
    run_server([HelloServer], fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

      req = Helloworld.HelloRequest.new(name: "get peer")
      {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
      assert reply.message == "Hello, 127.0.0.1"
    end)
  end

  test "get cert returns correct client certificate when not present" do
    run_server([HelloServer], fn port ->
      assert {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

      req = Helloworld.HelloRequest.new(name: "get cert")
      assert {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
      assert reply.message == "Hello, unauthenticated"
    end)
  end

  describe "telemetry" do
    test "sends server start+stop events on success" do
      server_rpc_prefix = GRPC.Telemetry.server_rpc_prefix()
      start_server_name = server_rpc_prefix ++ [:start]
      stop_server_name = server_rpc_prefix ++ [:stop]
      exception_server_name = server_rpc_prefix ++ [:exception]

      client_rpc_prefix = GRPC.Telemetry.client_rpc_prefix()
      start_client_name = client_rpc_prefix ++ [:start]
      stop_client_name = client_rpc_prefix ++ [:stop]
      exception_client_name = client_rpc_prefix ++ [:exception]

      attach_events([
        start_server_name,
        stop_server_name,
        exception_server_name,
        start_client_name,
        stop_client_name,
        exception_client_name
      ])

      run_server([HelloServer], fn port ->
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

        req = Helloworld.HelloRequest.new(name: "delay", duration: 1000)

        assert {:ok, _} = Helloworld.Greeter.Stub.say_hello(channel, req)
      end)

      assert_received {^start_server_name, measurements, metadata}
      assert %{monotonic_time: _, system_time: _} = measurements

      assert %{
               server: HelloServer,
               endpoint: nil,
               function_name: :say_hello,
               stream: %GRPC.Server.Stream{}
             } = metadata

      assert_received {^stop_server_name, measurements, metadata}
      assert %{duration: duration} = measurements
      assert duration > 1000

      assert %{
               server: HelloServer,
               endpoint: nil,
               function_name: :say_hello,
               stream: %GRPC.Server.Stream{}
             } = metadata

      assert_received {:gun_down, _, _, _, _}

      assert_received {^start_client_name, measurements, metadata}
      assert %{count: 1} == measurements

      assert %{
               stream: %GRPC.Client.Stream{
                 rpc:
                   {"say_hello", {Helloworld.HelloRequest, false}, {Helloworld.HelloReply, false}}
               }
             } = metadata

      assert_received {^stop_client_name, measurements, metadata}
      assert %{duration: duration} = measurements
      assert duration > 1100

      assert %{
               stream: %GRPC.Client.Stream{
                 rpc:
                   {"say_hello", {Helloworld.HelloRequest, false}, {Helloworld.HelloReply, false}}
               }
             } = metadata

      refute_receive _
    end

    test "sends server start+exception events on success" do
      server_rpc_prefix = GRPC.Telemetry.server_rpc_prefix()
      start_server_name = server_rpc_prefix ++ [:start]
      stop_server_name = server_rpc_prefix ++ [:stop]
      exception_server_name = server_rpc_prefix ++ [:exception]

      client_rpc_prefix = GRPC.Telemetry.client_rpc_prefix()
      start_client_name = client_rpc_prefix ++ [:start]
      stop_client_name = client_rpc_prefix ++ [:stop]
      exception_client_name = client_rpc_prefix ++ [:exception]

      attach_events([
        start_server_name,
        stop_server_name,
        exception_server_name,
        start_client_name,
        stop_client_name,
        exception_client_name
      ])

      run_server([HelloServer], fn port ->
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

        req = Helloworld.HelloRequest.new(name: "raise", duration: 1100)

        assert {:error, %GRPC.RPCError{status: 2}} =
                 Helloworld.Greeter.Stub.say_hello(channel, req)
      end)

      assert_received {^start_server_name, measurements, metadata}
      assert %{monotonic_time: _, system_time: _} = measurements

      assert %{
               server: HelloServer,
               endpoint: nil,
               function_name: :say_hello,
               stream: %GRPC.Server.Stream{}
             } = metadata

      assert_received {^exception_server_name, measurements, metadata}
      assert %{duration: duration} = measurements
      assert duration > 1100

      assert %{
               server: HelloServer,
               endpoint: nil,
               function_name: :say_hello,
               stream: %GRPC.Server.Stream{},
               kind: :error,
               reason: %ArgumentError{message: "exception raised"},
               stacktrace: stacktrace
             } = metadata

      assert is_list(stacktrace)

      Enum.each(stacktrace, fn entry ->
        # ensure stacktrace is a pure stacktrace
        assert {mod, fun, arity, meta} = entry
        assert is_atom(mod)
        assert is_atom(fun)
        assert is_integer(arity)
        assert is_list(meta)
      end)

      assert_received {^start_client_name, measurements, metadata}
      assert %{count: 1} = measurements

      assert %{
               stream: %GRPC.Client.Stream{
                 rpc:
                   {"say_hello", {Helloworld.HelloRequest, false}, {Helloworld.HelloReply, false}}
               }
             } = metadata

      assert_received {^stop_client_name, measurements, metadata}
      assert %{duration: duration} = measurements
      assert duration > 1100

      assert %{
               stream: %GRPC.Client.Stream{
                 rpc:
                   {"say_hello", {Helloworld.HelloRequest, false}, {Helloworld.HelloReply, false}}
               }
             } = metadata

      assert_received {:gun_down, _, _, _, _}

      refute_receive _
    end
  end
end
