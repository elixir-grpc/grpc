defmodule GRPC.Integration.ServerTest do
  use GRPC.Integration.TestCase

  defmodule FeatureServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def get_feature(point, _stream) do
      %Routeguide.Feature{location: point, name: "#{point.latitude},#{point.longitude}"}
    end

    def route_chat(_ex_stream, stream) do
      GRPC.Server.send_headers(stream, %{})
      Process.exit(self(), :shutdown)
      Process.sleep(500)
    end
  end

  defmodule TranscodeErrorServer do
    use GRPC.Server,
      service: Transcode.Messaging.Service,
      http_transcode: true

    def get_message(req, _stream) do
      status = String.to_existing_atom(req.name)

      raise GRPC.RPCError, status: status
    end
  end

  defmodule TranscodeServer do
    use GRPC.Server,
      service: Transcode.Messaging.Service,
      http_transcode: true

    def get_message(msg_request, _stream) do
      %Transcode.Message{name: msg_request.name, text: "get_message"}
    end

    def stream_messages(msg_request, stream) do
      Enum.each(1..5, fn i ->
        msg = %Transcode.Message{
          name: msg_request.name,
          text: "#{i}"
        }

        GRPC.Server.send_reply(stream, msg)
      end)
    end

    def create_message(msg, _stream) do
      msg
    end

    def create_message_with_nested_body(msg_request, _stream) do
      %Transcode.Message{
        name: msg_request.message.name,
        text: "create_message_with_nested_body"
      }
    end

    def get_message_with_field_path(msg_request, _) do
      msg_request.message
    end

    def get_message_with_response_body(msg_request, _) do
      %Transcode.MessageOut{
        response: %Transcode.Message{
          name: msg_request.name,
          text: "get_message_with_response_body"
        }
      }
    end

    def get_message_with_query(msg_request, _stream) do
      %Transcode.Message{name: msg_request.name, text: "get_message_with_query"}
    end

    def get_message_with_subpath_query(msg_request, _stream) do
      %Transcode.Message{
        name: msg_request.message.name,
        text: "get_message_with_subpath_query"
      }
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
      %Helloworld.HelloReply{message: "Hello"}
    end

    def say_hello(%{name: "large response"}, _stream) do
      name = String.duplicate("a", round(:math.pow(2, 14)))
      %Helloworld.HelloReply{message: "Hello, #{name}"}
    end

    def say_hello(%{name: "get peer"}, stream) do
      {ip, _port} = stream.adapter.get_peer(stream.payload)
      name = to_string(:inet_parse.ntoa(ip))
      %Helloworld.HelloReply{message: "Hello, #{name}"}
    end

    def say_hello(%{name: "get cert"}, stream) do
      case stream.adapter.get_cert(stream.payload) do
        :undefined ->
          %Helloworld.HelloReply{message: "Hello, unauthenticated"}

        _ ->
          %Helloworld.HelloReply{message: "Hello, authenticated"}
      end
    end

    def say_hello(req, _stream) do
      %Helloworld.HelloReply{message: "Hello, #{req.name}"}
    end

    def check_headers(_req, stream) do
      token = GRPC.Stream.get_headers(stream)["authorization"]
      %Helloworld.HeaderReply{authorization: token}
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
      %Routeguide.Feature{location: point, name: "#{point.latitude},#{point.longitude}"}
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
      %Routeguide.Feature{location: point, name: "#{point.latitude},#{point.longitude}"}
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
      point = %Routeguide.Point{latitude: 409_146_138, longitude: -746_188_906}
      {:ok, feature} = channel |> Routeguide.RouteGuide.Stub.get_feature(point)
      assert feature == %Routeguide.Feature{location: point, name: "409146138,-746188906"}

      req = %Helloworld.HelloRequest{name: "Elixir"}
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
        req = %Helloworld.HelloRequest{name: "Elixir"}
        {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
        assert reply.message == "Hello, Elixir"

        {:ok, conn_pid} = :gun.open(~c"localhost", port)
        stream_ref = :gun.get(conn_pid, "/status")
        Process.sleep(100)

        assert_received {:gun_response, ^conn_pid, ^stream_ref, :nofin, 200, _headers}
      end,
      0,
      adapter_opts: [status_handler: status_handler]
    )
  end

  test "returns appropriate error for unary requests" do
    logs =
      ExUnit.CaptureLog.capture_log(fn ->
        run_server([HelloErrorServer], fn port ->
          {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
          req = %Helloworld.HelloRequest{name: "Elixir"}
          {:error, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)

          assert %GRPC.RPCError{
                   status: GRPC.Status.unauthenticated(),
                   message: "Please authenticate"
                 } == reply
        end)
      end)

    assert logs =~ "Exception raised while handling /helloworld.Greeter/SayHello"
  end

  test "return errors for unknown errors" do
    logs =
      ExUnit.CaptureLog.capture_log(fn ->
        run_server([HelloErrorServer], fn port ->
          {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
          req = %Helloworld.HelloRequest{name: "unknown error"}

          assert {:error,
                  %GRPC.RPCError{message: "Internal Server Error", status: GRPC.Status.unknown()}} ==
                   channel |> Helloworld.Greeter.Stub.say_hello(req)
        end)
      end)

    assert logs =~ "Exception raised while handling /helloworld.Greeter/SayHello"
  end

  test "returns appropriate error for stream requests" do
    run_server([FeatureErrorServer], fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      rect = %Routeguide.Rectangle{}
      error = %GRPC.RPCError{message: "Please authenticate", status: 16}
      assert {:error, ^error} = channel |> Routeguide.RouteGuide.Stub.list_features(rect)
    end)
  end

  test "return large response(more than MAX_FRAME_SIZE 16384)" do
    run_server([HelloServer], fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      req = %Helloworld.HelloRequest{name: "large response"}
      {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
      name = String.duplicate("a", round(:math.pow(2, 14)))
      assert "Hello, #{name}" == reply.message
    end)
  end

  test "return deadline error for slow server" do
    run_server([TimeoutServer], fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      rect = %Routeguide.Rectangle{}
      error = %GRPC.RPCError{message: "Deadline expired", status: 4}

      assert {:error, ^error} =
               channel |> Routeguide.RouteGuide.Stub.list_features(rect, timeout: 500)
    end)
  end

  test "return normally for a little slow server" do
    run_server([SlowServer], fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      low = %Routeguide.Point{latitude: 400_000_000, longitude: -750_000_000}
      high = %Routeguide.Point{latitude: 420_000_000, longitude: -730_000_000}
      rect = %Routeguide.Rectangle{lo: low, hi: high}
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

      {:ok, reply} = channel |> Helloworld.Greeter.Stub.check_headers(%Helloworld.HeaderRequest{})

      assert reply.authorization == token
    end)
  end

  test "get peer returns correct IP address" do
    run_server([HelloServer], fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

      req = %Helloworld.HelloRequest{name: "get peer"}
      {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
      assert reply.message == "Hello, 127.0.0.1"
    end)
  end

  test "get cert returns correct client certificate when not present" do
    run_server([HelloServer], fn port ->
      assert {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

      req = %Helloworld.HelloRequest{name: "get cert"}
      assert {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
      assert reply.message == "Hello, unauthenticated"
    end)
  end

  test "gracefully handles server shutdown disconnects" do
    logs =
      ExUnit.CaptureLog.capture_log(fn ->
        run_server(FeatureServer, fn port ->
          {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
          client_stream = Routeguide.RouteGuide.Stub.route_chat(channel)
          assert %GRPC.Client.Stream{} = client_stream
          {:ok, ex_stream} = GRPC.Stub.recv(client_stream, timeout: :infinity)
          assert [{:error, %GRPC.RPCError{status: 13}}] = Enum.into(ex_stream, [])
        end)
      end)

    assert logs == ""
  end

  describe "http/json transcode" do
    test "grpc method can be called using json when http_transcode == true" do
      run_server([TranscodeServer], fn port ->
        name = "direct_call"

        {:ok, conn_pid} = :gun.open(~c"localhost", port)

        stream_ref =
          :gun.post(
            conn_pid,
            "/transcode.Messaging/GetMessage",
            [
              {"content-type", "application/json"}
            ],
            Jason.encode!(%{"name" => name})
          )

        assert_receive {:gun_up, ^conn_pid, :http}
        assert {:ok, body} = :gun.await_body(conn_pid, stream_ref)

        assert %{"text" => "get_message"} = Jason.decode!(body)
      end)
    end

    test "should map grpc error codes to http status" do
      run_server([TranscodeErrorServer], fn port ->
        for {code_name, _status} <- [
              {"cancelled", 400},
              {"unknown", 500},
              {"invalid_argument", 400},
              {"deadline_exceeded", 504},
              {"not_found", 404},
              {"already_exists", 409},
              {"permission_denied", 403},
              {"resource_exhausted", 429},
              {"failed_precondition", 412},
              {"aborted", 409},
              {"out_of_range", 400},
              {"unimplemented", 501},
              {"internal", 500},
              {"unavailable", 503},
              {"data_loss", 500},
              {"unauthenticated", 401}
            ] do
          {:ok, conn_pid} = :gun.open(~c"localhost", port)

          _stream_ref =
            :gun.get(
              conn_pid,
              "/v1/messages/#{code_name}",
              [
                {"accept", "application/json"}
              ]
            )

          assert_receive {:gun_up, ^conn_pid, :http}
        end
      end)
    end

    test "accept: application/json can be used with get requests" do
      run_server([TranscodeServer], fn port ->
        name = "direct_call"

        {:ok, conn_pid} = :gun.open(~c"localhost", port)

        stream_ref =
          :gun.get(conn_pid, "/v1/messages/#{name}", [
            {"accept", "application/json"}
          ])

        assert_receive {:gun_response, ^conn_pid, ^stream_ref, :nofin, 200, _headers}
        assert {:ok, body} = :gun.await_body(conn_pid, stream_ref)

        assert %{"text" => "get_message"} = Jason.decode!(body)
      end)
    end

    test "can transcode path params" do
      run_server([TranscodeServer], fn port ->
        name = "foo"

        {:ok, conn_pid} = :gun.open(~c"localhost", port)

        stream_ref =
          :gun.get(conn_pid, "/v1/messages/#{name}", [
            {"content-type", "application/json"}
          ])

        assert_receive {:gun_response, ^conn_pid, ^stream_ref, :nofin, 200, _headers}
        assert {:ok, body} = :gun.await_body(conn_pid, stream_ref)

        assert %{
                 "name" => ^name,
                 "text" => _name
               } = Jason.decode!(body)
      end)
    end

    test "can transcode query params" do
      run_server([TranscodeServer], fn port ->
        {:ok, conn_pid} = :gun.open(~c"localhost", port)

        stream_ref =
          :gun.get(conn_pid, "/v1/messages?name=some_name", [
            {"content-type", "application/json"}
          ])

        assert_receive {:gun_up, ^conn_pid, :http}
        assert {:ok, body} = :gun.await_body(conn_pid, stream_ref)

        assert %{
                 "name" => "some_name",
                 "text" => "get_message_with_query"
               } = Jason.decode!(body)
      end)
    end

    test "can map request body using HttpRule.body and response using HttpRule.response_body" do
      run_server([TranscodeServer], fn port ->
        {:ok, conn_pid} = :gun.open(~c"localhost", port)

        body = %{"name" => "name"}

        stream_ref =
          :gun.post(
            conn_pid,
            "/v1/messages/nested",
            [
              {"content-type", "application/json"}
            ],
            Jason.encode!(body)
          )

        assert_receive {:gun_response, ^conn_pid, ^stream_ref, :nofin, 200, _headers}
        assert {:ok, body} = :gun.await_body(conn_pid, stream_ref)

        assert %{"name" => "name", "text" => "create_message_with_nested_body"} =
                 Jason.decode!(body)
      end)
    end

    test "can map response body using HttpRule.response_body" do
      run_server([TranscodeServer], fn port ->
        {:ok, conn_pid} = :gun.open(~c"localhost", port)
        name = "response_body_mapper"

        stream_ref =
          :gun.get(
            conn_pid,
            "/v1/messages/response_body/#{name}",
            [
              {"content-type", "application/json"}
            ]
          )

        assert_receive {:gun_up, ^conn_pid, :http}
        assert {:ok, body} = :gun.await_body(conn_pid, stream_ref)

        assert %{"name" => ^name, "text" => "get_message_with_response_body"} =
                 Jason.decode!(body)
      end)
    end

    test "can send streaming responses" do
      run_server([TranscodeServer], fn port ->
        {:ok, conn_pid} = :gun.open(~c"localhost", port)

        stream_ref =
          :gun.get(
            conn_pid,
            "/v1/messages/stream/stream_test",
            [
              {"content-type", "application/json"}
            ]
          )

        assert_receive {:gun_response, ^conn_pid, ^stream_ref, :nofin, 200, _headers}
        assert {:ok, body} = :gun.await_body(conn_pid, stream_ref)
        msgs = String.split(body, "\n", trim: true)
        assert length(msgs) == 5
      end)
    end

    test "can use field paths in requests" do
      run_server([TranscodeServer], fn port ->
        {:ok, conn_pid} = :gun.open(~c"localhost", port)
        name = "fieldpath"

        stream_ref =
          :gun.get(
            conn_pid,
            "/v1/messages/fieldpath/#{name}",
            [
              {"content-type", "application/json"}
            ]
          )

        assert_receive {:gun_response, ^conn_pid, ^stream_ref, :nofin, 200, _headers}
        assert {:ok, body} = :gun.await_body(conn_pid, stream_ref)
        assert %{"name" => ^name} = Jason.decode!(body)
      end)
    end

    test "service methods can have the same path but different methods in http rule option" do
      run_server([TranscodeServer], fn port ->
        {:ok, conn_pid} = :gun.open(~c"localhost", port)

        payload = %{"name" => "foo", "text" => "bar"}

        stream_ref =
          :gun.post(
            conn_pid,
            "/v1/messages",
            [
              {"content-type", "application/json"}
            ],
            Jason.encode!(payload)
          )

        assert_receive {:gun_up, ^conn_pid, :http}
        assert_receive {:gun_response, ^conn_pid, ^stream_ref, :nofin, 200, _headers}
        assert {:ok, body} = :gun.await_body(conn_pid, stream_ref)

        assert ^payload = Jason.decode!(body)

        stream_ref =
          :gun.get(conn_pid, "/v1/messages?name=another_name", [
            {"content-type", "application/json"}
          ])

        assert_receive {:gun_response, ^conn_pid, ^stream_ref, :nofin, 200, _headers}

        assert {:ok, body} = :gun.await_body(conn_pid, stream_ref)

        assert %{
                 "name" => "another_name",
                 "text" => "get_message_with_query"
               } = Jason.decode!(body)
      end)
    end
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

        req = %Helloworld.HelloRequest{name: "delay", duration: 1000}

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
      assert %{monotonic_time: _, system_time: _} = measurements

      assert %{
               stream: %GRPC.Client.Stream{
                 rpc:
                   {"say_hello", {Helloworld.HelloRequest, false}, {Helloworld.HelloReply, false},
                    %{}}
               }
             } = metadata

      assert_received {^stop_client_name, measurements, metadata}
      assert %{duration: duration} = measurements
      assert duration > 1100

      assert %{
               stream: %GRPC.Client.Stream{
                 rpc:
                   {"say_hello", {Helloworld.HelloRequest, false}, {Helloworld.HelloReply, false},
                    %{}}
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

        req = %Helloworld.HelloRequest{name: "raise", duration: 1100}

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
      assert %{monotonic_time: _, system_time: _} = measurements

      assert %{
               stream: %GRPC.Client.Stream{
                 rpc:
                   {"say_hello", {Helloworld.HelloRequest, false}, {Helloworld.HelloReply, false},
                    %{}}
               }
             } = metadata

      assert_received {^stop_client_name, measurements, metadata}
      assert %{duration: duration} = measurements
      assert duration > 1100

      assert %{
               stream: %GRPC.Client.Stream{
                 rpc:
                   {"say_hello", {Helloworld.HelloRequest, false}, {Helloworld.HelloReply, false},
                    %{}}
               }
             } = metadata

      assert_received {:gun_down, _, _, _, _}

      refute_receive _
    end
  end
end
