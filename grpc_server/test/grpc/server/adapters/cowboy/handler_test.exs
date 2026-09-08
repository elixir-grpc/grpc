defmodule GRPC.Server.Adapters.Cowboy.HandlerTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog
  import GRPC.DataCase, only: [attach_telemetry: 1]

  # --------------------------------------------------------------------------
  # Minimal server used across all tests
  # --------------------------------------------------------------------------

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream) do
      %Helloworld.HelloReply{message: "Hello, #{req.name}"}
    end
  end

  defmodule SlowServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream) do
      Process.sleep(5_000)
      %Helloworld.HelloReply{message: "Hello, #{req.name}"}
    end
  end

  defmodule TrappingServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream) do
      Process.flag(:trap_exit, true)
      Process.sleep(3_000)
      %Helloworld.HelloReply{message: "Hello, #{req.name}"}
    end
  end

  # --------------------------------------------------------------------------
  # Helpers
  # --------------------------------------------------------------------------

  # Build a gRPC length-prefixed message frame (no compression).
  defp grpc_frame(proto_binary) do
    <<0::8, byte_size(proto_binary)::32, proto_binary::binary>>
  end

  defp grpc_request_headers do
    [
      {"content-type", "application/grpc+proto"},
      {"te", "trailers"}
    ]
  end

  # Open an HTTP/2 cleartext connection to the server and return the conn pid.
  defp open_h2(port) do
    {:ok, conn} = :gun.open(~c"localhost", port, %{protocols: [:http2]})
    {:ok, :http2} = :gun.await_up(conn, 5_000)
    conn
  end

  # Collect all gun frames for *stream_ref* until END_STREAM, then return the
  # final grpc-status value found in either the response headers or trailers.
  defp collect_grpc_status(conn, stream_ref) do
    collect_grpc_status(conn, stream_ref, nil)
  end

  defp collect_grpc_status(conn, stream_ref, last_status) do
    case :gun.await(conn, stream_ref, 5_000) do
      {:response, :fin, _http_status, headers} ->
        find_grpc_status(headers) || last_status

      {:response, :nofin, _http_status, headers} ->
        collect_grpc_status(conn, stream_ref, find_grpc_status(headers))

      {:data, :fin, _data} ->
        last_status

      {:data, :nofin, _data} ->
        collect_grpc_status(conn, stream_ref, last_status)

      {:trailers, trailers} ->
        find_grpc_status(trailers) || last_status

      {:error, reason} ->
        flunk("gun error: #{inspect(reason)}")
    end
  end

  defp find_grpc_status(headers) do
    case List.keyfind(headers, "grpc-status", 0) do
      {"grpc-status", v} -> v
      nil -> nil
    end
  end

  # --------------------------------------------------------------------------
  # Tests: max_body_size enforcement
  # --------------------------------------------------------------------------

  describe "max_body_size" do
    test "rejects a body that exceeds max_body_size with RESOURCE_EXHAUSTED (8)" do
      capture_log(fn ->
        run_server_with_opts([HelloServer], [max_body_size: 64], fn port ->
          # Build a gRPC frame whose total size is well above the 64-byte cap.
          large_name = String.duplicate("x", 200)

          body =
            grpc_frame(Protobuf.encode(%Helloworld.HelloRequest{name: large_name}))

          assert byte_size(body) > 64,
                 "test body (#{byte_size(body)} bytes) must exceed max_body_size: 64"

          conn = open_h2(port)
          ref = :gun.post(conn, "/helloworld.Greeter/SayHello", grpc_request_headers(), body)

          assert collect_grpc_status(conn, ref) == "8"

          :gun.close(conn)
        end)
      end)
    end

    test "allows a body within max_body_size and returns OK (0)" do
      run_server_with_opts([HelloServer], [max_body_size: 4096], fn port ->
        body = grpc_frame(Protobuf.encode(%Helloworld.HelloRequest{name: "hi"}))

        assert byte_size(body) < 4096,
               "test body (#{byte_size(body)} bytes) must fit within max_body_size: 4096"

        conn = open_h2(port)
        ref = :gun.post(conn, "/helloworld.Greeter/SayHello", grpc_request_headers(), body)

        assert collect_grpc_status(conn, ref) == "0"

        :gun.close(conn)
      end)
    end

    test "default max_body_size is 4 MB – normal requests succeed without explicit option" do
      run_server_with_opts([HelloServer], [], fn port ->
        body = grpc_frame(Protobuf.encode(%Helloworld.HelloRequest{name: "default limit"}))

        conn = open_h2(port)
        ref = :gun.post(conn, "/helloworld.Greeter/SayHello", grpc_request_headers(), body)

        assert collect_grpc_status(conn, ref) == "0"

        :gun.close(conn)
      end)
    end
  end

  # --------------------------------------------------------------------------
  # Tests: read timeout – no :infinity when grpc-timeout is absent
  # --------------------------------------------------------------------------

  describe "read timeout" do
    test "omitting grpc-timeout header still completes a normal request" do
      # If timeout_left_opt/1 incorrectly passed :infinity to cowboy for a
      # nil timer, normal unary requests would still succeed – the regression
      # is that a slow-trickle attack could hold the connection indefinitely.
      # This smoke-test verifies the nil-timer path doesn't break normal calls.
      run_server_with_opts([HelloServer], [], fn port ->
        # Deliberately omit the grpc-timeout header.
        headers = grpc_request_headers()
        body = grpc_frame(Protobuf.encode(%Helloworld.HelloRequest{name: "no timeout header"}))

        conn = open_h2(port)
        ref = :gun.post(conn, "/helloworld.Greeter/SayHello", headers, body)

        assert collect_grpc_status(conn, ref) == "0"

        :gun.close(conn)
      end)
    end
  end

  # --------------------------------------------------------------------------
  # Tests: telemetry for RPCs the adapter stops before they return
  # --------------------------------------------------------------------------

  describe "aborted RPCs" do
    test "an expired deadline publishes :abort and no :stop" do
      attach_telemetry([:grpc, :server, :rpc, :abort])
      attach_telemetry([:grpc, :server, :rpc, :stop])
      attach_telemetry([:grpc, :server, :rpc, :exception])

      capture_log(fn ->
        run_server_with_opts([SlowServer], [], fn port ->
          headers = [{"grpc-timeout", "50m"} | grpc_request_headers()]
          body = grpc_frame(Protobuf.encode(%Helloworld.HelloRequest{name: "slow"}))

          conn = open_h2(port)
          ref = :gun.post(conn, "/helloworld.Greeter/SayHello", headers, body)

          assert collect_grpc_status(conn, ref) == "4"

          :gun.close(conn)
        end)
      end)

      assert_receive {:telemetry, [:grpc, :server, :rpc, :abort], measurements, metadata}
      assert metadata.reason == :timeout
      assert metadata.path == "/helloworld.Greeter/SayHello"
      assert metadata.server == SlowServer
      assert metadata.endpoint == nil
      assert is_pid(metadata.pid)
      assert metadata.stream.http_request_headers["grpc-timeout"] == "50m"
      assert metadata.stream.deadline
      assert measurements.duration > 0

      # The RPC process is stopped by a signal it cannot unwind, so the span
      # around the call publishes nothing: without `:abort` the call would be
      # absent from telemetry entirely.
      refute_receive {:telemetry, [:grpc, :server, :rpc, :stop], _, _}, 200
      refute_receive {:telemetry, [:grpc, :server, :rpc, :exception], _, _}, 10
    end

    test "publishes :abort once when both abort paths run for one call" do
      attach_telemetry([:grpc, :server, :rpc, :abort])

      capture_log(fn ->
        run_server_with_opts([TrappingServer], [], fn port ->
          headers = [{"grpc-timeout", "50m"} | grpc_request_headers()]
          body = grpc_frame(Protobuf.encode(%Helloworld.HelloRequest{name: "trap"}))

          conn = open_h2(port)
          ref = :gun.post(conn, "/helloworld.Greeter/SayHello", headers, body)

          assert collect_grpc_status(conn, ref) == "4"

          :gun.close(conn)
        end)
      end)

      # `send_error/4` publishes and signals; cowboy then calls `terminate/3`,
      # by which point this RPC process has not died.
      assert_receive {:telemetry, [:grpc, :server, :rpc, :abort], _, %{reason: :timeout}}
      refute_receive {:telemetry, [:grpc, :server, :rpc, :abort], _, _}, 500
    end

    test "a dropped connection publishes :abort" do
      attach_telemetry([:grpc, :server, :rpc, :abort])

      capture_log(fn ->
        run_server_with_opts([SlowServer], [], fn port ->
          body = grpc_frame(Protobuf.encode(%Helloworld.HelloRequest{name: "slow"}))

          conn = open_h2(port)
          _ref = :gun.post(conn, "/helloworld.Greeter/SayHello", grpc_request_headers(), body)

          # No deadline: the RPC is still running when the client goes away.
          Process.sleep(100)
          :gun.close(conn)

          assert_receive {:telemetry, [:grpc, :server, :rpc, :abort], _, metadata}, 1_000
          assert metadata.path == "/helloworld.Greeter/SayHello"
          refute metadata.reason == :timeout
        end)
      end)
    end

    test "a call that returns publishes :stop and no :abort" do
      attach_telemetry([:grpc, :server, :rpc, :abort])
      attach_telemetry([:grpc, :server, :rpc, :stop])

      run_server_with_opts([HelloServer], [], fn port ->
        body = grpc_frame(Protobuf.encode(%Helloworld.HelloRequest{name: "hi"}))

        conn = open_h2(port)
        ref = :gun.post(conn, "/helloworld.Greeter/SayHello", grpc_request_headers(), body)

        assert collect_grpc_status(conn, ref) == "0"

        :gun.close(conn)
      end)

      assert_receive {:telemetry, [:grpc, :server, :rpc, :stop], _measurements, _metadata}
      refute_receive {:telemetry, [:grpc, :server, :rpc, :abort], _, _}, 200
    end
  end

  # --------------------------------------------------------------------------
  # Private helper: start a server with specific opts and run a test function
  # --------------------------------------------------------------------------

  defp run_server_with_opts(servers, opts, func) do
    {:ok, _pid, port} =
      start_supervised(%{
        id: {GRPC.Server, System.unique_integer([:positive])},
        start: {GRPC.Server, :start, [servers, 0, opts]},
        type: :worker,
        restart: :permanent,
        shutdown: 500
      })

    try do
      func.(port)
    after
      GRPC.Server.stop(servers)
    end
  end
end
