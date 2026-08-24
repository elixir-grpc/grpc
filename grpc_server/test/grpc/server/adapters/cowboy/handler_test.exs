defmodule GRPC.Server.Adapters.Cowboy.HandlerTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  # --------------------------------------------------------------------------
  # Minimal server used across all tests
  # --------------------------------------------------------------------------

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream) do
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

  # Like collect_grpc_status/2, but also decodes the accumulated body as a HelloReply.
  defp collect_grpc_response(conn, stream_ref) do
    collect_grpc_response(conn, stream_ref, nil, <<>>)
  end

  defp collect_grpc_response(conn, stream_ref, last_status, body) do
    case :gun.await(conn, stream_ref, 5_000) do
      {:response, :fin, _http_status, headers} ->
        {find_grpc_status(headers) || last_status, decode_hello_reply(body)}

      {:response, :nofin, _http_status, headers} ->
        collect_grpc_response(conn, stream_ref, find_grpc_status(headers), body)

      {:data, :fin, data} ->
        {last_status, decode_hello_reply(body <> data)}

      {:data, :nofin, data} ->
        collect_grpc_response(conn, stream_ref, last_status, body <> data)

      {:trailers, trailers} ->
        {find_grpc_status(trailers) || last_status, decode_hello_reply(body)}

      {:error, reason} ->
        flunk("gun error: #{inspect(reason)}")
    end
  end

  defp decode_hello_reply(<<_flag::8, length::32, message::bytes-size(length), _rest::binary>>) do
    Protobuf.decode(message, Helloworld.HelloReply)
  end

  defp decode_hello_reply(_incomplete), do: nil

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
  # Tests: request body delivered across multiple HTTP/2 DATA frames
  # --------------------------------------------------------------------------

  describe "streamed request body" do
    # read_full_body/5 recurses once per :more read; every other test in this
    # file sends a body small enough to complete in a single :ok read, so
    # this is the only test that exercises that recursive accumulation path.
    test "reassembles a request body sent as several separate DATA frames, in order" do
      run_server_with_opts([HelloServer], [], fn port ->
        # Distinct segments so a dropped, duplicated, or reordered chunk changes the decoded name.
        chunk_a = String.duplicate("a", 20_000)
        chunk_b = String.duplicate("b", 20_000)
        chunk_c = String.duplicate("c", 20_000)
        name = chunk_a <> chunk_b <> chunk_c

        body = grpc_frame(Protobuf.encode(%Helloworld.HelloRequest{name: name}))
        <<part1::bytes-size(20_005), part2::bytes-size(20_000), part3::binary>> = body

        conn = open_h2(port)

        start_tracing_read_full_body()

        stream_ref =
          :gun.headers(conn, "POST", "/helloworld.Greeter/SayHello", grpc_request_headers())

        # Waiting for read_full_body to recurse before each send proves the
        # chunk just sent already landed in its own read_body call.
        await_read_full_body_call()
        :gun.data(conn, stream_ref, :nofin, part1)
        await_read_full_body_call()
        :gun.data(conn, stream_ref, :nofin, part2)
        await_read_full_body_call()
        :gun.data(conn, stream_ref, :fin, part3)

        assert {"0", reply} = collect_grpc_response(conn, stream_ref)
        assert reply.message == "Hello, #{name}"

        stop_tracing_read_full_body()
        :gun.close(conn)
      end)
    end
  end

  # read_full_body/5 is private; :local makes trace_pattern instrument it anyway.
  defp start_tracing_read_full_body do
    Code.ensure_loaded!(GRPC.Server.Adapters.Cowboy.Handler)
    :erlang.trace_pattern({GRPC.Server.Adapters.Cowboy.Handler, :read_full_body, :_}, true, [:local])
    :erlang.trace(:all, true, [:call])
  end

  defp stop_tracing_read_full_body do
    :erlang.trace(:all, false, [:call])
    :erlang.trace_pattern({GRPC.Server.Adapters.Cowboy.Handler, :read_full_body, :_}, false, [:local])
  end

  defp await_read_full_body_call do
    assert_receive {:trace, _pid, :call, {GRPC.Server.Adapters.Cowboy.Handler, :read_full_body, _args}},
                    2_000
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
