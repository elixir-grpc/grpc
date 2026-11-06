defmodule GRPC.Server.MessageLimitTest do
  # Sets the :max_message_length application env, which other tests read.
  use ExUnit.Case, async: false

  @limit 1000

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    # The name sets the size of the reply, so a test can ask for any size.
    def say_hello(%{name: "reply:" <> size}, _stream) do
      %Helloworld.HelloReply{message: String.duplicate("r", String.to_integer(size))}
    end

    def say_hello(req, _stream) do
      %Helloworld.HelloReply{message: "Hello, #{req.name}"}
    end
  end

  defmodule RouteServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def record_route(req_enum, _stream) do
      %Routeguide.RouteSummary{point_count: Enum.count(req_enum)}
    end
  end

  setup do
    Application.put_env(:grpc, :max_message_length, @limit)
    on_exit(fn -> Application.delete_env(:grpc, :max_message_length) end)
  end

  describe "sending" do
    test "a reply over the limit fails with RESOURCE_EXHAUSTED and states the limit" do
      run_server(HelloServer, fn port ->
        assert {"8", message} = say_hello(port, "reply:#{@limit + 100}")
        assert message =~ "too large"
        assert message =~ "the limit is #{@limit} bytes"
      end)
    end

    test "a reply under the limit succeeds" do
      run_server(HelloServer, fn port ->
        assert {"0", _} = say_hello(port, "reply:#{@limit - 100}")
      end)
    end

    test "a reply larger than the default flow-control windows succeeds with the default limit" do
      Application.delete_env(:grpc, :max_message_length)
      size = 300_000
      assert size > 65_535

      run_server(HelloServer, fn port ->
        assert {"0", _} = say_hello(port, "reply:#{size}")
      end)
    end
  end

  describe "receiving" do
    test "a unary request over the limit fails with RESOURCE_EXHAUSTED and states the limit" do
      run_server(HelloServer, fn port ->
        assert {"8", message} = say_hello(port, String.duplicate("x", @limit + 100))
        assert message =~ "Received message is too large"
        assert message =~ "the limit is #{@limit} bytes"
      end)
    end

    test "a unary request under the limit succeeds" do
      run_server(HelloServer, fn port ->
        assert {"0", _} = say_hello(port, String.duplicate("x", @limit - 100))
      end)
    end

    test "a message of a request stream over the limit fails with RESOURCE_EXHAUSTED" do
      run_server(RouteServer, fn port ->
        small = grpc_frame(Protobuf.encode(%Routeguide.Point{latitude: 1, longitude: 2}))

        # The limit is checked on the length prefix, before the body is decoded.
        big = grpc_frame(String.duplicate("p", @limit + 100))

        conn = open_h2(port)
        ref = :gun.post(conn, "/routeguide.RouteGuide/RecordRoute", request_headers())
        :gun.data(conn, ref, :nofin, small)
        :gun.data(conn, ref, :fin, big)

        assert {"8", message} = collect_status(conn, ref)
        assert message =~ "the limit is #{@limit} bytes"
        :gun.close(conn)
      end)
    end

    test "a request stream whose messages are each under the limit succeeds" do
      run_server(RouteServer, fn port ->
        frame = grpc_frame(Protobuf.encode(%Routeguide.Point{latitude: 1, longitude: 2}))

        conn = open_h2(port)
        ref = :gun.post(conn, "/routeguide.RouteGuide/RecordRoute", request_headers())
        # Together the frames pass the limit; one message at a time does not.
        for _ <- 1..200, do: :gun.data(conn, ref, :nofin, frame)
        :gun.data(conn, ref, :fin, <<>>)

        assert {"0", _} = collect_status(conn, ref)
        :gun.close(conn)
      end)
    end
  end

  defp say_hello(port, name) do
    body = grpc_frame(Protobuf.encode(%Helloworld.HelloRequest{name: name}))
    conn = open_h2(port)
    ref = :gun.post(conn, "/helloworld.Greeter/SayHello", request_headers(), body)
    result = collect_status(conn, ref)
    :gun.close(conn)
    result
  end

  defp grpc_frame(proto), do: <<0::8, byte_size(proto)::32, proto::binary>>

  defp request_headers, do: [{"content-type", "application/grpc+proto"}, {"te", "trailers"}]

  defp open_h2(port) do
    {:ok, conn} = :gun.open(~c"localhost", port, %{protocols: [:http2]})
    {:ok, :http2} = :gun.await_up(conn, 5_000)
    conn
  end

  # Reads the response to its end and returns {grpc-status, grpc-message}.
  defp collect_status(conn, ref), do: collect_status(conn, ref, {nil, nil})

  defp collect_status(conn, ref, acc) do
    case :gun.await(conn, ref, 10_000) do
      {:response, :fin, _status, headers} ->
        merge_status(headers, acc)

      {:response, :nofin, _status, headers} ->
        collect_status(conn, ref, merge_status(headers, acc))

      {:data, :fin, _data} ->
        acc

      {:data, :nofin, _data} ->
        collect_status(conn, ref, acc)

      {:trailers, trailers} ->
        merge_status(trailers, acc)

      {:error, reason} ->
        flunk("gun error: #{inspect(reason)}")
    end
  end

  defp merge_status(headers, {status, message}) do
    {
      with_default(List.keyfind(headers, "grpc-status", 0), status),
      with_default(List.keyfind(headers, "grpc-message", 0), message)
    }
  end

  defp with_default({_, value}, _default), do: URI.decode(value)
  defp with_default(nil, default), do: default

  defp run_server(server, func) do
    {:ok, _pid, port} =
      start_supervised(%{
        id: {GRPC.Server, System.unique_integer([:positive])},
        start: {GRPC.Server, :start, [server, 0, []]},
        type: :worker,
        restart: :permanent,
        shutdown: 500
      })

    try do
      func.(port)
    after
      GRPC.Server.stop(server)
    end
  end
end
