defmodule GRPC.Server.MessageLimitEarlyStopTest do
  # Sets the :max_message_length application env, which other tests read.
  use ExUnit.Case, async: false

  # The client offers a message of `@claimed` bytes (under the 4 MiB `:max_body_size`),
  # sent in 64 KiB pieces. HTTP/2
  # lets it send about one stream window (64 KiB) before the server grants more, so
  # a server that stops at the prefix receives far less than `@claimed`.
  @limit 1000
  @claimed 3_000_000
  @piece 65_536
  # One stream window, one connection window, and room for headers and frames.
  @received_cap 300_000

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream), do: %Helloworld.HelloReply{message: "Hello, #{req.name}"}
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

  test "a unary request whose prefix is over the limit is refused after about one window" do
    run_server(HelloServer, fn port ->
      conn = open_h2(port)
      ref = :gun.post(conn, "/helloworld.Greeter/SayHello", request_headers())
      send_oversize(conn, ref, <<0, @claimed::32>>, @claimed)

      assert {"8", message} = collect_status(conn, ref)
      assert message =~ "Received message is too large (#{@claimed} bytes"
      assert message =~ "the limit is #{@limit} bytes"
      assert received_bytes(port) < @received_cap
      :gun.close(conn)
    end)
  end

  test "a request stream message whose prefix is over the limit is refused after about one window" do
    run_server(RouteServer, fn port ->
      small = grpc_frame(Protobuf.encode(%Routeguide.Point{latitude: 1, longitude: 2}))

      conn = open_h2(port)
      ref = :gun.post(conn, "/routeguide.RouteGuide/RecordRoute", request_headers())
      :gun.data(conn, ref, :nofin, small)
      send_oversize(conn, ref, <<0, @claimed::32>>, @claimed)

      assert {"8", message} = collect_status(conn, ref)
      assert message =~ "the limit is #{@limit} bytes"
      assert received_bytes(port) < @received_cap
      :gun.close(conn)
    end)
  end

  test "a prefix that arrives in two pieces is checked once it is whole" do
    run_server(RouteServer, fn port ->
      conn = open_h2(port)
      ref = :gun.post(conn, "/routeguide.RouteGuide/RecordRoute", request_headers())
      prefix = <<0, @claimed::32>>
      :gun.data(conn, ref, :nofin, binary_part(prefix, 0, 2))
      :gun.data(conn, ref, :nofin, binary_part(prefix, 2, 3))
      send_oversize(conn, ref, <<>>, @claimed)

      assert {"8", _message} = collect_status(conn, ref)
      assert received_bytes(port) < @received_cap
      :gun.close(conn)
    end)
  end

  describe "messages over the window but under the limit" do
    setup do
      Application.delete_env(:grpc, :max_message_length)
    end

    test "a unary request larger than the flow-control window still arrives" do
      run_server(HelloServer, fn port ->
        name = String.duplicate("x", 3 * @piece)
        body = grpc_frame(Protobuf.encode(%Helloworld.HelloRequest{name: name}))

        conn = open_h2(port)
        ref = :gun.post(conn, "/helloworld.Greeter/SayHello", request_headers(), body)

        assert {"0", _} = collect_status(conn, ref)
        :gun.close(conn)
      end)
    end

    test "request stream messages larger than the window still arrive" do
      run_server(RouteServer, fn port ->
        # A Point with an unknown field, which the decoder skips, as padding.
        padding = String.duplicate("x", 3 * @piece)

        point =
          IO.iodata_to_binary([
            Protobuf.encode(%Routeguide.Point{latitude: 1, longitude: 2}),
            122,
            Protobuf.Wire.Varint.encode(byte_size(padding)),
            padding
          ])

        conn = open_h2(port)
        ref = :gun.post(conn, "/routeguide.RouteGuide/RecordRoute", request_headers())
        for _ <- 1..3, do: :gun.data(conn, ref, :nofin, grpc_frame(point))
        :gun.data(conn, ref, :fin, <<>>)

        assert {"0", _} = collect_status(conn, ref)
        :gun.close(conn)
      end)
    end
  end

  # Sends `head` and then filler up to `total` bytes, the last piece with fin.
  defp send_oversize(conn, ref, head, total) do
    :gun.data(conn, ref, :nofin, head)
    filler = :binary.copy(<<"x">>, @piece)
    pieces = div(total, @piece)

    for _ <- 1..(pieces - 1), do: :gun.data(conn, ref, :nofin, filler)
    :gun.data(conn, ref, :fin, filler)
  end

  # Bytes the server's sockets have read, summed over connections on `port`. The
  # listening socket reads nothing, so it adds zero.
  defp received_bytes(port) do
    for socket <- :erlang.ports(),
        {:ok, {_addr, ^port}} <- [:inet.sockname(socket)],
        {:ok, [recv_oct: bytes]} <- [:inet.getstat(socket, [:recv_oct])],
        reduce: 0 do
      total -> total + bytes
    end
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
