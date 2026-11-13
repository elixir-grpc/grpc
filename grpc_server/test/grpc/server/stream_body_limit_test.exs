defmodule GRPC.Server.StreamBodyLimitTest do
  # GHSA-736x-r87j-jhcm: `:max_body_size` did not apply to request-streaming and
  # bidirectional-streaming calls, and no limit stopped a length prefix that declares a
  # huge message. The streaming cap shipped in v1.0.6 (GHSA-vx6q-cr5q-945g) and the
  # prefix check is #633. These tests send the report's four cases to a server with an
  # explicit 1 MiB cap and check how much of each the server reads.
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  @cap 1_048_576
  @piece 65_536
  # Far more than the server should read. The client is held back by flow control, so
  # it sends only what the server's windows allow.
  @budget 512 * @piece
  # The cap, plus what flow control lets arrive before the server checks a chunk.
  @read_limit 3 * @cap

  defmodule RouteServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def record_route(req_enum, _stream) do
      %Routeguide.RouteSummary{point_count: Enum.count(req_enum)}
    end

    def route_chat(req_enum, stream) do
      Enum.each(req_enum, &GRPC.Server.send_reply(stream, &1))
    end
  end

  @paths [
    {"client-streaming", "/routeguide.RouteGuide/RecordRoute"},
    {"bidirectional", "/routeguide.RouteGuide/RouteChat"}
  ]

  for {kind, path} <- @paths do
    describe "a #{kind} call" do
      test "stops a prefix that declares 0xFFFFFFFF bytes" do
        assert_refused(unquote(path), <<0, 0xFFFFFFFF::32>>)
      end

      test "stops a prefix that declares 1 GB, under the message limit" do
        assert_refused(unquote(path), <<0, 1_000_000_000::32>>)
      end

      test "stops well-formed messages that add up to more than the cap" do
        # An unknown field is skipped by the decoder, so this is a valid message of
        # either type, 1,000 bytes of padding long.
        padding = String.duplicate("x", 1_000)
        length = IO.iodata_to_binary(Protobuf.Wire.Varint.encode(byte_size(padding)))
        message = <<122>> <> length <> padding
        frame = <<0, byte_size(message)::32, message::binary>>
        assert_refused(unquote(path), <<>>, frame)
      end
    end
  end

  defp assert_refused(path, head, filler \\ nil) do
    capture_log(fn ->
      run_server(fn port ->
        conn = open_h2(port)
        ref = :gun.post(conn, path, request_headers())
        send_budget(conn, ref, head, filler || :binary.copy(<<"x">>, @piece))

        assert {"8", message} = collect_status(conn, ref)
        assert message != ""

        assert received_bytes(port) < @read_limit,
               "the server read #{received_bytes(port)} bytes of #{@budget}"

        :gun.close(conn)
      end)
    end)
  end

  defp send_budget(conn, ref, head, filler) do
    if head != <<>>, do: :gun.data(conn, ref, :nofin, head)
    pieces = div(@budget, byte_size(filler))
    for _ <- 1..(pieces - 1), do: :gun.data(conn, ref, :nofin, filler)
    :gun.data(conn, ref, :fin, filler)
  end

  # Bytes the server's sockets have read, summed over connections on `port`.
  defp received_bytes(port) do
    for socket <- :erlang.ports(),
        {:ok, {_addr, ^port}} <- [:inet.sockname(socket)],
        {:ok, [recv_oct: bytes]} <- [:inet.getstat(socket, [:recv_oct])],
        reduce: 0 do
      total -> total + bytes
    end
  end

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

  defp run_server(func) do
    {:ok, _pid, port} =
      start_supervised(%{
        id: {GRPC.Server, System.unique_integer([:positive])},
        start: {GRPC.Server, :start, [RouteServer, 0, [max_body_size: @cap]]},
        type: :worker,
        restart: :permanent,
        shutdown: 500
      })

    try do
      func.(port)
    after
      GRPC.Server.stop(RouteServer)
    end
  end
end
