defmodule GRPC.Server.BodyLimitFlowWindowTest do
  # GHSA-7x9q-vvph-rjcr: when a call is refused for `:max_body_size`, the client may
  # still have DATA in flight for it. The server dropped that DATA and never gave its
  # share of the connection window back, so after enough refusals every later call on
  # the connection waited for window that never came.
  #
  # A raw HTTP/2 client is needed: it can send DATA after the server has answered,
  # which gun does not do, and it tracks the connection window as a real client must.
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  @max_body_size 1_000
  @frame_size 16_384
  # More refusals than the default connection window (16 stream windows) can absorb
  # if each one leaks a full stream window.
  @refusals 40
  @concurrent 8
  @rounds 12

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream), do: %Helloworld.HelloReply{message: "Hello, #{req.name}"}
  end

  test "small calls still work after many refused calls had DATA in flight" do
    capture_log(fn ->
      run_server(HelloServer, fn port ->
        conn = connect(port)

        conn =
          Enum.reduce(1..@refusals, conn, fn n, conn ->
            refuse_with_data_in_flight(conn, [stream_id(n)])
          end)

        small_calls(conn, @refusals + 1)
        :gen_tcp.close(conn.socket)
      end)
    end)
  end

  test "small calls still work after refused calls ran at the same time" do
    capture_log(fn ->
      run_server(HelloServer, fn port ->
        conn = connect(port)

        # Each round refuses @concurrent calls that are open together, so their
        # in-flight DATA arrives interleaved.
        conn =
          Enum.reduce(0..(@rounds - 1), conn, fn round, conn ->
            ids = for n <- 1..@concurrent, do: stream_id(round * @concurrent + n)
            refuse_with_data_in_flight(conn, ids)
          end)

        small_calls(conn, @rounds * @concurrent + 1)
        :gen_tcp.close(conn.socket)
      end)
    end)
  end

  test "a call opened before the window emptied can still send its body" do
    capture_log(fn ->
      run_server(HelloServer, fn port ->
        conn = connect(port)

        # Open first and send nothing, as a client does while it builds the request.
        waiting = stream_id(1)
        conn = send_headers(conn, waiting, :nofin)

        conn =
          Enum.reduce(2..(@refusals + 1), conn, fn n, conn ->
            refuse_with_data_in_flight(conn, [stream_id(n)])
          end)

        conn = send_small_body(conn, waiting)
        assert grpc_status(conn, waiting) == "0"
        :gen_tcp.close(conn.socket)
      end)
    end)
  end

  defp stream_id(n), do: 2 * n - 1

  defp small_calls(conn, first) do
    Enum.reduce(first..(first + 2), conn, fn n, conn ->
      conn = send_headers(conn, stream_id(n), :nofin)
      conn = send_small_body(conn, stream_id(n))
      assert grpc_status(conn, stream_id(n)) == "0"
      conn
    end)
  end

  # Sends a body over the limit on every stream in `ids`, waits for the refusals,
  # then sends DATA that was already on the wire, up to what the connection and
  # stream windows allow, taking turns between the streams.
  defp refuse_with_data_in_flight(conn, ids) do
    body = <<0, 0, 0, 0, 0>> <> :binary.copy(<<0>>, 2 * @max_body_size)
    needed = byte_size(body) * length(ids)

    # HEADERS need no window. A client with no window left waits for the server to
    # grant more, and gives up after a second.
    conn = Enum.reduce(ids, conn, &send_headers(&2, &1, :nofin)) |> await_window(needed)

    assert conn.window >= needed,
           "the connection window ran out after #{div(hd(ids), 2)} calls " <>
             "(#{conn.window} bytes left); the client cannot send"

    conn = Enum.reduce(ids, conn, &send_data(&2, &1, :nofin, body))

    {_frames, conn} =
      recv_until(conn, 3_000, fn frames -> Enum.all?(ids, &stream_closed?(frames, &1)) end)

    for id <- ids, do: assert(grpc_status(conn, id) in ["8", nil])

    in_flight = :binary.copy(<<0>>, @frame_size)
    windows = Map.new(ids, &{&1, 65_535 - byte_size(body)})

    {conn, _windows} =
      for _ <- 1..3, id <- ids, reduce: {conn, windows} do
        {conn, windows} ->
          size = min(@frame_size, min(conn.window, windows[id]))

          if size > 0 do
            {send_data(conn, id, :nofin, binary_part(in_flight, 0, size)),
             Map.update!(windows, id, &(&1 - size))}
          else
            {conn, windows}
          end
      end

    # Let the server's window updates arrive before the next call.
    {_frames, conn} = recv_until(conn, 50, fn _ -> false end)
    conn
  end

  # Sends a small, valid request on an open stream and waits for its reply.
  defp send_small_body(conn, stream_id) do
    proto = Protobuf.encode(%Helloworld.HelloRequest{name: "hi"})
    body = <<0::8, byte_size(proto)::32, proto::binary>>

    conn = await_window(conn, byte_size(body))

    assert conn.window >= byte_size(body),
           "the connection window is #{conn.window} bytes; a small call cannot send its body"

    conn = send_data(conn, stream_id, :fin, body)
    {_frames, conn} = recv_until(conn, 3_000, &stream_closed?(&1, stream_id))
    conn
  end

  # -- raw HTTP/2 client ------------------------------------------------------

  # Waits up to a second for the connection window to reach `needed` bytes.
  defp await_window(conn, needed) do
    if conn.window >= needed do
      conn
    else
      case :gen_tcp.recv(conn.socket, 0, 1_000) do
        {:ok, data} ->
          {_frames, conn} = recv_until(%{conn | buf: conn.buf <> data}, 0, fn _ -> false end)
          await_window(conn, needed)

        {:error, :timeout} ->
          conn

        {:error, reason} ->
          flunk("connection ended: #{inspect(reason)}")
      end
    end
  end

  defp connect(port) do
    {:ok, socket} = :gen_tcp.connect(~c"localhost", port, [:binary, active: false])

    :ok =
      :gen_tcp.send(socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", :cow_http2.settings(%{})])

    conn = %{
      socket: socket,
      buf: <<>>,
      window: 65_535,
      hpack: :cow_hpack.init(),
      decoder: :cow_hpack.init(),
      headers: %{}
    }

    {_frames, conn} = recv_until(conn, 200, fn _ -> false end)
    conn
  end

  defp send_headers(conn, stream_id, fin) do
    headers = [
      {":method", "POST"},
      {":scheme", "http"},
      {":authority", "localhost"},
      {":path", "/helloworld.Greeter/SayHello"},
      {"content-type", "application/grpc+proto"},
      {"te", "trailers"}
    ]

    {block, hpack} = :cow_hpack.encode(headers, conn.hpack)
    :ok = :gen_tcp.send(conn.socket, :cow_http2.headers(stream_id, fin, block))
    %{conn | hpack: hpack}
  end

  defp send_data(conn, stream_id, fin, data) do
    :ok = :gen_tcp.send(conn.socket, :cow_http2.data(stream_id, fin, data))
    %{conn | window: conn.window - byte_size(data)}
  end

  # Reads frames until `done?.(frames)` or `timeout` ms pass with nothing new.
  # Window updates for the connection add to `conn.window`.
  defp recv_until(conn, timeout, done?, acc \\ []) do
    {frames, buf} = parse_frames(conn.buf, [])
    conn = %{conn | buf: buf}
    conn = Enum.reduce(frames, conn, &handle_frame/2)
    acc = acc ++ frames

    cond do
      done?.(acc) ->
        {acc, conn}

      true ->
        case :gen_tcp.recv(conn.socket, 0, timeout) do
          {:ok, data} -> recv_until(%{conn | buf: conn.buf <> data}, timeout, done?, acc)
          {:error, :timeout} -> {acc, conn}
          {:error, reason} -> flunk("connection ended: #{inspect(reason)}")
        end
    end
  end

  defp handle_frame({:window_update, increment}, conn),
    do: %{conn | window: conn.window + increment}

  defp handle_frame({:settings, _}, conn) do
    :ok = :gen_tcp.send(conn.socket, :cow_http2.settings_ack())
    conn
  end

  # HPACK state is shared by every header block, so each one is decoded in order.
  defp handle_frame({:headers, stream_id, _fin, _head_fin, block}, conn) do
    {headers, decoder} = :cow_hpack.decode(block, conn.decoder)

    %{
      conn
      | decoder: decoder,
        headers: Map.update(conn.headers, stream_id, headers, &(&1 ++ headers))
    }
  end

  defp handle_frame({:goaway, _, reason, _}, _conn), do: flunk("server sent GOAWAY: #{reason}")
  defp handle_frame(_frame, conn), do: conn

  defp parse_frames(buf, acc) do
    case :cow_http2.parse(buf) do
      {:ok, frame, rest} -> parse_frames(rest, [frame | acc])
      {:ignore, rest} -> parse_frames(rest, acc)
      :more -> {Enum.reverse(acc), buf}
      other -> flunk("bad frame from server: #{inspect(other)}")
    end
  end

  defp stream_closed?(frames, stream_id) do
    Enum.any?(frames, fn
      {:rst_stream, ^stream_id, _} -> true
      {:headers, ^stream_id, :fin, _, _} -> true
      {:data, ^stream_id, :fin, _} -> true
      _ -> false
    end)
  end

  defp grpc_status(conn, stream_id) do
    case List.keyfind(Map.get(conn.headers, stream_id, []), "grpc-status", 0) do
      {_, status} -> status
      nil -> nil
    end
  end

  defp run_server(server, func) do
    {:ok, _pid, port} =
      start_supervised(%{
        id: {GRPC.Server, System.unique_integer([:positive])},
        start: {GRPC.Server, :start, [server, 0, [max_body_size: @max_body_size]]},
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
