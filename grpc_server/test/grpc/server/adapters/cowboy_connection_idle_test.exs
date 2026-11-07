defmodule GRPC.Server.Adapters.CowboyConnectionIdleTest do
  # A raw HTTP/2 client shows what the server sends on the wire. The ceilings are
  # long on purpose: each test waits for an event, not for a fixed time.
  #
  # The server tests pass `max_connection_idle_test_pid`. The helper then sends
  # `{:idle_check, :open | :idle}` at each wake, and `{:idle_close, helper}` before it
  # closes the connection. It waits for `{:continue, helper}`.
  use GRPC.Integration.TestCase

  alias GRPC.Server.Adapters.Cowboy.ConnectionAge

  @max_stream_id 0x7FFFFFFF
  @ceiling 5_000
  @preface "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

  defmodule BlockingServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    # Tells the test the RPC is open, then waits for the test to release it.
    def get_feature(point, _stream) do
      send(
        :persistent_term.get({GRPC.Server.Adapters.CowboyConnectionIdleTest, :test}),
        {:in_flight, self()}
      )

      receive do
        :release -> %Routeguide.Feature{location: point, name: "done"}
      end
    end
  end

  setup do
    :persistent_term.put({__MODULE__, :test}, self())
    on_exit(fn -> :persistent_term.erase({__MODULE__, :test}) end)
  end

  describe "ConnectionAge.configure/1" do
    test "rejects values that are not :infinity or a positive integer" do
      for value <- [0, -1, 1.5, "1000", nil] do
        assert_raise ArgumentError, ~r/max_connection_idle/, fn ->
          ConnectionAge.configure(%{max_connection_idle: value})
        end
      end
    end

    test "accepts :infinity and a positive integer" do
      opts = %{max_connection_idle: 1_000}
      assert ConnectionAge.configure(opts) == opts
      assert ConnectionAge.configure(%{max_connection_idle: :infinity})
    end
  end

  describe "ConnectionAge.idle_check/3" do
    test "the connection is idle from the time the counters are made" do
      counters = ConnectionAge.new_counters(1_000)

      assert {:idle, 400} = ConnectionAge.idle_check(counters, 1_000, 1_600)
      assert :expired = ConnectionAge.idle_check(counters, 1_000, 2_000)
      assert :expired = ConnectionAge.idle_check(counters, 1_000, 5_000)
    end

    test "an open stream keeps the connection from idle, however long it is open" do
      counters = ConnectionAge.new_counters(0)
      ConnectionAge.stream_opened(counters)

      assert {:open, 1_000} = ConnectionAge.idle_check(counters, 1_000, 500)
      assert {:open, 1_000} = ConnectionAge.idle_check(counters, 1_000, 1_000_000)
    end

    test "the idle time counts from the end of the last stream" do
      counters = ConnectionAge.new_counters(0)
      ConnectionAge.stream_opened(counters)
      ConnectionAge.stream_closed(counters, 5_000)

      assert {:idle, 700} = ConnectionAge.idle_check(counters, 1_000, 5_300)
      assert :expired = ConnectionAge.idle_check(counters, 1_000, 6_000)
    end

    test "the connection is open until all streams end" do
      counters = ConnectionAge.new_counters(0)
      ConnectionAge.stream_opened(counters)
      ConnectionAge.stream_opened(counters)
      ConnectionAge.stream_closed(counters, 100)

      assert {:open, 1_000} = ConnectionAge.idle_check(counters, 1_000, 10_000)

      ConnectionAge.stream_closed(counters, 200)
      assert {:idle, 900} = ConnectionAge.idle_check(counters, 1_000, 300)
    end

    test "a new stream ends the idle time, and the next close starts a new one" do
      counters = ConnectionAge.new_counters(0)
      assert :expired = ConnectionAge.idle_check(counters, 1_000, 1_500)

      ConnectionAge.stream_opened(counters)
      assert {:open, 1_000} = ConnectionAge.idle_check(counters, 1_000, 1_500)

      ConnectionAge.stream_closed(counters, 1_600)
      assert {:idle, 1_000} = ConnectionAge.idle_check(counters, 1_000, 1_600)
    end
  end

  describe "stream counting" do
    test "stream_opened/0 and stream_closed/0 do nothing without an idle time" do
      assert :ok = ConnectionAge.stream_opened()
      assert :ok = ConnectionAge.stream_closed()

      assert :ok = ConnectionAge.stream_init(%{version: :"HTTP/2"}, %{max_connection_age: 60_000})
      assert :ok = ConnectionAge.stream_opened()
      assert :ok = ConnectionAge.stream_closed()
      assert Process.get({ConnectionAge, :counters}) == nil
    end

    test "stream_init/2 does nothing for HTTP/1.1" do
      opts = %{max_connection_idle: 100}
      assert :ok = ConnectionAge.stream_init(%{version: :"HTTP/1.1"}, opts)
      assert Process.get({ConnectionAge, :started}) == nil
      assert Process.get({ConnectionAge, :counters}) == nil
    end
  end

  describe "the helper" do
    test "wakes, reports open and idle, and ends with the connection" do
      conn =
        start_fake_connection(%{max_connection_idle: 20, max_connection_idle_test_pid: self()})

      assert_receive {:trace, ^conn, :spawn, helper, _mfa}, @ceiling
      helper_ref = Process.monitor(helper)

      send(conn, :open)
      assert_receive {:idle_check, :open}, @ceiling
      assert_receive {:idle_check, :open}, @ceiling

      send(conn, :close)
      assert_receive {:idle_check, :idle}, @ceiling
      assert_receive {:idle_close, ^helper}, @ceiling

      # The connection ends while the helper waits. The helper ends too.
      Process.exit(conn, :kill)
      assert_receive {:DOWN, ^helper_ref, :process, ^helper, _}, @ceiling
    end

    test "starts one helper for each connection" do
      conn = start_fake_connection(%{max_connection_idle: 60_000})
      assert_receive {:trace, ^conn, :spawn, helper, _mfa}, @ceiling
      send(conn, :init_again)
      assert_receive :init_again_done, @ceiling
      refute_received {:trace, ^conn, :spawn, _, _}

      helper_ref = Process.monitor(helper)
      Process.exit(conn, :kill)
      assert_receive {:DOWN, ^helper_ref, :process, ^helper, _}, @ceiling
    end
  end

  describe "with :max_connection_idle set" do
    test "closes the connection when no RPC has been open for the idle time" do
      run_idle_server(fn port ->
        conn = h2_connect(port)
        conn = send_get_feature(conn, 1)
        assert_receive {:in_flight, rpc}, @ceiling
        assert_receive {:idle_check, :open}, @ceiling

        send(rpc, :release)
        assert_receive {:idle_check, :idle}, @ceiling
        continue_close()

        {conn, {:goaway, @max_stream_id, :no_error, _}} = next_frame(conn, &goaway?/1)
        {conn, {:goaway, 1, :no_error, _}} = next_frame(conn, &goaway?/1)
        assert :closed = next_frame(conn, fn _ -> false end)
      end)
    end

    test "does not close the connection while an RPC is open" do
      run_idle_server(fn port ->
        conn = h2_connect(port)
        _conn = send_get_feature(conn, 1)
        assert_receive {:in_flight, rpc}, @ceiling
        [conn_pid] = :ranch.procs(inspect(BlockingServer), :connections)

        for _ <- 1..3, do: assert_receive({:idle_check, :open}, @ceiling)

        refute_received {:idle_close, _}
        assert Process.alive?(conn_pid)

        send(rpc, :release)
        assert_receive {:idle_check, :idle}, @ceiling
        continue_close()
      end)
    end

    test "counts a stream that the client resets" do
      run_idle_server(fn port ->
        conn = h2_connect(port)
        conn = send_get_feature(conn, 1)
        assert_receive {:in_flight, rpc}, @ceiling
        rpc_ref = Process.monitor(rpc)

        :ok = :gen_tcp.send(conn.socket, :cow_http2.rst_stream(1, :cancel))
        assert_receive {:DOWN, ^rpc_ref, :process, ^rpc, _}, @ceiling

        assert_receive {:idle_check, :idle}, @ceiling
        continue_close()
        assert :closed = next_frame(conn, fn _ -> false end)
      end)
    end

    test "a stream that arrives before the close is not lost" do
      run_idle_server(fn port ->
        conn = h2_connect(port)
        conn = send_get_feature(conn, 1)
        assert_receive {:in_flight, rpc1}, @ceiling
        send(rpc1, :release)

        # The helper has seen that the connection is idle. It waits to close.
        assert_receive {:idle_close, helper}, @ceiling

        conn = send_get_feature(conn, 3)
        assert_receive {:in_flight, rpc2}, @ceiling
        assert rpc2 != rpc1

        send(helper, {:continue, helper})

        {conn, {:goaway, @max_stream_id, :no_error, _}} = next_frame(conn, &goaway?/1)
        {conn, {:goaway, 3, :no_error, _}} = next_frame(conn, &goaway?/1)

        # The RPC is open when the server closes the connection. It finishes.
        assert Process.alive?(rpc2)
        send(rpc2, :release)

        {conn, {:data, 3, _, body}} = next_frame(conn, &match?({:data, 3, _, _}, &1))
        assert {:ok, %Routeguide.Feature{name: "done"}} = decode_reply(body)

        {conn, {:headers, 3, :fin, _, _}} =
          next_frame(conn, &match?({:headers, 3, :fin, _, _}, &1))

        assert :closed = next_frame(conn, fn _ -> false end)
      end)
    end

    test "a new connection takes new RPCs after the first one closes" do
      run_idle_server(fn port ->
        first = h2_connect(port)
        first = send_get_feature(first, 1)
        assert_receive {:in_flight, rpc}, @ceiling
        send(rpc, :release)
        continue_close()
        assert :closed = next_frame(first, fn _ -> false end)

        second = h2_connect(port)
        second = send_get_feature(second, 1)
        assert_receive {:in_flight, rpc2}, @ceiling
        assert rpc2 != rpc
        send(rpc2, :release)
        continue_close()
        assert :closed = next_frame(second, fn _ -> false end)
      end)
    end
  end

  describe "with :max_connection_age and :max_connection_idle set" do
    test "closes on the idle time when it ends first" do
      run_idle_server([max_connection_age: 600_000], fn port ->
        conn = h2_connect(port)
        conn = send_get_feature(conn, 1)
        assert_receive {:in_flight, rpc}, @ceiling
        send(rpc, :release)

        assert_receive {:idle_check, :idle}, @ceiling
        continue_close()
        assert :closed = next_frame(conn, fn _ -> false end)
      end)
    end

    test "closes on the age when it ends first, with an RPC open" do
      run_idle_server([max_connection_age: 100, max_connection_idle: 600_000], fn port ->
        conn = h2_connect(port)
        conn = send_get_feature(conn, 1)
        assert_receive {:in_flight, rpc}, @ceiling

        {conn, {:goaway, @max_stream_id, :no_error, _}} = next_frame(conn, &goaway?/1)
        {conn, {:goaway, 1, :no_error, _}} = next_frame(conn, &goaway?/1)
        send(rpc, :release)

        assert :closed = next_frame(conn, fn _ -> false end)
        refute_received {:idle_close, _}
      end)
    end
  end

  # The idle time and the initial GOAWAY timeout are short to keep the tests fast.
  defp run_idle_server(adapter_opts \\ [], fun) do
    adapter_opts =
      Keyword.merge(
        [
          max_connection_idle: 100,
          goaway_initial_timeout: 50,
          max_connection_idle_test_pid: self()
        ],
        adapter_opts
      )

    run_server(BlockingServer, fun, 0, adapter_opts: adapter_opts)
  end

  defp continue_close do
    assert_receive {:idle_close, helper}, @ceiling
    send(helper, {:continue, helper})
  end

  # A process that acts as the Cowboy connection process. It runs the stream callbacks on
  # request. The test gets a trace message when it spawns the helper.
  defp start_fake_connection(opts) do
    test_pid = self()

    conn =
      spawn(fn ->
        receive do
          :go -> :ok
        end

        req = %{version: :"HTTP/2"}
        :ok = ConnectionAge.stream_init(req, opts)
        fake_connection_loop(req, opts, test_pid)
      end)

    :erlang.trace(conn, true, [:procs])
    send(conn, :go)
    conn
  end

  defp fake_connection_loop(req, opts, test_pid) do
    receive do
      :open ->
        ConnectionAge.stream_opened()

      :close ->
        ConnectionAge.stream_closed()

      :init_again ->
        ConnectionAge.stream_init(req, opts)
        send(test_pid, :init_again_done)
    end

    fake_connection_loop(req, opts, test_pid)
  end

  defp goaway?(frame), do: match?({:goaway, _, _, _}, frame)

  defp h2_connect(port) do
    {:ok, socket} =
      :gen_tcp.connect(~c"127.0.0.1", port, [:binary, active: false, nodelay: true])

    on_exit(fn -> :gen_tcp.close(socket) end)
    :ok = :gen_tcp.send(socket, [@preface, :cow_http2.settings(%{})])
    %{socket: socket, buffer: <<>>, hpack: :cow_hpack.init()}
  end

  defp send_get_feature(%{socket: socket, hpack: hpack} = conn, stream_id) do
    headers = [
      {":method", "POST"},
      {":scheme", "http"},
      {":path", "/routeguide.RouteGuide/GetFeature"},
      {":authority", "localhost"},
      {"content-type", "application/grpc"},
      {"te", "trailers"}
    ]

    {block, hpack} = :cow_hpack.encode(headers, hpack)
    body = Routeguide.Point.encode(%Routeguide.Point{latitude: 1, longitude: 1})
    message = <<0, byte_size(body)::32, body::binary>>

    :ok =
      :gen_tcp.send(socket, [
        :cow_http2.headers(stream_id, :nofin, block),
        :cow_http2.data(stream_id, :fin, message)
      ])

    %{conn | hpack: hpack}
  end

  defp decode_reply(<<0, size::32, message::binary-size(size)>>),
    do: {:ok, Routeguide.Feature.decode(message)}

  # Returns the next frame that `pred` accepts, or :closed when the server closes the socket.
  defp next_frame(%{buffer: buffer, socket: socket} = conn, pred) do
    case :cow_http2.parse(buffer, 16_384) do
      {:ok, frame, rest} ->
        conn = %{conn | buffer: rest}
        if pred.(frame), do: {conn, frame}, else: next_frame(conn, pred)

      {:ignore, rest} ->
        next_frame(%{conn | buffer: rest}, pred)

      :more ->
        case :gen_tcp.recv(socket, 0, @ceiling) do
          {:ok, data} -> next_frame(%{conn | buffer: buffer <> data}, pred)
          {:error, :closed} -> :closed
          {:error, reason} -> flunk("socket error: #{inspect(reason)}")
        end
    end
  end
end
