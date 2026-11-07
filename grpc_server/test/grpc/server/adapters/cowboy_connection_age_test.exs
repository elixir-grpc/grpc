defmodule GRPC.Server.Adapters.CowboyConnectionAgeTest do
  # A raw HTTP/2 client shows what the server sends on the wire. The ceilings are
  # long on purpose: each test waits for an event, not for a fixed time.
  use GRPC.Integration.TestCase

  import ExUnit.CaptureLog

  alias GRPC.Server.Adapters.Cowboy.ConnectionAge

  @max_stream_id 0x7FFFFFFF
  @ceiling 5_000
  @preface "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

  defmodule BlockingServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    # Tells the test the RPC is open, then waits for the test to release it.
    def get_feature(point, _stream) do
      send(
        :persistent_term.get({GRPC.Server.Adapters.CowboyConnectionAgeTest, :test}),
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
    test "leaves the options as they are when no age is set" do
      opts = %{idle_timeout: :infinity}
      assert ConnectionAge.configure(opts) == opts
    end

    test "maps the grace to Cowboy's goaway_complete_timeout" do
      assert %{goaway_complete_timeout: 250} =
               ConnectionAge.configure(%{
                 max_connection_age: 1_000,
                 max_connection_age_grace: 250
               })
    end

    test "rejects values that are not :infinity or a positive integer" do
      for key <- [:max_connection_age, :max_connection_age_grace],
          value <- [0, -1, 1.5, "1000", nil] do
        assert_raise ArgumentError, ~r/#{key}/, fn ->
          ConnectionAge.configure(%{key => value})
        end
      end
    end
  end

  describe "ConnectionAge.jitter/2" do
    test "spreads the age by ±10%" do
      assert ConnectionAge.jitter(1_000, 0.0) == 900
      assert ConnectionAge.jitter(1_000, 0.5) == 1_000
      assert ConnectionAge.jitter(1_000, 1.0) == 1_100
    end

    test "stays inside the bounds for random input" do
      for _ <- 1..1_000 do
        assert ConnectionAge.jitter(10_000, :rand.uniform()) in 9_000..11_000
      end
    end
  end

  describe "ConnectionAge.stream_init/2" do
    test "starts one timer for each connection, and ends it with the connection" do
      test_pid = self()

      conn =
        spawn(fn ->
          receive do
            :go -> :ok
          end

          req = %{version: :"HTTP/2"}
          opts = %{max_connection_age: 60_000}
          :ok = ConnectionAge.stream_init(req, opts)
          :ok = ConnectionAge.stream_init(req, opts)
          send(test_pid, :initialized)
          Process.sleep(:infinity)
        end)

      :erlang.trace(conn, true, [:procs])
      send(conn, :go)

      # The tracer gets the trace messages of `conn` before the message that follows them.
      assert_receive {:trace, ^conn, :spawn, timer, _mfa}, @ceiling
      assert_receive :initialized, @ceiling
      refute_received {:trace, ^conn, :spawn, _, _}

      timer_ref = Process.monitor(timer)
      Process.exit(conn, :kill)
      assert_receive {:DOWN, ^timer_ref, :process, ^timer, _}, @ceiling
    end

    test "does nothing when no age is set" do
      assert :ok = ConnectionAge.stream_init(%{version: :"HTTP/2"}, %{})
      assert Process.get({ConnectionAge, :started}) == nil
    end

    test "does nothing for HTTP/1.1" do
      assert :ok = ConnectionAge.stream_init(%{version: :"HTTP/1.1"}, %{max_connection_age: 100})
      assert Process.get({ConnectionAge, :started}) == nil
    end
  end

  describe "with :max_connection_age set" do
    test "sends GOAWAY, lets an open RPC finish, then closes the connection" do
      run_aged_server(fn port ->
        conn = h2_connect(port)
        conn = send_get_feature(conn, 1)
        assert_receive {:in_flight, rpc}, @ceiling

        # The first GOAWAY allows new streams to arrive. The second names the last real one.
        {conn, {:goaway, @max_stream_id, :no_error, _}} = next_frame(conn, &goaway?/1)
        {conn, {:goaway, 1, :no_error, _}} = next_frame(conn, &goaway?/1)

        # The RPC is still open when GOAWAY arrives, and it finishes before the grace ends.
        assert Process.alive?(rpc)
        send(rpc, :release)

        {conn, {:data, 1, _, body}} = next_frame(conn, &match?({:data, 1, _, _}, &1))
        assert {:ok, %Routeguide.Feature{name: "done"}} = decode_reply(body)

        {conn, {:headers, 1, :fin, _, _}} =
          next_frame(conn, &match?({:headers, 1, :fin, _, _}, &1))

        assert :closed = next_frame(conn, fn _ -> false end)
      end)
    end

    test "closes the connection when the grace ends, and stops the RPC that is still open" do
      run_aged_server([max_connection_age_grace: 100], fn port ->
        conn = h2_connect(port)
        conn = send_get_feature(conn, 1)
        assert_receive {:in_flight, rpc}, @ceiling
        rpc_ref = Process.monitor(rpc)

        log =
          capture_log(fn ->
            {conn, {:goaway, @max_stream_id, :no_error, _}} = next_frame(conn, &goaway?/1)
            assert :closed = next_frame(conn, fn _ -> false end)
            assert_receive {:DOWN, ^rpc_ref, :process, ^rpc, _reason}, @ceiling
          end)

        refute log =~ "[error]"
      end)
    end

    test "a new connection takes new RPCs after the first one closes" do
      run_aged_server(fn port ->
        first = h2_connect(port)
        first = send_get_feature(first, 1)
        assert_receive {:in_flight, rpc}, @ceiling
        {first, {:goaway, 1, :no_error, _}} = next_frame(first, &goaway?(&1, 1))
        send(rpc, :release)
        assert :closed = next_frame(first, fn _ -> false end)

        second = h2_connect(port)
        second = send_get_feature(second, 1)
        assert_receive {:in_flight, rpc2}, @ceiling
        assert rpc2 != rpc
        {_second, {:goaway, @max_stream_id, :no_error, _}} = next_frame(second, &goaway?/1)
        send(rpc2, :release)
      end)
    end
  end

  # The age and the initial GOAWAY timeout are short to keep the tests fast. The age has
  # a ±10% jitter, so it is at least 100 ms after the first stream.
  defp run_aged_server(adapter_opts \\ [], fun) do
    adapter_opts =
      Keyword.merge([max_connection_age: 100, goaway_initial_timeout: 50], adapter_opts)

    run_server(BlockingServer, fun, 0, adapter_opts: adapter_opts)
  end

  defp goaway?(frame), do: match?({:goaway, _, _, _}, frame)
  defp goaway?({:goaway, id, _, _}, id), do: true
  defp goaway?(_, _), do: false

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
