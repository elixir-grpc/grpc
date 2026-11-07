defmodule GRPC.Integration.MintGoawayDrainTest do
  # The Mint client after a server GOAWAY (`:max_connection_age` on the server):
  # with the pool, new calls go to a new connection while open calls finish on the old
  # one. Without the pool, new calls fail until the drain ends.
  # async: false, because it toggles `:pool_enabled` and uses global telemetry handlers.
  use GRPC.Integration.TestCase, async: false

  @ceiling 5_000
  # Long enough that a new connection does not age out while a test runs.
  @age 1_500
  @adapter GRPC.Client.Adapters.Mint

  @server_opts [
    adapter_opts: [
      max_connection_age: @age,
      max_connection_age_grace: 10_000,
      goaway_initial_timeout: 50
    ]
  ]

  defmodule DrainServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    # latitude 1: tells the test the RPC is open, then waits until the test releases it.
    def get_feature(%{latitude: 1} = point, _stream) do
      send(test_pid(), {:in_flight, self()})

      receive do
        :release -> %Routeguide.Feature{location: point, name: "released"}
      end
    end

    def get_feature(point, _stream), do: %Routeguide.Feature{location: point, name: "quick"}

    # Waits for the end of the request stream, which a client cannot send during a drain.
    def record_route(req_enum, _stream) do
      %Routeguide.RouteSummary{point_count: Enum.count(req_enum)}
    end

    defp test_pid, do: :persistent_term.get({GRPC.Integration.MintGoawayDrainTest, :test})
  end

  setup do
    :persistent_term.put({__MODULE__, :test}, self())
    previous_pool = Application.get_env(:grpc, :pool_enabled)
    test_pid = self()
    handler_id = {__MODULE__, make_ref()}

    :telemetry.attach_many(
      handler_id,
      for(event <- [:connected, :streams, :stopped], do: [:grpc, :client, :transport, event]),
      fn event, measurements, metadata, _config ->
        send(test_pid, {:telemetry, event, measurements, metadata})
      end,
      nil
    )

    on_exit(fn ->
      :telemetry.detach(handler_id)
      Application.put_env(:grpc, :pool_enabled, previous_pool)
      :persistent_term.erase({__MODULE__, :test})
    end)
  end

  describe "with the pool" do
    setup do
      Application.put_env(:grpc, :pool_enabled, true)
    end

    for retry <- [0, 3] do
      test "a call that starts in the drain uses a new connection, and the old one leaves (retry: #{retry})" do
        run_server(
          DrainServer,
          fn port ->
            ref = make_ref()

            {:ok, channel} =
              GRPC.Stub.connect("127.0.0.1:#{port}",
                name: ref,
                adapter: @adapter,
                adapter_opts: [retry: unquote(retry)]
              )

            old = assert_connected(ref)

            task =
              Task.async(fn ->
                Routeguide.RouteGuide.Stub.get_feature(channel, point(1), timeout: 10_000)
              end)

            assert_receive {:in_flight, rpc}, @ceiling

            # The pool opens the replacement when the old connection reports the drain.
            new = assert_connected(ref)
            assert new != old
            assert [%{draining: true}, %{draining: false}] = sorted_channels(channel, old)

            assert {:ok, %Routeguide.Feature{name: "quick"}} =
                     Routeguide.RouteGuide.Stub.get_feature(channel, point(2), timeout: @ceiling)

            assert_receive {:telemetry, [:grpc, :client, :transport, :streams], %{active: 1},
                            %{logical_connection_ref: ^ref, transport_ref: ^new}}

            assert Process.alive?(rpc)
            send(rpc, :release)
            assert {:ok, %Routeguide.Feature{name: "released"}} = Task.await(task, @ceiling)

            assert_receive {:telemetry, [:grpc, :client, :transport, :stopped], _,
                            %{logical_connection_ref: ^ref, transport_ref: ^old}},
                           @ceiling

            assert [%{draining: false, channel: %{adapter_payload: %{conn_pid: ^new}}}] =
                     channels(channel)

            # The old connection does not reconnect, and the pool opens no third one.
            refute_receive {:telemetry, [:grpc, :client, :transport, :connected], _,
                            %{logical_connection_ref: ^ref}},
                           300

            GRPC.Stub.disconnect(channel)
          end,
          0,
          @server_opts
        )
      end
    end

    test "a client-streaming call cannot send in the drain, and fails with UNAVAILABLE" do
      run_server(
        DrainServer,
        fn port ->
          ref = make_ref()

          {:ok, channel} =
            GRPC.Stub.connect("127.0.0.1:#{port}", name: ref, adapter: @adapter)

          old = assert_connected(ref)

          stream = Routeguide.RouteGuide.Stub.record_route(channel, timeout: 10_000)
          stream = GRPC.Stub.send_request(stream, point(1))
          old_server_conn = await_server_connection()
          server_ref = Process.monitor(old_server_conn)

          assert assert_connected(ref) != old

          stream = GRPC.Stub.send_request(stream, point(2))
          stream = GRPC.Stub.end_stream(stream)

          assert {:error, %GRPC.RPCError{status: 14}} = GRPC.Stub.recv(stream)

          # Without the cancel, the call stays open until `:max_connection_age_grace`.
          assert_receive {:telemetry, [:grpc, :client, :transport, :stopped], _,
                          %{logical_connection_ref: ^ref, transport_ref: ^old}},
                         @ceiling

          assert_receive {:DOWN, ^server_ref, :process, ^old_server_conn, _}, @ceiling

          assert {:ok, %Routeguide.Feature{name: "quick"}} =
                   Routeguide.RouteGuide.Stub.get_feature(channel, point(2), timeout: @ceiling)

          GRPC.Stub.disconnect(channel)
        end,
        0,
        @server_opts
      )
    end
  end

  describe "without the pool" do
    setup do
      Application.put_env(:grpc, :pool_enabled, false)
    end

    for retry <- [0, 3] do
      test "a call in the drain fails, and the client connects again after it (retry: #{retry})" do
        run_server(
          DrainServer,
          fn port ->
            ref = make_ref()

            {:ok, channel} =
              GRPC.Stub.connect("127.0.0.1:#{port}",
                name: ref,
                adapter: @adapter,
                adapter_opts: [retry: unquote(retry)]
              )

            old = assert_connected(ref)

            task =
              Task.async(fn ->
                Routeguide.RouteGuide.Stub.get_feature(channel, point(1), timeout: 10_000)
              end)

            assert_receive {:in_flight, rpc}, @ceiling
            await_goaway(old)

            assert {:error, %GRPC.RPCError{status: 14}} =
                     Routeguide.RouteGuide.Stub.get_feature(channel, point(2), timeout: @ceiling)

            send(rpc, :release)
            assert {:ok, %Routeguide.Feature{name: "released"}} = Task.await(task, @ceiling)

            assert_connected(ref)

            # The channel list is not ready yet when the transport reports `:connected`.
            assert {:ok, %Routeguide.Feature{name: "quick"}} = get_feature_when_ready(channel)

            GRPC.Stub.disconnect(channel)
          end,
          0,
          @server_opts
        )
      end
    end
  end

  defp point(latitude), do: %Routeguide.Point{latitude: latitude, longitude: 0}

  defp assert_connected(ref) do
    assert_receive {:telemetry, [:grpc, :client, :transport, :connected], _,
                    %{logical_connection_ref: ^ref, transport_ref: transport_ref}},
                   @ceiling

    transport_ref
  end

  defp channels(channel) do
    [{pool, _}] =
      Registry.lookup(GRPC.Client.Pool.Registry, {GRPC.Client.Pool.Server, channel.pool})

    pool |> :sys.get_state() |> Map.fetch!(:channels) |> Map.values()
  end

  # The draining channel first.
  defp sorted_channels(channel, old_conn_pid) do
    Enum.sort_by(channels(channel), fn %{channel: %{adapter_payload: %{conn_pid: pid}}} ->
      pid != old_conn_pid
    end)
  end

  defp get_feature_when_ready(channel, attempts \\ 100) do
    case Routeguide.RouteGuide.Stub.get_feature(channel, point(2), timeout: @ceiling) do
      {:error, %GRPC.RPCError{status: 14}} when attempts > 0 ->
        Process.sleep(20)
        get_feature_when_ready(channel, attempts - 1)

      result ->
        result
    end
  end

  defp await_server_connection(deadline \\ System.monotonic_time(:millisecond) + @ceiling) do
    case :ranch.procs(inspect(DrainServer), :connections) do
      [conn] ->
        conn

      _ ->
        if System.monotonic_time(:millisecond) > deadline,
          do: flunk("the server has no connection")

        Process.sleep(10)
        await_server_connection(deadline)
    end
  end

  defp await_goaway(conn_pid, deadline \\ System.monotonic_time(:millisecond) + @ceiling) do
    cond do
      not Mint.HTTP.open?(:sys.get_state(conn_pid).conn) ->
        :ok

      System.monotonic_time(:millisecond) > deadline ->
        flunk("the client never saw GOAWAY")

      true ->
        Process.sleep(10)
        await_goaway(conn_pid, deadline)
    end
  end
end
