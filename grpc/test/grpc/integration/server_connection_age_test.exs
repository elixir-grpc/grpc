defmodule GRPC.Integration.ServerConnectionAgeTest do
  # Runs the server's `:max_connection_age` against the Gun and Mint client adapters.
  # The server tests in grpc_server read the frames on the wire.
  use GRPC.Integration.TestCase

  require Record

  @cowboy Path.join(Mix.Project.deps_paths()[:cowboy], "src/cowboy_http2.erl")
  Record.defrecordp(:conn, :state, Record.extract(:state, from: @cowboy))

  @ceiling 5_000

  defmodule AgedServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    # latitude 1: tells the test the RPC is open, then waits until the test releases it.
    # Other latitudes: replies at once, and tells the test which connections the server has.
    def get_feature(%{latitude: 1} = point, _stream) do
      send(test_pid(), {:in_flight, self()})

      receive do
        :release -> %Routeguide.Feature{location: point, name: "released"}
      end
    end

    def get_feature(point, _stream) do
      send(test_pid(), {:connections, :ranch.procs(inspect(__MODULE__), :connections)})
      %Routeguide.Feature{location: point, name: "quick"}
    end

    defp test_pid,
      do: :persistent_term.get({GRPC.Integration.ServerConnectionAgeTest, :test})
  end

  setup do
    :persistent_term.put({__MODULE__, :test}, self())
    on_exit(fn -> :persistent_term.erase({__MODULE__, :test}) end)
  end

  # A long grace keeps the server from closing the connection while the RPC is open.
  @server_opts [
    adapter_opts: [
      max_connection_age: 100,
      max_connection_age_grace: 10_000,
      goaway_initial_timeout: 50
    ]
  ]

  for {name, adapter} <- [gun: GRPC.Client.Adapters.Gun, mint: GRPC.Client.Adapters.Mint] do
    describe "#{name} client" do
      test "an RPC open at GOAWAY finishes, and the next RPC uses a new connection" do
        run_server(
          AgedServer,
          fn port ->
            {:ok, channel} = GRPC.Stub.connect("127.0.0.1:#{port}", adapter: unquote(adapter))

            task =
              Task.async(fn ->
                Routeguide.RouteGuide.Stub.get_feature(channel, point(1), timeout: 10_000)
              end)

            assert_receive {:in_flight, rpc}, @ceiling
            [old_conn] = :ranch.procs(inspect(AgedServer), :connections)
            ref = Process.monitor(old_conn)

            # `:closing` is after the second GOAWAY. The connection stays open for the RPC.
            await_closing(old_conn)
            refute_received {:DOWN, ^ref, :process, ^old_conn, _}
            assert Process.alive?(rpc)

            send(rpc, :release)
            assert {:ok, %Routeguide.Feature{name: "released"}} = Task.await(task, @ceiling)
            assert_receive {:DOWN, ^ref, :process, ^old_conn, _}, @ceiling

            assert {:ok, %Routeguide.Feature{name: "quick"}} =
                     Routeguide.RouteGuide.Stub.get_feature(channel, point(2), timeout: @ceiling)

            assert_receive {:connections, [new_conn]}, @ceiling
            assert new_conn != old_conn

            GRPC.Stub.disconnect(channel)
          end,
          0,
          @server_opts
        )
      end
    end
  end

  defp point(latitude), do: %Routeguide.Point{latitude: latitude, longitude: 0}

  defp await_closing(conn_pid, deadline \\ System.monotonic_time(:millisecond) + @ceiling) do
    {state, _buffer} = :sys.get_state(conn_pid)

    cond do
      conn(state, :http2_status) == :closing ->
        :ok

      System.monotonic_time(:millisecond) > deadline ->
        flunk("the connection never reached :closing")

      true ->
        Process.sleep(10)
        await_closing(conn_pid, deadline)
    end
  end
end
