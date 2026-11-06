defmodule GRPC.Client.Adapters.GunDeadChannelTest do
  # async: false — toggles global Application env and attaches global telemetry handlers
  use GRPC.Client.DataCase, async: false

  alias GRPC.Client.Adapters.Gun
  alias GRPC.Client.Stream

  @unavailable GRPC.Status.unavailable()
  @get_feature "/routeguide.RouteGuide/GetFeature"

  defmodule FeatureServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def get_feature(point, _stream), do: %Routeguide.Feature{location: point, name: "ok"}

    def list_features(rectangle, stream) do
      GRPC.Server.send_reply(stream, %Routeguide.Feature{location: rectangle.lo, name: "ok"})
    end
  end

  defmodule FakeResponseProcess do
    @moduledoc false
    use GenServer

    def start_link(reply), do: GenServer.start_link(__MODULE__, reply)

    @impl true
    def init(reply), do: {:ok, reply}

    @impl true
    def handle_call({:await, _deadline}, _from, reply), do: {:reply, reply, reply}
  end

  setup do
    previous = Application.get_env(:grpc, :pool_enabled)
    on_exit(fn -> Application.put_env(:grpc, :pool_enabled, previous) end)
  end

  defp point, do: %Routeguide.Point{latitude: 1, longitude: 1}
  defp rectangle, do: %Routeguide.Rectangle{lo: point(), hi: point()}

  # Runs a unary and a server-streaming call. Raises and exits come back as
  # values so a failing assertion shows what the call did.
  defp rpc_results(channel) do
    unary =
      attempt(fn -> Routeguide.RouteGuide.Stub.get_feature(channel, point(), timeout: 2_000) end)

    server_stream =
      attempt(fn ->
        case Routeguide.RouteGuide.Stub.list_features(channel, rectangle(), timeout: 2_000) do
          {:ok, replies} -> {:ok, Enum.to_list(replies)}
          other -> other
        end
      end)

    [unary: unary, server_stream: server_stream]
  end

  defp attempt(fun) do
    fun.()
  catch
    kind, reason -> {:raised, kind, reason}
  end

  defp assert_unavailable(results) do
    for {kind, result} <- results do
      assert {:error, %GRPC.RPCError{status: @unavailable, message: message}} = result,
             "#{kind} call returned #{inspect(result)}"

      assert message =~ ~r/connection_error|no healthy connection/
      refute message =~ ":down: :normal"
    end
  end

  defp assert_ok_or_unavailable(results) do
    for {kind, result} <- results do
      case result do
        {:ok, _} -> :ok
        {:error, %GRPC.RPCError{status: @unavailable}} -> :ok
        other -> flunk("#{kind} call returned #{inspect(other)}")
      end
    end
  end

  # Waits for the process to be gone. A monitor on a dead pid reports `:noproc`
  # at once, so this works whether or not the process had already exited.
  defp await_down(pid) do
    ref = Process.monitor(pid)
    assert_receive {:DOWN, ^ref, :process, ^pid, _reason}
  end

  defp conn_pid(%{pool: nil, adapter_payload: %{conn_pid: pid}}), do: pid

  defp conn_pid(%{pool: pool_ref}) do
    [{pool_pid, _}] =
      Registry.lookup(GRPC.Client.Pool.Registry, {GRPC.Client.Pool.Server, pool_ref})

    %{channels: channels} = :sys.get_state(pool_pid)
    [%{channel: %{adapter_payload: %{conn_pid: pid}}}] = Map.values(channels)
    pid
  end

  defp gun_pid(conn_pid), do: :sys.get_state(conn_pid).gun_pid

  defp start_feature_server do
    {:ok, _pid, port} = GRPC.Server.start(FeatureServer, 0)
    on_exit(fn -> GRPC.Server.stop(FeatureServer) end)
    port
  end

  # Peer that completes the HTTP/2 handshake and never answers a request.
  defp start_quiet_peer do
    {:ok, listen_socket} = :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true])
    {:ok, port} = :inet.port(listen_socket)

    peer =
      spawn_link(fn ->
        {:ok, socket} = :gen_tcp.accept(listen_socket)
        :ok = :gen_tcp.send(socket, <<0::24, 0x04, 0, 0::32>>)
        :ok = :gen_tcp.send(socket, <<0::24, 0x04, 0x01, 0::32>>)

        receive do
          :close -> :gen_tcp.close(socket)
        end
      end)

    on_exit(fn ->
      send(peer, :close)
      :gen_tcp.close(listen_socket)
    end)

    port
  end

  for pooled? <- [false, true] do
    describe "channel from GRPC.Stub.connect/2 (pool_enabled: #{pooled?})" do
      setup do
        Application.put_env(:grpc, :pool_enabled, unquote(pooled?))
        port = start_feature_server()
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
        %{channel: channel}
      end

      test "after GRPC.Stub.disconnect/1", %{channel: channel} do
        conn_pid = conn_pid(channel)
        {:ok, disconnected} = GRPC.Stub.disconnect(channel)
        await_down(conn_pid)

        assert_unavailable(rpc_results(disconnected))
      end

      test "after the connection process is killed", %{channel: channel} do
        conn_pid = conn_pid(channel)
        Process.exit(conn_pid, :kill)
        await_down(conn_pid)

        check_after_connection_loss(channel, unquote(pooled?))
      end

      test "after the connection process exits normally", %{channel: channel} do
        conn_pid = conn_pid(channel)
        GenServer.stop(conn_pid, :normal)
        await_down(conn_pid)

        check_after_connection_loss(channel, unquote(pooled?))
      end

      test "after Gun gives up and exits", %{channel: channel} do
        conn_pid = conn_pid(channel)
        Process.exit(gun_pid(conn_pid), :kill)
        await_down(conn_pid)

        check_after_connection_loss(channel, unquote(pooled?))
      end
    end
  end

  # A bare channel keeps no replacement, so every call fails. A pool starts a
  # new connection to the live server, so a call may also succeed.
  defp check_after_connection_loss(channel, false), do: assert_unavailable(rpc_results(channel))

  defp check_after_connection_loss(channel, true),
    do: assert_ok_or_unavailable(rpc_results(channel))

  describe "calls in flight when the connection process stops" do
    setup do
      Application.put_env(:grpc, :pool_enabled, false)
      port = start_quiet_peer()
      attach_telemetry([:grpc, :client, :transport, :streams])
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      %{channel: channel, conn_pid: conn_pid(channel)}
    end

    for stop <- [:kill, :normal, :disconnect] do
      test "unary call when the connection process stops (#{stop})", %{
        channel: channel,
        conn_pid: conn_pid
      } do
        task =
          Task.async(fn ->
            Routeguide.RouteGuide.Stub.get_feature(channel, point(), timeout: 2_000)
          end)

        assert_receive {:telemetry, _, %{active: 1}, %{transport_ref: ^conn_pid}}
        # The event fires before the connection process replies to the request;
        # this call returns once the reply is on its way.
        :sys.get_state(conn_pid)
        stop_connection(unquote(stop), channel, conn_pid)

        assert_unavailable(unary: Task.await(task))
      end

      test "server-streaming call when the connection process stops (#{stop})", %{
        channel: channel,
        conn_pid: conn_pid
      } do
        task =
          Task.async(fn ->
            Routeguide.RouteGuide.Stub.list_features(channel, rectangle(), timeout: 2_000)
          end)

        assert_receive {:telemetry, _, %{active: 1}, %{transport_ref: ^conn_pid}}
        # The event fires before the connection process replies to the request;
        # this call returns once the reply is on its way.
        :sys.get_state(conn_pid)
        stop_connection(unquote(stop), channel, conn_pid)

        assert_unavailable(server_stream: Task.await(task))
      end
    end
  end

  defp stop_connection(:kill, _channel, conn_pid) do
    Process.exit(conn_pid, :kill)
    await_down(conn_pid)
  end

  defp stop_connection(:normal, _channel, conn_pid) do
    GenServer.stop(conn_pid, :normal)
    await_down(conn_pid)
  end

  defp stop_connection(:disconnect, channel, conn_pid) do
    {:ok, _} = GRPC.Stub.disconnect(channel)
    await_down(conn_pid)
  end

  # A caller can hold a channel whose connection process died after the
  # liveness check in GRPC.Stub, so the adapter has to cope with it itself.
  describe "adapter calls on a connection process that is gone" do
    setup do
      pid = spawn(fn -> :ok end)
      await_down(pid)

      secret_headers = %{"authorization" => "Bearer secret-token"}

      stale = fn conn_pid, server_stream ->
        channel = build(:channel, adapter_payload: %{conn_pid: conn_pid})

        %Stream{
          channel: channel,
          path: @get_feature,
          codec: GRPC.Codec.Proto,
          request_mod: Routeguide.Point,
          response_mod: Routeguide.Feature,
          server_stream: server_stream,
          headers: secret_headers
        }
      end

      %{stale: stale, dead_pid: pid}
    end

    for {label, conn_pid_key} <- [{"dead pid", :dead_pid}, {"nil conn_pid", nil}] do
      test "send_request/3 returns a stream whose receive_data/2 is unavailable (#{label})",
           ctx do
        conn_pid = if unquote(conn_pid_key), do: ctx.dead_pid
        message = GRPC.Codec.Proto.encode(point())

        for server_stream <- [false, true] do
          stream = ctx.stale.(conn_pid, server_stream)
          stream = Gun.send_request(stream, message, timeout: 1_000)

          assert {:error, %GRPC.RPCError{status: @unavailable, message: error_message}} =
                   Gun.receive_data(stream, timeout: 1_000)

          assert error_message =~ "connection_error"
          refute error_message =~ "secret-token"
        end
      end

      test "streaming calls report unavailable from receive_data/2 and never raise (#{label})",
           ctx do
        conn_pid = if unquote(conn_pid_key), do: ctx.dead_pid
        message = GRPC.Codec.Proto.encode(point())

        stream = ctx.stale.(conn_pid, false)
        stream = Gun.send_headers(stream, timeout: 1_000)
        stream = Gun.send_data(stream, message, [])
        stream = Gun.send_data(stream, message, send_end_stream: true)
        stream = Gun.end_stream(stream)

        assert {:error, %GRPC.RPCError{status: @unavailable, message: error_message}} =
                 Gun.receive_data(stream, timeout: 1_000)

        refute error_message =~ "secret-token"
        assert :ok = Gun.cancel(stream)
      end

      test "cancel/1 returns :ok for a stream opened on a live connection (#{label})", ctx do
        # The stream was opened while the connection lived; the connection is gone now.
        conn_pid = if unquote(conn_pid_key), do: ctx.dead_pid

        stream =
          ctx.stale.(conn_pid, true)
          |> Stream.put_payload(:stream_ref, make_ref())
          |> Stream.put_payload(:response_pid, ctx.dead_pid)

        assert :ok = Gun.cancel(stream)
      end
    end
  end

  describe "receive_data/2 when the response process reports a connection failure" do
    for reason <- [:down, :noproc, :normal, :shutdown] do
      test "maps {#{inspect(reason)}, _} to unavailable" do
        {:ok, response_pid} =
          FakeResponseProcess.start_link({:error, {unquote(reason), :normal}})

        for server_stream <- [false, true] do
          stream = %Stream{payload: %{response_pid: response_pid}, server_stream: server_stream}

          assert {:error, %GRPC.RPCError{status: @unavailable, message: message}} =
                   Gun.receive_data(stream, timeout: 1_000)

          assert message =~ "connection_error"
          assert message =~ inspect(unquote(reason))
        end
      end
    end

    test "keeps unknown errors as UNKNOWN" do
      {:ok, response_pid} = FakeResponseProcess.start_link({:error, {:weird, :thing}})
      stream = %Stream{payload: %{response_pid: response_pid}, server_stream: false}
      unknown = GRPC.Status.unknown()

      assert {:error, %GRPC.RPCError{status: ^unknown}} = Gun.receive_data(stream, timeout: 1_000)
    end
  end
end
