defmodule GRPC.Client.Adapters.GunMailboxTest do
  # Gun used to send `:gun_up` and `:gun_down` to the process that called
  # `GRPC.Stub.connect/2` (#340). `ConnectionProcess` now owns Gun, so the
  # caller's mailbox must stay free of Gun and exit messages in every phase of
  # a connection.
  #
  # async: false, because it toggles global Application env.
  use ExUnit.Case, async: false

  import GRPC.Integration.TestCase, only: [reconnect_server: 2]

  alias GRPC.Client.Adapters.Gun.ConnectionProcess

  @events for event <- [:started, :connected, :down, :stopped],
              do: [:grpc, :client, :transport, event]

  @deadline_ms 5_000

  defmodule FeatureServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service
    alias GRPC.Server

    def get_feature(point, _stream) do
      %Routeguide.Feature{location: point, name: "#{point.latitude},#{point.longitude}"}
    end

    def list_features(rectangle, stream) do
      Enum.each([rectangle.lo, rectangle.hi], fn point ->
        Server.send_reply(stream, %Routeguide.Feature{location: point, name: "listed"})
      end)
    end

    def route_chat(req_enum, stream) do
      Enum.each(req_enum, fn note ->
        Server.send_reply(stream, %{note | message: "Reply: #{note.message}"})
      end)
    end
  end

  # Gun's own retry delay is about a second, so shorten it to keep reconnects fast.
  def fast_retry_fun(retries, _opts), do: %{retries: retries - 1, timeout: 10}

  setup do
    previous = Application.get_env(:grpc, :pool_enabled)
    on_exit(fn -> Application.put_env(:grpc, :pool_enabled, previous) end)

    # Telemetry handlers run in the emitting process. Forward to a collector, not
    # to the test process, so that the test mailbox only holds what the caller
    # itself receives.
    {:ok, collector} = Agent.start_link(fn -> [] end)
    handler_id = {__MODULE__, make_ref()}

    :telemetry.attach_many(
      handler_id,
      @events,
      fn event, measurements, metadata, _config ->
        Agent.update(collector, &[{List.last(event), measurements, metadata} | &1])
      end,
      nil
    )

    on_exit(fn -> :telemetry.detach(handler_id) end)

    %{collector: collector}
  end

  for pool_enabled <- [true, false], trap_exit <- [false, true] do
    @pool_enabled pool_enabled
    @trap_exit trap_exit
    @mode if pool_enabled, do: "pool", else: "legacy"

    test "caller mailbox stays clean across the connection lifecycle (#{@mode}, trap_exit: #{@trap_exit})",
         %{collector: collector} do
      Application.put_env(:grpc, :pool_enabled, @pool_enabled)
      Process.flag(:trap_exit, @trap_exit)

      {:ok, _, port} = GRPC.Server.start(FeatureServer, 0)
      # Tracks whether the server is up, so cleanup doesn't stop it twice.
      server_running = :atomics.new(1, [])
      :atomics.put(server_running, 1, 1)

      on_exit(fn ->
        if :atomics.get(server_running, 1) == 1, do: GRPC.Server.stop(FeatureServer)
      end)

      {:ok, channel} =
        GRPC.Stub.connect("localhost:#{port}",
          adapter_opts: [retry_fun: &__MODULE__.fast_retry_fun/2]
        )

      # connect: the connection process is up and Gun belongs to it, not to us.
      [transport_pid] = wait_for_events(collector, :connected, 1)
      gun_pid = :sys.get_state(transport_pid).gun_pid
      assert_mailbox_clean("after connect")

      # call: unary, server streaming, bidi streaming
      assert_calls_work(channel)
      assert_mailbox_clean("after calls")

      # server loss: Gun reports the drop to the connection process.
      :ok = GRPC.Server.stop(FeatureServer)
      :atomics.put(server_running, 1, 0)
      wait_for_events(collector, :down, 1)
      assert_mailbox_clean("after server loss")

      # reconnect: Gun reconnects by itself (same Gun pid), and calls work again.
      {:ok, _, ^port} = reconnect_server(FeatureServer, port)
      :atomics.put(server_running, 1, 1)
      wait_for_events(collector, :connected, 2)
      assert_eventually("calls succeed after the reconnect", fn -> calls_work?(channel) end)
      assert Process.alive?(gun_pid)
      assert_mailbox_clean("after reconnect")

      # disconnect: the connection process and Gun both stop.
      {:ok, _channel} = GRPC.Stub.disconnect(channel)

      assert_eventually("connection process and Gun stop", fn ->
        not Process.alive?(transport_pid) and not Process.alive?(gun_pid)
      end)

      assert_mailbox_clean("after disconnect")
    end
  end

  test "ConnectionProcess is the Gun owner, not the caller" do
    Application.put_env(:grpc, :pool_enabled, false)
    {:ok, _, port} = GRPC.Server.start(FeatureServer, 0)
    on_exit(fn -> GRPC.Server.stop(FeatureServer) end)

    {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
    %{adapter_payload: %{conn_pid: conn_pid}} = channel
    %{gun_pid: gun_pid} = :sys.get_state(conn_pid)

    assert is_pid(gun_pid)
    assert gun_pid != self()

    # Gun monitors its owner, so the monitor tells us who the owner is.
    assert {:monitored_by, conn_monitors} = Process.info(conn_pid, :monitored_by)
    assert gun_pid in conn_monitors
    assert {:monitored_by, own_monitors} = Process.info(self(), :monitored_by)
    refute gun_pid in own_monitors
    assert ConnectionProcess.disconnect(conn_pid) == :ok
  end

  defp assert_calls_work(channel) do
    point = %Routeguide.Point{latitude: 409_146_138, longitude: -746_188_906}
    rectangle = %Routeguide.Rectangle{lo: point, hi: %{point | latitude: 1}}

    assert {:ok, %Routeguide.Feature{}} = Routeguide.RouteGuide.Stub.get_feature(channel, point)

    assert {:ok, features} = Routeguide.RouteGuide.Stub.list_features(channel, rectangle)
    assert [{:ok, _}, {:ok, _}] = Enum.to_list(features)

    stream = Routeguide.RouteGuide.Stub.route_chat(channel)
    note = %Routeguide.RouteNote{location: point, message: "hi"}
    GRPC.Stub.send_request(stream, note, end_stream: true)
    assert {:ok, replies} = GRPC.Stub.recv(stream)
    assert [{:ok, %{message: "Reply: hi"}}] = Enum.to_list(replies)
  end

  defp calls_work?(channel) do
    assert_calls_work(channel)
    true
  rescue
    _ -> false
  catch
    _, _ -> false
  end

  # Polls the telemetry collector until `count` events of `name` arrived, and
  # returns the transport pids that sent them.
  defp wait_for_events(collector, name, count) do
    assert_eventually("#{count} #{name} event(s)", fn ->
      length(events(collector, name)) >= count
    end)

    collector |> events(name) |> Enum.map(& &1.transport_ref) |> Enum.uniq()
  end

  defp events(collector, name) do
    collector
    |> Agent.get(& &1)
    |> Enum.filter(&(elem(&1, 0) == name))
    |> Enum.map(&elem(&1, 2))
  end

  defp assert_eventually(description, fun, deadline \\ nil) do
    deadline = deadline || System.monotonic_time(:millisecond) + @deadline_ms

    cond do
      fun.() ->
        :ok

      System.monotonic_time(:millisecond) > deadline ->
        flunk("timed out waiting for: #{description}")

      true ->
        Process.sleep(10)
        assert_eventually(description, fun, deadline)
    end
  end

  # A sentinel makes sure messages already on their way have been delivered
  # before we read the mailbox. We only call this once the connection state
  # has settled.
  defp assert_mailbox_clean(phase) do
    sentinel = make_ref()
    send(self(), {:sentinel, sentinel})
    assert_receive {:sentinel, ^sentinel}

    {:messages, messages} = Process.info(self(), :messages)

    leaked =
      Enum.filter(messages, fn
        {:EXIT, _pid, _reason} -> true
        {:DOWN, _ref, :process, _pid, _reason} -> true
        message when is_tuple(message) -> gun_message?(elem(message, 0))
        _other -> false
      end)

    assert leaked == [], "caller mailbox has leaked messages #{phase}: #{inspect(leaked)}"
  end

  defp gun_message?(tag) when is_atom(tag), do: String.starts_with?(Atom.to_string(tag), "gun_")
  defp gun_message?(_tag), do: false
end
