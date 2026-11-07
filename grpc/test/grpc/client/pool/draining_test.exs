defmodule GRPC.Client.Pool.DrainingTest do
  # A channel whose connection got GOAWAY gets no new calls, and leaves the pool when
  # its last call ends. A fake adapter stands in for the connection.
  use ExUnit.Case, async: true

  alias GRPC.Client.Pool.Config
  alias GRPC.Client.Pool.Server.State

  @subject GRPC.Client.Pool.Implementation

  defmodule FakeAdapter do
    def connect(channel, _opts) do
      {:ok, %{channel | adapter_payload: %{conn_pid: spawn(fn -> Process.sleep(:infinity) end)}}}
    end

    def disconnect(channel) do
      Process.exit(channel.adapter_payload.conn_pid, :kill)
      {:ok, channel}
    end
  end

  defp state(pool_size, max_overflow) do
    @subject.init(%State{
      config: %Config{
        pool_ref: make_ref(),
        channel: %GRPC.Channel{host: "localhost", port: 1, adapter: FakeAdapter},
        pool_size: pool_size,
        max_pool_overflow: max_overflow,
        adapter_opts: []
      }
    })
  end

  defp conn_pid(%State.Channel{channel: %{adapter_payload: %{conn_pid: pid}}}), do: pid

  defp take(state), do: @subject.take_channel(state, self())

  test "a draining channel gets no new call, and the pool opens a replacement" do
    state = state(1, 0)
    {old, _ref, state} = take(state)

    state = @subject.handle_draining(state, conn_pid(old))

    assert %{draining: true, open_streams: 1} = state.channels[old.id]
    assert @subject.live_channel_count(state) == 1

    {new, _ref, state} = take(state)
    assert new.id != old.id
    assert map_size(state.channels) == 2
  end

  test "draining channels do not count against the overflow limit" do
    state = state(1, 0)
    {old, _ref, state} = take(state)
    state = @subject.handle_draining(state, conn_pid(old))

    # The replacement is the only live channel. It is at the limit with no overflow.
    assert {_new, _ref, state} = take(state)
    assert map_size(state.channels) == 2
  end

  test "the draining channel is reaped when its last call ends, the live one is kept" do
    state = state(1, 0)
    {old, old_ref, state} = take(state)
    state = @subject.handle_draining(state, conn_pid(old))
    [new] = for {_id, ch} <- state.channels, not ch.draining, do: ch

    {state, _ref} = @subject.return_channel(state, old_ref)

    assert Map.keys(state.channels) == [new.id]
    refute Process.alive?(conn_pid(old))
    assert Process.alive?(conn_pid(new))
  end

  test "a draining channel with no call is reaped at once" do
    state = state(1, 0)
    [old] = Map.values(state.channels)

    state = @subject.handle_draining(state, conn_pid(old))

    refute Map.has_key?(state.channels, old.id)
    assert @subject.live_channel_count(state) == 1
  end

  test "a draining connection that stops does not open a second replacement" do
    state = state(1, 0)
    {old, _ref, state} = take(state)
    state = @subject.handle_draining(state, conn_pid(old))

    {state, _monitor_refs} = @subject.handle_connection_process_crush(state, conn_pid(old))

    assert @subject.live_channel_count(state) == 1
    assert map_size(state.channels) == 1
  end

  test "a signal for an unknown or draining connection changes nothing" do
    state = state(1, 0)
    {old, _ref, state} = take(state)
    state = @subject.handle_draining(state, conn_pid(old))

    assert @subject.handle_draining(state, conn_pid(old)) == state
    assert @subject.handle_draining(state, self()) == state
  end
end
