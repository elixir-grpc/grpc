defmodule GRPC.Client.Pool.Implementation do
  @moduledoc false

  alias GRPC.Client.Pool.Config
  alias GRPC.Client.Pool.Server.State
  alias GRPC.Client.Pool.HealthCheck.DynamicSupervisor

  @spec init(State.t()) :: State.t()
  def init(%State{config: %Config{} = config} = state) do
    pool_size = config.pool_size
    open_connection_pool(state, pool_size)
  end

  defp open_connection_pool(%State{config: config} = state, pool_size) do
    Enum.reduce(1..pool_size, state, fn _, state_acc ->
      case open_new_connection(config) do
        %State.Channel{} = wrapped_channel ->
          state_acc
          |> put_in([:channels, wrapped_channel.id], wrapped_channel)
          |> put_in([:leases_by_channel_id, wrapped_channel.id], [])

        _ ->
          state_acc
      end
    end)
  end

  defp open_new_connection(%Config{channel: %GRPC.Channel{} = base_channel} = config) do
    # The Gun adapter registers named channels (by ref) and returns the existing
    # connection on a duplicate name. Every pool slot connects from the same
    # config.channel, so clearing the ref is what makes each slot an independent
    # connection instead of all slots aliasing one shared process.
    connect_channel = %GRPC.Channel{base_channel | ref: nil}

    adapter_opts =
      config.adapter_opts
      |> Keyword.put(:logical_connection_ref, base_channel.ref)
      |> put_notify_drain(connect_channel.adapter)

    case connect_channel.adapter.connect(connect_channel, adapter_opts) do
      {:ok, %GRPC.Channel{} = channel} ->
        channel_id = make_ref()

        wrapped_channel = %State.Channel{
          channel: channel,
          id: channel_id,
          open_streams: 0,
          monitor_ref: monitor_connection(channel)
        }

        if config.health_check_enabled do
          DynamicSupervisor.start(
            channel_id,
            fetch_conn_pid_from_channel(wrapped_channel),
            config.pool_ref
          )
        end

        wrapped_channel

      _ ->
        nil
    end
  end

  # A Mint connection process tells the pool when it starts to drain after GOAWAY.
  # Other adapters do not take the option.
  defp put_notify_drain(adapter_opts, GRPC.Client.Adapters.Mint),
    do: Keyword.put(adapter_opts, :notify_drain, true)

  defp put_notify_drain(adapter_opts, _adapter), do: adapter_opts

  # Connection processes are started under GRPC.Client.Supervisor, not linked to the
  # pool, so monitoring is how the pool learns a connection has died.
  defp monitor_connection(%GRPC.Channel{adapter_payload: %{conn_pid: conn_pid}})
       when is_pid(conn_pid),
       do: Process.monitor(conn_pid)

  defp monitor_connection(_channel), do: nil

  @doc """
  Marks the channel of a connection that got GOAWAY as draining and opens a
  replacement. A draining channel gets no new calls. It leaves the pool when its
  last call ends or its connection process stops.
  """
  @spec handle_draining(State.t(), pid()) :: State.t()
  def handle_draining(state, pid) do
    case find_channel_by_adapter_payload_connection_pid(state, pid) do
      {state, %State.Channel{draining: false} = channel} ->
        state = put_in(state, [:channels, channel.id, :draining], true)
        {state, _monitor_ref} = maybe_reap_channel({state, nil}, channel)
        {state, _monitor_refs} = maybe_replenish_pool({state, []})
        state

      {state, _already_draining_or_unknown} ->
        state
    end
  end

  @doc "The number of channels that are not draining."
  @spec live_channel_count(State.t()) :: non_neg_integer()
  def live_channel_count(%State{channels: channels}), do: count_live(channels)

  defp count_live(channels), do: Enum.count(channels, fn {_id, ch} -> not ch.draining end)

  @spec handle_connection_process_crush(State.t(), reference()) :: {State.t(), [reference(), ...]}
  def handle_connection_process_crush(state, pid) do
    state
    |> find_channel_by_adapter_payload_connection_pid(pid)
    |> maybe_remove_channel_and_leases()
  end

  defp find_channel_by_adapter_payload_connection_pid(%State{channels: channels} = state, pid) do
    {state,
     Enum.find_value(channels, fn {_id, wrapped_channel} ->
       if fetch_conn_pid_from_channel(wrapped_channel) == pid, do: wrapped_channel
     end)}
  end

  defp maybe_remove_channel_and_leases({state, nil = _channel} = _state_channel_tuple) do
    {state, []}
  end

  defp maybe_remove_channel_and_leases(
         {%State{channels: _}, %State.Channel{}} = state_channel_tuple
       ) do
    state_channel_tuple
    |> remove_channel()
    |> remove_leases_by_channel_id_for_channel()
    |> remove_leases_by_monitor_reference_for_channel()
    |> maybe_replenish_pool()
  end

  defp maybe_replenish_pool({%State{channels: channels, config: config} = state, monitor_refs}) do
    deficit = config.pool_size - count_live(channels)

    {
      if(deficit > 0, do: open_connection_pool(state, deficit), else: state),
      monitor_refs
    }
  end

  defp remove_channel({%State{channels: channels} = state, %State.Channel{id: id} = channel}) do
    {%{state | channels: Map.delete(channels, id)}, channel}
  end

  defp remove_leases_by_channel_id_for_channel(
         {%State{leases_by_channel_id: leases} = state, %State.Channel{id: id}}
       ) do
    {removed_leases, leases} = Map.pop(leases, id, [])

    monitor_refs =
      Enum.map(removed_leases, fn %State.Lease{monitor_ref: monitor_ref} -> monitor_ref end)

    {%{state | leases_by_channel_id: leases}, monitor_refs}
  end

  defp remove_leases_by_monitor_reference_for_channel(
         {%State{leases_by_monitor_ref: leases} = state, monitor_refs}
       ) do
    leases = Map.drop(leases, monitor_refs)
    {%{state | leases_by_monitor_ref: leases}, monitor_refs}
  end

  @spec handle_down(State.t(), reference()) :: {State.t(), [reference()]}
  def handle_down(state, ref) do
    case find_channel_by_monitor_ref(state, ref) do
      nil ->
        {new_state, _lease_ref} = return_channel(state, ref)
        {new_state, []}

      %State.Channel{} = wrapped_channel ->
        maybe_remove_channel_and_leases({state, wrapped_channel})
    end
  end

  defp find_channel_by_monitor_ref(%State{channels: channels}, ref) do
    Enum.find_value(channels, fn {_id, %State.Channel{monitor_ref: monitor_ref} = channel} ->
      if monitor_ref == ref, do: channel
    end)
  end

  @spec return_channel(State.t(), reference()) :: {State.t(), reference() | nil}
  def return_channel(state, monitor_ref) when is_reference(monitor_ref) do
    case Map.get(state.leases_by_monitor_ref, monitor_ref) do
      nil ->
        {state, monitor_ref}

      %State.Lease{id: id} ->
        channel = Map.fetch!(state.channels, id)
        return_channel(state, channel, monitor_ref)
    end
  end

  @spec return_channel(State.t(), State.Channel.t(), pid() | reference()) ::
          {State.t(), reference() | nil}
  def return_channel(
        %State{channels: channels} = state,
        %State.Channel{} = wrapped_channel,
        ref_or_pid
      ) do
    channels
    |> Map.has_key?(wrapped_channel.id)
    |> maybe_return_channel(state, wrapped_channel, ref_or_pid)
  end

  defp maybe_return_channel(
         true = _channel_in_state,
         %State{} = state,
         %State.Channel{} = wrapped_channel,
         ref_or_pid
       ) do
    state
    |> remove_lease_from_leases_by_channel_id(wrapped_channel, ref_or_pid)
    |> remove_lease_from_leases_by_monitor_ref()
    |> maybe_reap_channel(wrapped_channel)
  end

  defp maybe_return_channel(
         false = _channel_in_state,
         %State{} = state,
         _wrapped_channel,
         _ref_or_pid
       ) do
    {state, nil}
  end

  defp decrement_open_streams_count(%State{} = state, channel) do
    update_in(state, [:channels, channel.id, :open_streams], &max(0, &1 - 1))
  end

  defp remove_lease_from_leases_by_channel_id(
         %State{leases_by_channel_id: leases} = state,
         channel,
         ref_or_pid
       ) do
    old_leases = Map.get(leases, channel.id, [])

    matcher =
      if is_reference(ref_or_pid) do
        fn %State.Lease{monitor_ref: ref} -> ref == ref_or_pid end
      else
        fn %State.Lease{caller_pid: pid} -> pid == ref_or_pid end
      end

    case Enum.find(old_leases, matcher) do
      nil ->
        {state, nil}

      old_lease ->
        new_leases = List.delete(old_leases, old_lease)
        state = decrement_open_streams_count(state, channel)
        {put_in(state, [:leases_by_channel_id, channel.id], new_leases), old_lease.monitor_ref}
    end
  end

  defp remove_lease_from_leases_by_monitor_ref(
         {%State{leases_by_monitor_ref: leases} = state, monitor_ref}
       ) do
    {%{state | leases_by_monitor_ref: Map.delete(leases, monitor_ref)}, monitor_ref}
  end

  defp maybe_reap_channel({state, monitor_ref}, %State.Channel{id: channel_id}) do
    case Map.get(state.channels, channel_id) do
      %State.Channel{open_streams: 0, draining: true} = ch ->
        {reap_channel(state, ch), monitor_ref}

      %State.Channel{open_streams: 0} = ch ->
        if count_live(state.channels) > state.config.pool_size do
          {reap_channel(state, ch), monitor_ref}
        else
          {state, monitor_ref}
        end

      _ ->
        {state, monitor_ref}
    end
  end

  # The connection process of a draining channel can stop on its own at the same time.
  defp reap_channel(state, %State.Channel{id: channel_id} = ch) do
    try do
      ch.channel.adapter.disconnect(ch.channel)
    catch
      :exit, _reason -> :ok
    end

    if ch.monitor_ref, do: Process.demonitor(ch.monitor_ref, [:flush])

    new_channels = Map.delete(state.channels, channel_id)
    new_leases = Map.delete(state.leases_by_channel_id, channel_id)

    %{state | channels: new_channels, leases_by_channel_id: new_leases}
  end

  @spec take_channel(State.t(), pid()) :: {State.Channel.t(), reference(), State.t()}
  def take_channel(state, caller_pid) do
    state
    |> maybe_open_channels()
    |> choose_channel()
    |> maybe_open_new_connection()
    |> lease_channel(caller_pid)
  end

  @doc """
  This function may not return channel if requested channel has reached stream limit
  """
  @spec take_channel(State.t(), State.Channel.id(), pid()) ::
          {State.Channel.t() | nil, reference() | nil, State.t()}
  def take_channel(state, channel_id, caller_pid) do
    state
    |> choose_channel(channel_id)
    |> lease_channel(caller_pid)
  end

  defp maybe_open_channels(%{channels: channels} = state) do
    if count_live(channels) > 0, do: state, else: init(state)
  end

  defp choose_channel(
         %{
           channels: wrapped_channel,
           config: %Config{max_client_streams_per_connection: max_streams}
         } = state
       ) do
    {
      Enum.find_value(wrapped_channel, fn {_id, %State.Channel{open_streams: open_streams} = ch} ->
        if not ch.draining and (is_nil(max_streams) or open_streams < max_streams), do: ch
      end),
      state
    }
  end

  defp choose_channel(
         %{
           channels: wrapped_channel,
           config: %Config{max_client_streams_per_connection: max_streams}
         } = state,
         channel_id
       ) do
    {
      Enum.find_value(wrapped_channel, fn {_id,
                                           %State.Channel{open_streams: open_streams, id: id} = ch} ->
        if (is_nil(max_streams) or open_streams < max_streams) and id == channel_id, do: ch
      end),
      state
    }
  end

  defp maybe_open_new_connection({nil, %State{channels: channels, config: config} = state}) do
    if not is_nil(config.max_pool_overflow) and
         count_live(channels) >= config.pool_size + config.max_pool_overflow do
      {nil, state}
    else
      case open_new_connection(config) do
        %State.Channel{} = wrapped_channel ->
          state =
            state
            |> put_in([:channels, wrapped_channel.id], wrapped_channel)
            |> put_in([:leases_by_channel_id, wrapped_channel.id], [])

          {wrapped_channel, state}

        _ ->
          {nil, state}
      end
    end
  end

  defp maybe_open_new_connection({wrapped_channel, state}), do: {wrapped_channel, state}

  defp lease_channel({%State.Channel{} = wrapped_channel, state}, caller_pid) do
    monitor_ref = Process.monitor(caller_pid)

    {
      wrapped_channel,
      monitor_ref,
      state
      |> increment_open_streams_count(wrapped_channel)
      |> add_lease_to_leases_by_channel_id(wrapped_channel, monitor_ref, caller_pid)
      |> add_lease_to_leases_by_monitor_ref(wrapped_channel, monitor_ref, caller_pid)
    }
  end

  defp lease_channel({nil = _wrapped_channel, state}, _caller_pid), do: {nil, nil, state}

  defp increment_open_streams_count(%State{} = state, channel) do
    update_in(state, [:channels, channel.id, :open_streams], &(&1 + 1))
  end

  defp add_lease_to_leases_by_channel_id(%State{} = state, channel, monitor_ref, caller_pid) do
    new_lease = %State.Lease{monitor_ref: monitor_ref, caller_pid: caller_pid, id: channel.id}
    update_in(state, [:leases_by_channel_id, channel.id], &[new_lease | &1])
  end

  defp add_lease_to_leases_by_monitor_ref(%State{} = state, channel, monitor_ref, caller_pid) do
    put_in(state, [:leases_by_monitor_ref, monitor_ref], %State.Lease{
      id: channel.id,
      caller_pid: caller_pid,
      monitor_ref: monitor_ref
    })
  end

  defp fetch_conn_pid_from_channel(%State.Channel{
         channel: %GRPC.Channel{adapter_payload: %{conn_pid: conn_pid}}
       }),
       do: conn_pid
end
