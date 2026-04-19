defmodule GRPC.Client.Pool.Implementation do
  @moduledoc false

  require Logger

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

  defp open_new_connection(%Config{} = config) do
    case config.channel.adapter.connect(config.channel, config.adapter_opts) do
      {:ok, %GRPC.Channel{} = channel} ->
        channel_id = make_ref()
        wrapped_channel = %State.Channel{channel: channel, id: channel_id, open_streams: 0}

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
    deficit = config.pool_size - map_size(channels)

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

  @spec return_channel(State.t(), reference()) :: {State.t(), reference() | nil}
  def return_channel(state, monitor_ref) when is_reference(monitor_ref) do
    case Map.get(state.leases_by_monitor_ref, monitor_ref) do
      nil ->
        {state, monitor_ref}

      %State.Lease{id: id, caller_pid: caller_pid} ->
        channel = Map.fetch!(state.channels, id)
        return_channel(state, channel, caller_pid)
    end
  end

  @spec return_channel(State.t(), State.Channel.t(), pid()) :: {State.t(), reference() | nil}
  def return_channel(
        %State{channels: channels} = state,
        %State.Channel{} = wrapped_channel,
        caller_pid
      ) do
    channels
    |> Map.has_key?(wrapped_channel.id)
    |> maybe_return_channel(state, wrapped_channel, caller_pid)
  end

  defp maybe_return_channel(
         true = _channel_in_state,
         %State{} = state,
         %State.Channel{} = wrapped_channel,
         caller_pid
       ) do
    state
    |> decrement_open_streams_count(wrapped_channel)
    |> remove_lease_from_leases_by_channel_id(wrapped_channel, caller_pid)
    |> remove_lease_from_leases_by_monitor_ref()
  end

  defp maybe_return_channel(
         false = _channel_in_state,
         %State{} = state,
         _wrapped_channel,
         _caller_pid
       ) do
    {state, nil}
  end

  defp decrement_open_streams_count(%State{} = state, channel) do
    update_in(state, [:channels, channel.id, :open_streams], &(&1 - 1))
  end

  defp remove_lease_from_leases_by_channel_id(
         %State{leases_by_channel_id: leases} = state,
         channel,
         caller_pid
       ) do
    old_leases = Map.get(leases, channel.id, [])
    old_lease = Enum.find(old_leases, fn %State.Lease{caller_pid: pid} -> pid == caller_pid end)

    new_leases =
      Enum.reject(old_leases, fn %State.Lease{caller_pid: pid} -> pid == caller_pid end)

    {put_in(state, [:leases_by_channel_id, channel.id], new_leases),
     old_lease && old_lease.monitor_ref}
  end

  defp remove_lease_from_leases_by_monitor_ref(
         {%State{leases_by_monitor_ref: leases} = state, monitor_ref}
       ) do
    {%{state | leases_by_monitor_ref: Map.delete(leases, monitor_ref)}, monitor_ref}
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

  defp maybe_open_channels(%{channels: channels} = state) when map_size(channels) > 0, do: state
  defp maybe_open_channels(%{channels: %{}} = state), do: init(state)

  defp choose_channel(
         %{
           channels: wrapped_channel,
           config: %Config{max_client_streams_per_connection: max_streams}
         } = state
       ) do
    {
      Enum.find_value(wrapped_channel, fn {_id, %State.Channel{open_streams: open_streams} = ch} ->
        if is_nil(max_streams) or open_streams < max_streams, do: ch
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

  defp maybe_open_new_connection({nil, %State{channels: channels, config: config} = state})
       when not is_nil(config.max_pool_overflow) and
              map_size(channels) >= config.pool_size + config.max_pool_overflow,
       do: {nil, state}

  defp maybe_open_new_connection({nil, %State{config: config} = state}) do
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
