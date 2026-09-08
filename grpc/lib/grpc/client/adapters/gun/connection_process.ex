defmodule GRPC.Client.Adapters.Gun.ConnectionProcess do
  @moduledoc """
  Owns long-lived Gun connections on behalf of the Gun adapter.

  This process exists so named Gun-backed channels are no longer tied to the
  lifecycle of the process that happened to call `GRPC.Stub.connect/2`.

  Request-specific Gun messages are routed to per-stream response processes.
  Named channels are registered in the node-local `GRPC.Client.Registry` under
  `{ref, host, port}`, so a connection is reused by every caller on the node.
  """

  use GenServer

  require Logger
  alias GRPC.Client.Adapters.Gun.StreamResponseProcess
  alias GRPC.Client.Telemetry

  def connect(channel, open_opts) when is_map(open_opts) do
    case DynamicSupervisor.start_child(GRPC.Client.Supervisor, child_spec(channel, open_opts)) do
      {:ok, pid} -> {:ok, %{conn_pid: pid}}
      {:error, {:already_started, pid}} -> {:ok, %{conn_pid: pid}}
      {:error, reason} -> {:error, reason}
    end
  end

  def disconnect(pid) when is_pid(pid) do
    GenServer.call(pid, :disconnect)
  catch
    :exit, _reason -> :ok
  end

  def request(pid, path, headers, body), do: GenServer.call(pid, {:request, path, headers, body})
  def open_stream(pid, path, headers), do: GenServer.call(pid, {:open_stream, path, headers})
  def send_data(pid, ref, fin, data), do: GenServer.call(pid, {:send_data, ref, fin, data})
  def cancel(pid, ref), do: GenServer.call(pid, {:cancel, ref})

  defp child_spec(channel, open_opts) do
    %{
      id: {__MODULE__, owner_key(channel)},
      start: {__MODULE__, :start_link, [channel, open_opts]},
      restart: :temporary,
      type: :worker,
      shutdown: 5000
    }
  end

  def start_link(channel, open_opts) do
    if is_nil(channel.ref) do
      GenServer.start_link(__MODULE__, {channel, open_opts})
    else
      GenServer.start_link(__MODULE__, {channel, open_opts}, name: via(channel))
    end
  end

  @impl GenServer
  def init({%{host: host, port: port} = channel, open_opts}) do
    metadata = metadata(channel)
    Telemetry.execute(:started, %{generation: 0, active_streams: 0}, metadata)
    {await_timeout, open_opts} = Map.pop(open_opts, :await_timeout, 5_000)

    result =
      with {:ok, gun_pid} <- open(host, port, open_opts),
           {:ok, :http2} <- await_http2(gun_pid, await_timeout) do
        # Stop this owner when Gun exhausts its own reconnect attempts.
        Process.monitor(gun_pid)

        state = %{
          channel: channel,
          gun_pid: gun_pid,
          response_processes: %{},
          generation: 1,
          max_concurrent_streams: :unknown
        }

        Telemetry.execute(:connected, %{generation: 1}, Map.put(metadata, :reconnect, false))
        {:ok, state}
      end

    case result do
      {:ok, _state} = ok ->
        ok

      {:error, reason} ->
        maybe_connect_error(metadata, channel.scheme, reason)
        Telemetry.execute(:stopped, %{streams_terminated: 0}, Map.put(metadata, :reason, reason))
        {:stop, reason}
    end
  end

  @impl GenServer
  def handle_call(:disconnect, _from, state) do
    :ok = :gun.shutdown(state.gun_pid)
    {:stop, :normal, :ok, state}
  end

  def handle_call({:request, path, headers, body}, _from, state) do
    open_request(state, fn response_pid ->
      :gun.post(state.gun_pid, path, headers, body, %{reply_to: response_pid})
    end)
  end

  def handle_call({:open_stream, path, headers}, _from, state) do
    open_request(state, fn response_pid ->
      :gun.post(state.gun_pid, path, headers, %{reply_to: response_pid})
    end)
  end

  def handle_call({:send_data, stream_ref, fin, data}, _from, state) do
    :ok = :gun.data(state.gun_pid, stream_ref, fin, data)
    {:reply, :ok, state}
  end

  def handle_call({:cancel, stream_ref}, _from, state) do
    :ok = :gun.cancel(state.gun_pid, stream_ref)

    if response_pid = response_pid(state, stream_ref) do
      GenServer.stop(response_pid, :normal)
    end

    {:reply, :ok, drop_response_pid(state, stream_ref)}
  end

  @impl GenServer
  def handle_info({:gun_up, _gun_pid, :http2}, state) do
    generation = state.generation + 1

    Telemetry.execute(
      :connected,
      %{generation: generation},
      state |> metadata() |> Map.put(:reconnect, true)
    )

    {:noreply, %{state | generation: generation, max_concurrent_streams: :unknown}}
  end

  def handle_info({:gun_up, _gun_pid, _protocol}, state), do: {:noreply, state}

  def handle_info({:gun_down, _gun_pid, _protocol, reason, killed_streams}, state) do
    {state, killed_count} = drop_killed_streams(state, killed_streams, reason)
    metadata = Map.put(metadata(state), :reason, reason)
    Telemetry.execute(:reset, %{streams_terminated: 0}, metadata)
    Telemetry.execute(:down, %{streams_terminated: killed_count}, metadata)
    {:noreply, %{state | max_concurrent_streams: :unknown}}
  end

  def handle_info({:gun_notify, _gun_pid, :settings_changed, settings}, state) do
    max = normalize_max_streams(Map.get(settings, :max_concurrent_streams, :unknown))

    if max != state.max_concurrent_streams do
      Telemetry.execute(:settings, %{}, Map.put(metadata(state), :max_concurrent_streams, max))
    end

    {:noreply, %{state | max_concurrent_streams: max}}
  end

  def handle_info({:stream_terminated, response_pid}, state) do
    {:noreply, drop_response_pid_by_pid(state, response_pid)}
  end

  def handle_info({:stream_timeout, response_pid}, state) do
    case response_entry_by_pid(state, response_pid) do
      {stream_ref, _entry} ->
        _ = :gun.cancel(state.gun_pid, stream_ref)
        {:noreply, drop_response_pid(state, stream_ref)}

      nil ->
        {:noreply, state}
    end
  end

  # Gun is gone for good (reconnect retries exhausted or a crash). Fail all
  # in-flight streams and stop
  def handle_info({:DOWN, _monitor_ref, :process, gun_pid, reason}, %{gun_pid: gun_pid} = state) do
    Enum.each(state.response_processes, fn {_stream_ref, {response_pid, _monitor_ref}} ->
      send(response_pid, {:connection_down, reason})
    end)

    {:stop, {:shutdown, {:gun_down, reason}}, state}
  end

  def handle_info({:DOWN, monitor_ref, :process, _pid, _reason}, state) do
    {:noreply, drop_response_pid_by_monitor(state, monitor_ref)}
  end

  def handle_info(msg, state) do
    Logger.warning("#{inspect(__MODULE__)} received unexpected message: #{inspect(msg)}")
    {:noreply, state}
  end

  @impl GenServer
  def terminate(reason, %{response_processes: processes} = state) do
    count = map_size(processes)

    Enum.each(processes, fn {_ref, {pid, monitor_ref}} ->
      Process.demonitor(monitor_ref, [:flush])
      if Process.alive?(pid), do: GenServer.stop(pid, :normal)
    end)

    if count > 0, do: emit_streams(state, 0)

    Telemetry.execute(
      :stopped,
      %{streams_terminated: count},
      Map.put(metadata(state), :reason, reason)
    )

    :ok
  end

  defp open_request(state, open_stream) do
    if at_capacity?(state) do
      Telemetry.execute(
        :stream_rejected,
        %{count: 1},
        Map.put(metadata(state), :reason, :max_concurrent_streams)
      )

      {:reply, {:error, capacity_error()}, state}
    else
      with {:ok, response_pid} <- StreamResponseProcess.start_link(self()),
           stream_ref <- open_stream.(response_pid) do
        new_state = put_response_pid(state, stream_ref, response_pid)
        {:reply, {:ok, %{stream_ref: stream_ref, response_pid: response_pid}}, new_state}
      else
        {:error, reason} -> {:reply, {:error, reason}, state}
      end
    end
  end

  defp at_capacity?(%{max_concurrent_streams: max, response_processes: streams})
       when is_integer(max),
       do: map_size(streams) >= max

  defp at_capacity?(_state), do: false

  defp capacity_error do
    GRPC.RPCError.exception(
      GRPC.Status.resource_exhausted(),
      "peer maximum concurrent streams exhausted"
    )
  end

  defp await_http2(gun_pid, timeout) do
    case :gun.await_up(gun_pid, timeout) do
      {:ok, :http2} ->
        {:ok, :http2}

      {:ok, protocol} ->
        :gun.shutdown(gun_pid)
        {:error, {:unexpected_protocol, protocol}}

      {:error, reason} ->
        :gun.shutdown(gun_pid)
        {:error, reason}
    end
  end

  defp open({:local, socket_path}, _port, opts), do: :gun.open_unix(socket_path, opts)
  defp open(host, port, opts), do: :gun.open(parse_address(host), port, opts)

  defp parse_address(host) do
    host = String.to_charlist(host)

    case :inet.parse_address(host) do
      {:ok, address} -> address
      {:error, _} -> host
    end
  end

  defp put_response_pid(state, stream_ref, response_pid) do
    monitor_ref = Process.monitor(response_pid)
    state = put_in(state.response_processes[stream_ref], {response_pid, monitor_ref})
    emit_streams(state, map_size(state.response_processes))
    state
  end

  defp drop_response_pid(state, stream_ref) do
    case Map.pop(state.response_processes, stream_ref) do
      {{_pid, monitor_ref}, remaining} ->
        Process.demonitor(monitor_ref, [:flush])
        state = %{state | response_processes: remaining}
        emit_streams(state, map_size(remaining))
        state

      {nil, _remaining} ->
        state
    end
  end

  defp drop_response_pid_by_monitor(state, monitor_ref) do
    case Enum.find(state.response_processes, fn {_ref, {_pid, ref}} -> ref == monitor_ref end) do
      {stream_ref, _entry} -> drop_response_pid(state, stream_ref)
      nil -> state
    end
  end

  defp drop_response_pid_by_pid(state, pid) do
    case response_entry_by_pid(state, pid) do
      {stream_ref, _entry} -> drop_response_pid(state, stream_ref)
      nil -> state
    end
  end

  defp response_entry_by_pid(state, pid) do
    Enum.find(state.response_processes, fn {_ref, {response_pid, _monitor}} ->
      response_pid == pid
    end)
  end

  defp drop_killed_streams(state, killed_streams, reason) do
    refs = MapSet.new(killed_streams)

    {killed, remaining} =
      Map.split(state.response_processes, MapSet.to_list(refs))

    Enum.each(killed, fn {_stream_ref, {pid, monitor_ref}} ->
      Process.demonitor(monitor_ref, [:flush])
      send(pid, {:connection_down, reason})
    end)

    state = %{state | response_processes: remaining}
    if map_size(killed) > 0, do: emit_streams(state, map_size(remaining))
    {state, map_size(killed)}
  end

  defp response_pid(state, stream_ref) do
    case Map.get(state.response_processes, stream_ref) do
      {pid, _monitor} -> pid
      nil -> nil
    end
  end

  defp emit_streams(state, active) do
    Telemetry.execute(:streams, %{active: active}, metadata(state))
  end

  defp normalize_max_streams(:infinity), do: :infinity
  defp normalize_max_streams(value) when is_integer(value) and value > 0, do: value
  defp normalize_max_streams(_value), do: :unknown

  defp maybe_connect_error(metadata, scheme, reason) do
    if stage = connect_stage(reason, scheme) do
      Telemetry.execute(:connect_error, %{}, Map.merge(metadata, %{stage: stage, reason: reason}))
    end
  end

  defp connect_stage(reason, _scheme) when reason in [:nxdomain, :host_not_found], do: :resolve

  defp connect_stage(reason, _scheme) when reason in [:econnrefused, :enetunreach, :ehostunreach],
    do: :tcp

  defp connect_stage({:tls_alert, _detail}, _scheme), do: :tls
  defp connect_stage({:unexpected_protocol, _protocol}, _scheme), do: :http2
  defp connect_stage({:down, reason}, scheme), do: connect_stage(reason, scheme)
  defp connect_stage({_tag, reason}, scheme), do: connect_stage(reason, scheme)
  defp connect_stage(_reason, _scheme), do: nil

  defp metadata(%{channel: channel}), do: metadata(channel)

  defp metadata(channel) do
    %{
      logical_connection_ref: channel.ref,
      transport_ref: self(),
      target: {channel.host, channel.port},
      adapter: GRPC.Client.Adapters.Gun
    }
  end

  defp via(channel),
    do: {:via, Registry, {GRPC.Client.Registry, {__MODULE__, owner_key(channel)}}}

  defp owner_key(%{ref: ref, host: host, port: port}), do: {ref, host, port}
end
