defmodule GRPC.Client.Adapters.Gun.ConnectionProcess do
  @moduledoc """
  Owns long-lived Gun connections on behalf of the Gun adapter.

  This process exists so named Gun-backed channels are no longer tied to the
  lifecycle of the process that happened to call `GRPC.Stub.connect/2`.

  Without this wrapper, `GRPC.Client.Connection` would need to own Gun directly
  or understand Gun-specific owner messages. Keeping Gun ownership in this
  adapter-local process preserves a clean adapter boundary while ensuring the
  underlying Gun connection survives short-lived callers.

  Request-specific Gun messages are routed to per-stream response processes, so
  this process only needs to manage connection-level lifecycle and stream
  bookkeeping.

  Named channels are registered in the node-local `GRPC.Client.Registry` under
  `{ref, host, port}`, so a connection is reused by every caller on the node.
  """

  use GenServer

  @deadline_cancel_grace_ms 100

  require Logger
  alias GRPC.Client.Adapters.Gun.StreamResponseProcess

  def connect(channel, open_opts) when is_map(open_opts) do
    case DynamicSupervisor.start_child(GRPC.Client.Supervisor, child_spec(channel, open_opts)) do
      {:ok, connection_process_pid} ->
        {:ok, %{conn_pid: connection_process_pid}}

      {:error, {:already_started, connection_process_pid}} ->
        {:ok, %{conn_pid: connection_process_pid}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def disconnect(connection_process_pid) when is_pid(connection_process_pid) do
    GenServer.call(connection_process_pid, :disconnect)
  catch
    :exit, _reason ->
      :ok
  end

  def request(connection_process_pid, path, headers, body, deadline \\ :infinity) do
    GenServer.call(connection_process_pid, {:request, path, headers, body, deadline})
  end

  def open_stream(connection_process_pid, path, headers, deadline \\ :infinity) do
    GenServer.call(connection_process_pid, {:open_stream, path, headers, deadline})
  end

  def send_data(connection_process_pid, stream_ref, fin, data) do
    GenServer.call(connection_process_pid, {:send_data, stream_ref, fin, data})
  end

  def cancel(connection_process_pid, stream_ref) do
    GenServer.call(connection_process_pid, {:cancel, stream_ref})
  end

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
  def init({%{host: host, port: port}, open_opts}) do
    {await_timeout, open_opts} = Map.pop(open_opts, :await_timeout, 5_000)

    case open(host, port, open_opts) do
      {:ok, gun_pid} ->
        case :gun.await_up(gun_pid, await_timeout) do
          {:ok, :http2} ->
            # Monitor Gun so we don't keep casting into a dead pid if Gun
            # exhausts its reconnect retries (or crashes)
            Process.monitor(gun_pid)
            {:ok, %{gun_pid: gun_pid, response_processes: %{}}}

          {:ok, proto} ->
            :gun.shutdown(gun_pid)
            {:stop, "Error when opening connection: protocol #{proto} is not http2"}

          {:error, reason} ->
            :gun.shutdown(gun_pid)
            {:stop, reason}
        end

      {:error, reason} ->
        {:stop, reason}
    end
  end

  @impl GenServer
  def handle_call(:disconnect, _from, %{gun_pid: gun_pid} = state) do
    :ok = :gun.shutdown(gun_pid)
    {:stop, :normal, :ok, state}
  end

  def handle_call({:request, path, headers, body, deadline}, _from, %{gun_pid: gun_pid} = state) do
    with {:ok, response_pid} <- start_response_process(),
         stream_ref <- :gun.post(gun_pid, path, headers, body, %{reply_to: response_pid}),
         state <- put_response_pid(state, stream_ref, response_pid),
         :ok <- StreamResponseProcess.track(response_pid, self(), stream_ref, deadline) do
      {:reply, {:ok, %{stream_ref: stream_ref, response_pid: response_pid}}, state}
    else
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:open_stream, path, headers, deadline}, _from, %{gun_pid: gun_pid} = state) do
    with {:ok, response_pid} <- start_response_process(),
         stream_ref <- :gun.post(gun_pid, path, headers, %{reply_to: response_pid}),
         state <- put_response_pid(state, stream_ref, response_pid),
         :ok <- StreamResponseProcess.track(response_pid, self(), stream_ref, deadline) do
      {:reply, {:ok, %{stream_ref: stream_ref, response_pid: response_pid}}, state}
    else
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:send_data, stream_ref, fin, data}, _from, %{gun_pid: gun_pid} = state) do
    :ok = :gun.data(gun_pid, stream_ref, fin, data)
    {:reply, :ok, state}
  end

  def handle_call({:cancel, stream_ref}, _from, state) do
    {:reply, :ok, cancel_stream(state, stream_ref, nil, true)}
  end

  @impl GenServer
  def handle_info({:gun_up, _gun_pid, _protocol}, state), do: {:noreply, state}

  def handle_info({:stream_expired, stream_ref, response_pid}, state) do
    if response_pid(state, stream_ref) == response_pid do
      # Give deadline response frames already in flight a chance to close the
      # stream before Gun sends RST_STREAM. Gun 2.4 treats late HEADERS after
      # a local reset as a connection error.
      Process.send_after(self(), {:cancel_expired_stream, stream_ref}, @deadline_cancel_grace_ms)
      {:noreply, drop_response_pid(state, stream_ref)}
    else
      {:noreply, state}
    end
  end

  def handle_info({:cancel_expired_stream, stream_ref}, %{gun_pid: gun_pid} = state) do
    case :gun.stream_info(gun_pid, stream_ref) do
      {:ok, :undefined} -> :ok
      {:ok, _info} -> :gun.cancel(gun_pid, stream_ref)
      {:error, :not_connected} -> :ok
    end

    {:noreply, state}
  end

  def handle_info({:gun_down, _gun_pid, _protocol, reason, killed_streams}, state) do
    new_state =
      Enum.reduce(killed_streams, state, fn stream_ref, acc ->
        if response_pid = response_pid(acc, stream_ref) do
          send(response_pid, {:connection_down, reason})
        end

        drop_response_pid(acc, stream_ref)
      end)

    {:noreply, new_state}
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

  defp open({:local, socket_path}, _port, open_opts), do: :gun.open_unix(socket_path, open_opts)
  defp open(host, port, open_opts), do: :gun.open(parse_address(host), port, open_opts)

  defp parse_address(host) do
    host = String.to_charlist(host)

    case :inet.parse_address(host) do
      {:ok, address} -> address
      {:error, _} -> host
    end
  end

  defp via(channel) do
    {:via, Registry, {GRPC.Client.Registry, {__MODULE__, owner_key(channel)}}}
  end

  defp start_response_process do
    StreamResponseProcess.start_link()
  end

  defp put_response_pid(%{response_processes: processes} = state, stream_ref, response_pid) do
    monitor_ref = Process.monitor(response_pid)
    %{state | response_processes: Map.put(processes, stream_ref, {response_pid, monitor_ref})}
  end

  defp drop_response_pid(%{response_processes: processes} = state, stream_ref) do
    case Map.pop(processes, stream_ref) do
      {{_response_pid, monitor_ref}, remaining} ->
        Process.demonitor(monitor_ref, [:flush])
        %{state | response_processes: remaining}

      {nil, _remaining} ->
        state
    end
  end

  defp drop_response_pid_by_monitor(%{response_processes: processes} = state, monitor_ref) do
    case Enum.find(processes, fn {_stream_ref, {_response_pid, ref}} -> ref == monitor_ref end) do
      {stream_ref, _entry} -> %{state | response_processes: Map.delete(processes, stream_ref)}
      nil -> state
    end
  end

  defp response_pid(%{response_processes: processes}, stream_ref) do
    case Map.get(processes, stream_ref) do
      {response_pid, _monitor_ref} -> response_pid
      nil -> nil
    end
  end

  defp cancel_stream(%{response_processes: processes} = state, stream_ref, expected_pid, stop?) do
    case Map.get(processes, stream_ref) do
      {response_pid, _monitor_ref}
      when is_nil(expected_pid) or response_pid == expected_pid ->
        :ok = :gun.cancel(state.gun_pid, stream_ref)
        if stop?, do: stop_response_process(response_pid)
        drop_response_pid(state, stream_ref)

      _ ->
        state
    end
  end

  defp stop_response_process(response_pid) do
    GenServer.stop(response_pid, :normal)
  catch
    :exit, _reason -> :ok
  end

  defp owner_key(%{ref: ref, host: host, port: port}), do: {ref, host, port}
end
