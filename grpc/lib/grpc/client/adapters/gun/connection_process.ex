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
  """

  use GenServer

  require Logger
  alias GRPC.Client.Adapters.Gun.StreamResponseProcess

  @spec connect(GRPC.Channel.t(), map()) ::
          {:ok, %{conn_pid: pid()}} | {:error, any()}
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

  @spec disconnect(pid()) :: :ok
  def disconnect(connection_process_pid) when is_pid(connection_process_pid) do
    GenServer.call(connection_process_pid, :disconnect)
  catch
    :exit, _reason ->
      :ok
  end

  @spec request(pid(), iodata(), list(), iodata()) ::
          {:ok, %{stream_ref: reference(), response_pid: pid()}} | {:error, any()}
  def request(connection_process_pid, path, headers, body) do
    GenServer.call(connection_process_pid, {:request, path, headers, body})
  end

  @spec open_stream(pid(), iodata(), list()) ::
          {:ok, %{stream_ref: reference(), response_pid: pid()}} | {:error, any()}
  def open_stream(connection_process_pid, path, headers) do
    GenServer.call(connection_process_pid, {:open_stream, path, headers})
  end

  @spec send_data(pid(), reference(), :fin | :nofin, iodata()) :: :ok
  def send_data(connection_process_pid, stream_ref, fin, data) do
    GenServer.call(connection_process_pid, {:send_data, stream_ref, fin, data})
  end

  @spec cancel(pid(), reference()) :: :ok
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

  @spec start_link(GRPC.Channel.t(), map()) :: GenServer.on_start()
  def start_link(channel, open_opts) do
    if is_nil(channel.ref) do
      GenServer.start_link(__MODULE__, {channel, open_opts})
    else
      GenServer.start_link(__MODULE__, {channel, open_opts}, name: via(channel))
    end
  end

  @impl GenServer
  def init({%{host: host, port: port}, open_opts}) do
    case open(host, port, open_opts) do
      {:ok, gun_pid} ->
        case :gun.await_up(gun_pid) do
          {:ok, :http2} ->
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

  def handle_call({:request, path, headers, body}, _from, %{gun_pid: gun_pid} = state) do
    with {:ok, response_pid} <- start_response_process(),
         stream_ref <- :gun.post(gun_pid, path, headers, body, %{reply_to: response_pid}) do
      {:reply, {:ok, %{stream_ref: stream_ref, response_pid: response_pid}},
       put_response_pid(state, stream_ref, response_pid)}
    else
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:open_stream, path, headers}, _from, %{gun_pid: gun_pid} = state) do
    with {:ok, response_pid} <- start_response_process(),
         stream_ref <- :gun.post(gun_pid, path, headers, %{reply_to: response_pid}) do
      {:reply, {:ok, %{stream_ref: stream_ref, response_pid: response_pid}},
       put_response_pid(state, stream_ref, response_pid)}
    else
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:send_data, stream_ref, fin, data}, _from, %{gun_pid: gun_pid} = state) do
    :ok = :gun.data(gun_pid, stream_ref, fin, data)
    {:reply, :ok, state}
  end

  def handle_call({:cancel, stream_ref}, _from, %{gun_pid: gun_pid} = state) do
    :ok = :gun.cancel(gun_pid, stream_ref)

    if response_pid = response_pid(state, stream_ref) do
      GenServer.stop(response_pid, :normal)
    end

    {:reply, :ok, drop_response_pid(state, stream_ref)}
  end

  @impl GenServer
  def handle_info({:gun_up, _gun_pid, _protocol}, state), do: {:noreply, state}

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
    {:global, {__MODULE__, owner_key(channel)}}
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

  defp owner_key(%{ref: ref, host: host, port: port}), do: {ref, host, port}
end
