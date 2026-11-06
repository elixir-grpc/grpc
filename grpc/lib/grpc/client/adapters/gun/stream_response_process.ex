defmodule GRPC.Client.Adapters.Gun.StreamResponseProcess do
  @moduledoc """
  Owns response state for a single Gun request stream.

  Gun can deliver response messages for many concurrent streams over the same
  connection, so request-specific response handling needs to stay isolated per
  stream.

  This process buffers the messages for one stream, serves them back to the Gun
  adapter in arrival order, and terminates once the stream has reached a
  terminal state and all buffered messages have been consumed.
  """

  use GenServer
  require Logger

  @terminated_stream_error {:error, {:connection_error, :closed}}

  def start_link do
    GenServer.start_link(__MODULE__, [])
  end

  def track(pid, connection_pid, stream_ref, deadline) do
    GenServer.call(pid, {:track, connection_pid, stream_ref, deadline})
  catch
    :exit, _reason ->
      :ok
  end

  def await(pid, deadline) do
    GenServer.call(pid, {:await, deadline}, :infinity)
  catch
    :exit, _reason ->
      @terminated_stream_error
  end

  @impl GenServer
  def init([]) do
    {:ok,
     %{
       messages: :queue.new(),
       waiter: nil,
       done: false,
       deadline: :infinity,
       deadline_timer: nil,
       cleanup_target: nil
     }}
  end

  @impl GenServer
  def handle_call({:track, connection_pid, stream_ref, deadline}, _from, state) do
    state =
      state
      |> Map.put(:cleanup_target, {connection_pid, stream_ref})
      |> put_earlier_deadline(deadline)

    {:reply, :ok, state}
  end

  def handle_call({:await, deadline}, from, state) do
    state = put_earlier_deadline(state, deadline)
    %{messages: messages, done: done?} = state

    case :queue.out(messages) do
      {{:value, message}, remaining} ->
        new_state = %{state | messages: remaining}

        if done? and :queue.is_empty(remaining) do
          {:stop, :normal, message, new_state}
        else
          {:reply, message, new_state}
        end

      {:empty, _} ->
        if done? do
          {:stop, :normal, @terminated_stream_error, state}
        else
          {:noreply, %{state | waiter: from}}
        end
    end
  end

  @impl GenServer
  def handle_info({:deadline_expired, timer_ref}, %{deadline_timer: timer_ref} = state) do
    if state.waiter, do: GenServer.reply(state.waiter, {:error, :timeout})

    case state.cleanup_target do
      {connection_pid, stream_ref} ->
        send(connection_pid, {:stream_expired, stream_ref, self()})

      nil ->
        :ok
    end

    {:stop, :normal, %{state | waiter: nil, done: true, deadline_timer: nil}}
  end

  def handle_info({:deadline_expired, _timer_ref}, state), do: {:noreply, state}

  def handle_info({:gun_response, _conn_pid, _stream_ref, fin, status, headers}, state) do
    state
    |> push_message({:response, fin, status, headers}, terminal?(fin))
  end

  def handle_info({:gun_data, _conn_pid, _stream_ref, fin, data}, state) do
    state
    |> push_message({:data, fin, data}, terminal?(fin))
  end

  def handle_info({:gun_trailers, _conn_pid, _stream_ref, trailers}, state) do
    state
    |> push_message({:trailers, trailers}, true)
  end

  def handle_info({:gun_error, _conn_pid, _stream_ref, reason}, state) do
    state
    |> push_message({:error, {:stream_error, reason}}, true)
  end

  def handle_info({:gun_error, _conn_pid, reason}, state) do
    state
    |> push_message({:error, {:connection_error, reason}}, true)
  end

  def handle_info({:connection_down, reason}, state) do
    push_message(state, {:error, {:connection_error, reason}}, true)
  end

  def handle_info(msg, state) do
    Logger.warning("#{inspect(__MODULE__)} received unexpected message: #{inspect(msg)}")
    push_message(state, {:error, {:unexpected_message, inspect(msg)}}, true)
  end

  defp push_message(%{done: true} = state, _message, _terminal?) do
    {:noreply, state}
  end

  defp push_message(%{waiter: from} = state, message, terminal?) when not is_nil(from) do
    state = if terminal?, do: cancel_deadline(state), else: state
    GenServer.reply(from, message)
    new_state = %{state | waiter: nil, done: terminal?}

    if terminal? do
      {:stop, :normal, new_state}
    else
      {:noreply, new_state}
    end
  end

  defp push_message(%{messages: messages} = state, message, terminal?) do
    state = if terminal?, do: cancel_deadline(state), else: state
    {:noreply, %{state | messages: :queue.in(message, messages), done: terminal?}}
  end

  defp terminal?(:fin), do: true
  defp terminal?(:nofin), do: false

  defp put_earlier_deadline(%{done: true} = state, _deadline), do: state
  defp put_earlier_deadline(state, :infinity), do: state

  defp put_earlier_deadline(%{deadline: current} = state, deadline)
       when is_integer(deadline) and (current == :infinity or deadline < current) do
    state = cancel_deadline(state)
    timer_ref = make_ref()
    timeout = max(deadline - System.monotonic_time(:millisecond), 0)
    Process.send_after(self(), {:deadline_expired, timer_ref}, timeout)
    %{state | deadline: deadline, deadline_timer: timer_ref}
  end

  defp put_earlier_deadline(state, _deadline), do: state

  defp cancel_deadline(%{deadline_timer: nil} = state), do: state

  defp cancel_deadline(%{deadline_timer: timer_ref} = state) do
    Process.cancel_timer(timer_ref)
    %{state | deadline_timer: nil}
  end
end
