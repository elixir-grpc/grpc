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

  @type state :: %{
          messages: :queue.queue(),
          waiter: {GenServer.from(), reference() | nil} | nil,
          done: boolean()
        }

  @spec start_link() :: GenServer.on_start()
  def start_link do
    GenServer.start_link(__MODULE__, [])
  end

  @spec await(pid(), timeout()) :: tuple()
  def await(pid, timeout) do
    GenServer.call(pid, {:await, timeout}, :infinity)
  catch
    :exit, _reason ->
      @terminated_stream_error
  end

  @impl GenServer
  def init([]) do
    {:ok, %{messages: :queue.new(), waiter: nil, done: false}}
  end

  @impl GenServer
  def handle_call({:await, timeout}, from, %{messages: messages, done: done?} = state) do
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
          {:noreply, %{state | waiter: {from, start_timeout(timeout)}}}
        end
    end
  end

  @impl GenServer
  def handle_info({:await_timeout, timeout_ref}, %{waiter: {from, timeout_ref}} = state) do
    GenServer.reply(from, {:error, :timeout})
    {:noreply, %{state | waiter: nil}}
  end

  def handle_info({:await_timeout, _timeout_ref}, state), do: {:noreply, state}

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

  def handle_info({:connection_down, reason}, state) do
    push_message(state, {:error, {:connection_error, reason}}, true)
  end

  def handle_info(msg, state) do
    Logger.warning("#{inspect(__MODULE__)} received unexpected message: #{inspect(msg)}")
    push_message(state, {:error, {:unexpected_message, inspect(msg)}}, true)
  end

  defp push_message(%{waiter: {from, timeout_ref}} = state, message, terminal?) do
    cancel_timeout(timeout_ref)
    GenServer.reply(from, message)
    new_state = %{state | waiter: nil, done: state.done or terminal?}

    if terminal? do
      {:stop, :normal, new_state}
    else
      {:noreply, new_state}
    end
  end

  defp push_message(%{messages: messages} = state, message, terminal?) do
    {:noreply, %{state | messages: :queue.in(message, messages), done: state.done or terminal?}}
  end

  defp terminal?(:fin), do: true
  defp terminal?(:nofin), do: false

  defp start_timeout(:infinity), do: nil

  defp start_timeout(timeout) when is_integer(timeout) do
    timeout_ref = make_ref()
    Process.send_after(self(), {:await_timeout, timeout_ref}, timeout)
    timeout_ref
  end

  defp cancel_timeout(nil), do: :ok

  defp cancel_timeout(timeout_ref) do
    Process.cancel_timer(timeout_ref)
    :ok
  end
end
