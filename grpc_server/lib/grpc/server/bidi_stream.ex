defmodule GRPC.Server.BidiStream do
  @moduledoc """
  Manages bidirectional streaming request messages using a supervised Task.

  This module stores incoming request messages for a bidi streaming RPC
  and provides them as a lazy enumerable to the handler. It blocks when
  no messages are available and wakes up when new messages arrive.

  Uses a Task instead of GenServer for lighter weight and automatic supervision.
  """
  require Logger

  @doc """
  Starts a supervised task that manages messages for a bidi stream.
  Returns {:ok, pid}.
  """
  def start_link(stream_id, initial_messages \\ []) do
    Task.Supervisor.start_child(GRPC.Server.StreamTaskSupervisor, fn ->
      loop(%{
        stream_id: stream_id,
        messages: :queue.from_list(initial_messages),
        waiting_caller: nil,
        stream_finished: false
      })
    end)
  end

  @doc """
  Adds decoded messages to the stream.
  """
  def put_messages(pid, messages) when is_list(messages) do
    send(pid, {:put_messages, messages})
    :ok
  end

  @doc """
  Marks the stream as finished (client sent END_STREAM).
  """
  def finish(pid) do
    send(pid, :finish)
    :ok
  end

  @doc """
  Cancels the stream (client sent RST_STREAM).
  """
  def cancel(pid) do
    send(pid, :cancel)
    :ok
  end

  @doc """
  Creates a lazy enumerable that reads messages from this stream.
  Blocks when no messages are available.
  """
  def to_enum(pid) do
    Stream.resource(
      fn -> pid end,
      fn pid ->
        case get_next_message(pid) do
          {:ok, message} -> {[message], pid}
          :done -> {:halt, pid}
        end
      end,
      fn _pid -> :ok end
    )
  end

  ## Private

  defp get_next_message(pid) do
    ref = make_ref()
    send(pid, {:next_message, self(), ref})

    receive do
      {^ref, response} -> response
    end
  end

  defp loop(state) do
    receive do
      {:next_message, caller_pid, ref} ->
        Logger.info(
          "[BidiStream #{state.stream_id}] Received :next_message, queue_size=#{:queue.len(state.messages)}, finished=#{state.stream_finished}"
        )

        case :queue.out(state.messages) do
          {{:value, message}, new_queue} ->
            # Return message immediately
            Logger.debug("[BidiStream #{state.stream_id}] Returning message from queue")
            send(caller_pid, {ref, {:ok, message}})
            loop(%{state | messages: new_queue})

          {:empty, _} ->
            if state.stream_finished do
              # No more messages and stream is done
              Logger.debug("[BidiStream #{state.stream_id}] Stream finished, no more messages")
              send(caller_pid, {ref, :done})
              # Exit the task - stream is complete
              :ok
            else
              # No messages yet - store caller and wait
              Logger.debug("[BidiStream #{state.stream_id}] Queue empty, blocking caller")
              loop(%{state | waiting_caller: {caller_pid, ref}})
            end
        end

      {:put_messages, new_messages} ->
        Logger.info(
          "[BidiStream #{state.stream_id}] Received #{length(new_messages)} new messages"
        )

        # Add messages to queue
        new_queue =
          Enum.reduce(new_messages, state.messages, fn msg, queue ->
            :queue.in(msg, queue)
          end)

        # If someone is waiting, reply with first message
        Logger.debug(
          "[BidiStream #{state.stream_id}] After adding messages, queue_size=#{:queue.len(new_queue)}, has_waiting_caller=#{not is_nil(state.waiting_caller)}"
        )

        if state.waiting_caller do
          {caller_pid, ref} = state.waiting_caller

          case :queue.out(new_queue) do
            {{:value, message}, final_queue} ->
              send(caller_pid, {ref, {:ok, message}})
              loop(%{state | messages: final_queue, waiting_caller: nil})

            {:empty, _} ->
              # Shouldn't happen but handle gracefully
              loop(%{state | messages: new_queue})
          end
        else
          loop(%{state | messages: new_queue})
        end

      :finish ->
        Logger.info(
          "[BidiStream #{state.stream_id}] Received :finish, has_waiting_caller=#{not is_nil(state.waiting_caller)}"
        )

        # Mark stream as finished
        if state.waiting_caller do
          {caller_pid, ref} = state.waiting_caller
          # Reply to waiting caller that stream is done
          send(caller_pid, {ref, :done})
          # Exit the task - stream is complete
          :ok
        else
          loop(%{state | stream_finished: true})
        end

      :cancel ->
        Logger.info("[BidiStream #{state.stream_id}] Received :cancel (RST_STREAM from client)")
        # If someone is waiting, reply that stream is done
        if state.waiting_caller do
          {caller_pid, ref} = state.waiting_caller
          send(caller_pid, {ref, :done})
        end

        # Exit the task - stream was cancelled
        :ok
    end
  end
end
