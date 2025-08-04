defmodule Grpc.Client.Adapters.Finch.StreamRequestProcess do
  use GenServer

  alias Grpc.Client.Adapters.Finch.RequestProcess

  def start_link(path, client_headers, data \\ nil) do
    GenServer.start_link(__MODULE__, [path, client_headers, data])
  end

  def close(pid) do
    GenServer.call(pid, :close)
  end

  def next_response(pid) do
    GenServer.call(pid, :next_response)
  end

  def consume(pid, msg) do
    GenServer.cast(pid, {:consume, msg})
  end

  @impl true
  def init([path, client_headers, data]) do
    request_process = RequestProcess.start_link(self(), path, client_headers, data)

    {:ok,
     %{
       request_process: request_process,
       responses: :queue.new(),
       from: nil
     }}
  end

  @impl true
  def handle_call(:next_response, from, state) do
    {:noreply, %{state | from: from}, {:continue, :produce_response}}
  end

  @impl true
  def handle_call(:close, from, state) do
    IO.inspect(state.responses, label: "Closing stream request")
    responses = :queue.in(:done, state.responses)
    {:noreply, %{state | responses: responses, from: from}, {:continue, :produce_response}}
  end

  @impl true
  def handle_cast({:consume, msg}, state) do
    IO.inspect(:consume, label: __MODULE__)
    responses = :queue.in(msg, state.responses)
    {:noreply, %{state | responses: responses}, {:continue, :produce_response}}
  end

  @impl true
  def handle_continue(:produce_response, state) do
    no_responses? = :queue.is_empty(state.responses)
    without_from? = is_nil(state.from)

    cond do
      without_from? ->
        {:noreply, state}

      no_responses? ->
        {:noreply, state}

      true ->
        case :queue.out(state.responses) do
          # Return the new item
          {{:value, item}, new_queue} ->
            GenServer.reply(state.from, item)
            {:noreply, %{state | responses: new_queue, from: nil}}
        end
    end
  end
end
