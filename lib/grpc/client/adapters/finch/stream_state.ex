defmodule GRPC.Client.Adapters.Finch.StreamState do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok)
  end

  def add_item(pid, item) do
    IO.inspect(:add_item, label: __MODULE__)
    GenServer.cast(pid, {:add_item, item})
  end

  def close(pid) do
    IO.inspect(:close, label: __MODULE__)
    GenServer.cast(pid, :close)
  end

  def next_item(pid) do
    IO.inspect(:next_item, label: __MODULE__)
    GenServer.call(pid, :next_item)
  end

  @impl true
  def init(:ok) do
    initial_state = %{items: :queue.new(), from: nil, done: false}
    {:ok, initial_state}
  end

  @impl true
  def handle_cast({:add_item, item}, state) do
    new_queue = :queue.in(item, state.items)
    {:noreply, %{state | items: new_queue}, {:continue, :response}}
  end

  @impl true
  def handle_cast(:close, state) do
    new_queue = :queue.in(:close, state.items)
    {:noreply, %{state | items: new_queue, done: true}, {:continue, :response}}
  end

  @impl true
  def handle_call(:next_item, from, state) do
    {:noreply, %{state | from: from}, {:continue, :response}}
  end

  @impl true
  def handle_continue(:response, state) do
    IO.inspect(:handle_continue, label: __MODULE__)
    no_items? = :queue.is_empty(state.items)
    without_from? = is_nil(state.from)

    cond do
      without_from? ->
        {:noreply, state}

      no_items? and state.done ->
        GenServer.reply(state.from, nil)
        {:stop, :normal, state}

      no_items? ->
        {:noreply, state}

      true ->
        case :queue.out(state.items) do
          # The request was complete, we stop the process and return done
          {{:value, item}, new_queue} ->
            IO.inspect("item : #{inspect(item)}", label: __MODULE__)
            GenServer.reply(state.from, item)
            {:noreply, %{state | items: new_queue}}
        end
    end
  end
end
