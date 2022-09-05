defmodule GRPC.Client.Adapters.Mint.ConnectionProcess do
  use GenServer

  require Logger

  defstruct [:conn, requests: %{}]

  def start_link(scheme, host, port, opts \\ []) do
    GenServer.start_link(__MODULE__, {scheme, host, port, opts})
  end

  def request(pid, method, path, headers, body, streamed_response?) do
    GenServer.call(pid, {:request, method, path, headers, body, streamed_response?})
  end

  def disconnect(pid) do
    GenServer.call(pid, {:disconnect, :brutal})
  end

  def process_stream_data(pid, request_ref) do
    GenServer.call(pid, {:process_stream_data, request_ref})
  end

  ## Callbacks

  @impl true
  def init({scheme, host, port, opts}) do
    case Mint.HTTP.connect(scheme, host, port, opts) do
      {:ok, conn} ->
        state = %__MODULE__{conn: conn}
        {:ok, state}

      {:error, reason} ->
        # TODO check what's better: add to state map if connection is alive?
        # TODO Or simply stop the process and handle the error on caller?
        {:stop, reason}
    end
  end

  @impl true
  def handle_call({:disconnect, :brutal}, _from, state) do
    # TODO add a code to if disconnect is brutal we just stop if is friendly we wait for pending requests
    Mint.HTTP.close(state.conn)
    {:stop, :normal, :ok, state}
  end

  def handle_call({:process_stream_data, request_ref}, _from, state) do
    case state.requests[request_ref] do
      %{done: true} ->
        {%{response: response, from: _from}, state} = pop_in(state.requests[request_ref])
        {:reply, {response.data, true}, state}

      %{request: %{data: data}} ->
        state = put_in(state.requests[request_ref].response[:data], nil)
        {:reply, {data, false}, state}
    end
  end

  def handle_call(
        {:request, method, path, headers, body, streamed_response?},
        from,
        state
      ) do
    case Mint.HTTP.request(state.conn, method, path, headers, body) do
      {:ok, conn, request_ref} ->
        state = put_in(state.conn, conn)

        state =
          put_in(state.requests[request_ref], %{
            from: from,
            streamed_response: streamed_response?,
            done: false,
            response: %{}
          })

        {:noreply, state}

      {:error, conn, reason} ->
        state = put_in(state.conn, conn)
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_info(message, state) do
    case Mint.HTTP.stream(state.conn, message) do
      :unknown ->
        Logger.debug(fn -> "Received unknown message: " <> inspect(message) end)
        {:noreply, state}

      {:ok, conn, responses} ->
        state = put_in(state.conn, conn)
        state = Enum.reduce(responses, state, &process_response/2)
        {:noreply, state}
    end
  end

  defp process_response({:status, request_ref, status}, state) do
    put_in(state.requests[request_ref].response[:status], status)
  end

  defp process_response({:headers, request_ref, headers}, state) do
    # For server stream connections, we wait for the  headers and accumulate the data.
    if state.requests[request_ref].streamed_response do
      from = state.requests[request_ref].from
      state = put_in(state.requests[request_ref].response[:headers], headers)
      GenServer.reply(from, {:ok, {state.requests[request_ref].response, request_ref}})
      state
    else
      put_in(state.requests[request_ref].response[:headers], headers)
    end
  end

  defp process_response({:data, request_ref, new_data}, state) do
    update_in(state.requests[request_ref].response[:data], fn data -> (data || "") <> new_data end)
  end

  defp process_response({:done, request_ref}, state) do
    if state.requests[request_ref].streamed_response do
      put_in(state.requests[request_ref][:done], true)
    else
      {%{response: response, from: from}, state} = pop_in(state.requests[request_ref])
      GenServer.reply(from, {:ok, response})
      state
    end
  end
end
