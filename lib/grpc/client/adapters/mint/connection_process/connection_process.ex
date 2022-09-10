defmodule GRPC.Client.Adapters.Mint.ConnectionProcess do
  use GenServer

  alias GRPC.Client.Adapters.Mint.ConnectionProcess.State
  alias GRPC.Client.Adapters.Mint.StreamResponseProcess

  require Logger

  def start_link(scheme, host, port, opts \\ []) do
    GenServer.start_link(__MODULE__, {scheme, host, port, opts})
  end

  def request(pid, method, path, headers, body, opts \\ []) do
    GenServer.call(pid, {:request, method, path, headers, body, opts})
  end

  def disconnect(pid) do
    GenServer.call(pid, {:disconnect, :brutal})
  end

  ## Callbacks

  @impl true
  def init({scheme, host, port, opts}) do
    # The current behavior in gun is return error if the connection wasn't successful
    # Should we do the same here?
    case Mint.HTTP.connect(scheme, host, port, opts) do
      {:ok, conn} ->
        {:ok, State.new(conn)}

      {:error, reason} ->
        # TODO check what's better: add to state map if connection is alive?
        # TODO Or simply stop the process and handle the error on caller?
        {:stop, reason}
    end
  end

  @impl true
  def handle_call({:disconnect, :brutal}, _from, state) do
    # TODO add a code to if disconnect is brutal we just stop if is friendly we wait for pending requests
    {:stop, :normal, Mint.HTTP.close(state.conn), state}
  end

  def handle_call(
        {:request, method, path, headers, body, opts},
        from,
        state
      ) do
    case Mint.HTTP.request(state.conn, method, path, headers, body) do
      {:ok, conn, request_ref} ->
        new_state =
          state
          |> State.update_conn(conn)
          |> State.put_in_ref(request_ref, %{
            from: from,
            streamed_response: opts[:streamed_response] || false,
            stream_response_pid: opts[:stream_response_pid],
            done: false,
            response: %{}
          })

        {:noreply, new_state}

      {:error, conn, reason} ->
        new_state = State.update_conn(state, conn)
        {:reply, {:error, reason}, new_state}
    end
  end

  @impl true
  def handle_info(message, state) do
    case Mint.HTTP.stream(state.conn, message) do
      :unknown ->
        Logger.debug(fn -> "Received unknown message: " <> inspect(message) end)
        {:noreply, state}

      {:ok, conn, responses} ->
        state = State.update_conn(state, conn)
        state = Enum.reduce(responses, state, &process_response/2)
        {:noreply, state}
    end
  end

  defp process_response({:status, request_ref, status}, state) do
    State.update_response_status(state, request_ref, status)
  end

  defp process_response({:headers, request_ref, headers}, state) do
    streamed_response? = State.steamed_response?(state, request_ref)
    empty_headers? = State.empty_headers?(state, request_ref)

    # For server streamed connections, we wait until the response headers to reply the caller process.
    # If we have headers set in state, it means that the response headers already arrived,
    # so incoming headers message it's referring to trailers headers
    cond do
      streamed_response? and empty_headers? ->
        new_state = State.update_response_headers(state, request_ref, headers)
        response = State.get_response(state, request_ref)

        state
        |> State.caller_process(request_ref)
        |> GenServer.reply({:ok, response})

        new_state

      empty_headers? ->
        State.update_response_headers(state, request_ref, headers)

      streamed_response? ->
        state
        |> State.stream_response_pid(request_ref)
        |> StreamResponseProcess.consume(:trailers, headers)

        state

      true ->
        State.update_response_trailers(state, request_ref, headers)
    end
  end

  defp process_response({:data, request_ref, new_data}, state) do
    if(State.steamed_response?(state, request_ref)) do
      state
      |> State.stream_response_pid(request_ref)
      |> StreamResponseProcess.consume(:data, new_data)

      state
    else
      State.append_response_data(state, request_ref, new_data)
    end
  end

  defp process_response({:done, request_ref}, state) do
    if State.steamed_response?(state, request_ref) do
      state
      |> State.stream_response_pid(request_ref)
      |> StreamResponseProcess.done()

      {_ref, new_state} = State.pop_ref(state, request_ref)
      new_state
    else
      {%{response: response, from: from}, state} = State.pop_ref(state, request_ref)
      GenServer.reply(from, {:ok, response})
      state
    end
  end

  @impl true
  def terminate(_reason, _state) do
   :normal
  end
end
