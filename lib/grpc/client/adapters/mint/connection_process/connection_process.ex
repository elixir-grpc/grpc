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

  def stream_request_body(pid, request_ref, body) do
    GenServer.call(pid, {:stream_body, request_ref, body})
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
        {:request, method, path, headers, :stream, opts},
        _from,
        state
      ) do
    case Mint.HTTP.request(state.conn, method, path, headers, :stream) do
      {:ok, conn, request_ref} ->
        new_state =
          state
          |> State.update_conn(conn)
          |> State.put_in_ref(request_ref, %{
            stream_response_pid: opts[:stream_response_pid],
            done: false,
            response: %{}
          })

        {:reply, {:ok, %{request_ref: request_ref}}, new_state}

      {:error, conn, reason} ->
        new_state = State.update_conn(state, conn)
        {:reply, {:error, reason}, new_state}
    end
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

  def handle_call({:stream_body, request_ref, body}, _from, state) do
    case Mint.HTTP.stream_request_body(state.conn, request_ref, body) do
      {:ok, conn} ->
        {:reply, :ok, State.update_conn(state, conn)}

      {:error, conn, error} ->
        {:reply, {:error, error}, State.update_conn(state, conn)}
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
    empty_headers? = State.empty_headers?(state, request_ref)

    case empty_headers? do
      true ->
        new_state = State.update_response_headers(state, request_ref, headers)
        response = State.get_response(new_state, request_ref)

        new_state
        |> State.caller_process(request_ref)
        |> case do
          nil ->
            new_state
            |> State.stream_response_pid(request_ref)
            |> StreamResponseProcess.consume(:headers, headers)

          ref ->
            GenServer.reply(ref, {:ok, response})
        end

        new_state

      false ->
        state
        |> State.stream_response_pid(request_ref)
        |> StreamResponseProcess.consume(:trailers, headers)

        state
    end
  end

  defp process_response({:data, request_ref, new_data}, state) do
    state
    |> State.stream_response_pid(request_ref)
    |> StreamResponseProcess.consume(:data, new_data)

    state
  end

  defp process_response({:done, request_ref}, state) do
    state
    |> State.stream_response_pid(request_ref)
    |> StreamResponseProcess.done()

    {_ref, new_state} = State.pop_ref(state, request_ref)
    new_state
  end

  @impl true
  def terminate(_reason, _state) do
    :normal
  end
end
