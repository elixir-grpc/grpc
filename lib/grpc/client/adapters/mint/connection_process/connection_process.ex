defmodule GRPC.Client.Adapters.Mint.ConnectionProcess do
  @moduledoc false

  #  This module is responsible for managing a connection with a gRPC server.
  #  It's also responsible for managing requests, which also includes checks for the
  #  connection/request window size, splitting a given payload into appropriate sized chunks
  #  and streaming those to the server using an internal queue.

  use GenServer

  alias GRPC.Client.Adapters.Mint.ConnectionProcess.State
  alias GRPC.Client.Adapters.Mint.StreamResponseProcess

  require Logger

  @connection_closed_error "the connection is closed"

  @doc """
  Starts and link connection process
  """
  @spec start_link(Mint.Types.scheme(), Mint.Types.address(), :inet.port_number(), keyword()) ::
          GenServer.on_start()
  def start_link(scheme, host, port, opts \\ []) do
    opts = Keyword.put(opts, :parent, self())
    GenServer.start_link(__MODULE__, {scheme, host, port, opts})
  end

  @doc """
  Sends a request to the connected server.

  ## Options

    * :stream_response_pid (required) - the process to where send the responses coming from the connection will be sent to be processed
  """
  @spec request(
          pid :: pid(),
          method :: String.t(),
          path :: String.t(),
          Mint.Types.headers(),
          body :: iodata() | nil | :stream,
          opts :: keyword()
        ) :: {:ok, %{request_ref: Mint.Types.request_ref()}} | {:error, Mint.Types.error()}
  def request(pid, method, path, headers, body, opts \\ []) do
    GenServer.call(pid, {:request, method, path, headers, body, opts})
  end

  @doc """
  Closes the given connection.
  """
  @spec disconnect(pid :: pid()) :: :ok
  def disconnect(pid) do
    GenServer.call(pid, :disconnect)
  end

  @doc """
  Streams a chunk of the request body on the connection or signals the end of the body.
  """
  @spec stream_request_body(
          pid(),
          Mint.Types.request_ref(),
          iodata() | :eof | {:eof, trailing_headers :: Mint.Types.headers()}
        ) :: :ok | {:error, Mint.Types.error()}
  def stream_request_body(pid, request_ref, body) do
    GenServer.call(pid, {:stream_body, request_ref, body})
  end

  @doc """
  cancels an open request request
  """
  @spec cancel(pid(), Mint.Types.request_ref()) :: :ok | {:error, Mint.Types.error()}
  def cancel(pid, request_ref) do
    GenServer.call(pid, {:cancel_request, request_ref})
  end

  ## Callbacks

  @impl true
  def init({scheme, host, port, opts}) do
    case Mint.HTTP.connect(scheme, host, port, opts) do
      {:ok, conn} ->
        {:ok, State.new(conn, opts[:parent])}

      {:error, reason} ->
        Logger.error(
          "unable to establish a connection to #{scheme}://#{host}:#{port}. reason: #{inspect(reason)}"
        )

        {:stop, reason}
    end
  catch
    :exit, reason ->
      Logger.error(
        "unable to establish a connection to #{scheme}://#{host}:#{port}. reason: #{inspect(reason)}"
      )

      {:stop, reason}
  end

  @impl true
  def handle_call(:disconnect, _from, state) do
    # TODO add a code to if disconnect is brutal we just  stop if is friendly we wait for pending requests
    {:ok, conn} = Mint.HTTP.close(state.conn)
    {:stop, :normal, :ok, State.update_conn(state, conn)}
  end

  def handle_call(_request, _from, %{conn: %Mint.HTTP2{state: :closed}} = state) do
    {:reply, {:error, "the connection is closed"}, state}
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
          |> State.put_empty_ref_state(request_ref, opts[:stream_response_pid])

        {:reply, {:ok, %{request_ref: request_ref}}, new_state}

      {:error, conn, reason} ->
        new_state = State.update_conn(state, conn)
        {:reply, {:error, reason}, new_state}
    end
  end

  def handle_call(
        {:request, method, path, headers, body, opts},
        _from,
        state
      ) do
    case Mint.HTTP.request(state.conn, method, path, headers, :stream) do
      {:ok, conn, request_ref} ->
        queue = :queue.in({request_ref, body, nil}, state.request_stream_queue)

        new_state =
          state
          |> State.update_conn(conn)
          |> State.update_request_stream_queue(queue)
          |> State.put_empty_ref_state(request_ref, opts[:stream_response_pid])

        {:reply, {:ok, %{request_ref: request_ref}}, new_state,
         {:continue, :process_request_stream_queue}}

      {:error, conn, reason} ->
        new_state = State.update_conn(state, conn)
        {:reply, {:error, reason}, new_state}
    end
  end

  def handle_call({:stream_body, request_ref, :eof}, _from, state) do
    case Mint.HTTP.stream_request_body(state.conn, request_ref, :eof) do
      {:ok, conn} ->
        {:reply, :ok, State.update_conn(state, conn)}

      {:error, conn, error} ->
        {:reply, {:error, error}, State.update_conn(state, conn)}
    end
  end

  def handle_call({:stream_body, request_ref, body}, from, state) do
    queue = :queue.in({request_ref, body, from}, state.request_stream_queue)

    {:noreply, State.update_request_stream_queue(state, queue),
     {:continue, :process_request_stream_queue}}
  end

  def handle_call({:cancel_request, request_ref}, _from, state) do
    state = process_response({:done, request_ref}, state)

    case Mint.HTTP2.cancel_request(state.conn, request_ref) do
      {:ok, conn} -> {:reply, :ok, State.update_conn(state, conn)}
      {:error, conn, error} -> {:reply, {:error, error}, State.update_conn(state, conn)}
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

        state =
          case state.requests do
            requests when map_size(requests) == 0 ->
              state

            _ ->
              Enum.reduce(responses, state, &process_response/2)
          end

        check_connection_status(state)

      {:error, conn, _error, _responses} ->
        state = State.update_conn(state, conn)
        check_connection_status(state)
    end
  end

  @impl true
  def handle_continue(:process_request_stream_queue, state) do
    {{:value, request}, queue} = :queue.out(state.request_stream_queue)
    {ref, body, _from} = request
    window_size = get_window_size(state.conn, ref)
    dequeued_state = State.update_request_stream_queue(state, queue)

    cond do
      # Do nothing, wait for server (on stream/2) to give us more window size
      window_size == 0 -> {:noreply, state}
      IO.iodata_length(body) > window_size -> chunk_body_and_enqueue_rest(request, dequeued_state)
      true -> stream_body_and_reply(request, dequeued_state)
    end
  end

  @impl true
  def terminate(_reason, _state) do
    :normal
  end

  defp process_response({:status, request_ref, status}, state) do
    State.update_response_status(state, request_ref, status)
  end

  defp process_response({:headers, request_ref, headers}, state) do
    if State.empty_headers?(state, request_ref) do
      new_state = State.update_response_headers(state, request_ref, headers)

      :ok =
        new_state
        |> State.stream_response_pid(request_ref)
        |> StreamResponseProcess.consume(:headers, headers)

      new_state
    else
      :ok =
        state
        |> State.stream_response_pid(request_ref)
        |> StreamResponseProcess.consume(:trailers, headers)

      state
    end
  end

  defp process_response({:data, request_ref, new_data}, state) do
    :ok =
      state
      |> State.stream_response_pid(request_ref)
      |> StreamResponseProcess.consume(:data, new_data)

    state
  end

  defp process_response({:done, request_ref}, state) do
    :ok =
      state
      |> State.stream_response_pid(request_ref)
      |> StreamResponseProcess.done()

    {_ref, new_state} = State.pop_ref(state, request_ref)
    new_state
  end

  defp chunk_body_and_enqueue_rest({request_ref, body, from}, state) do
    {head, tail} = chunk_body(body, get_window_size(state.conn, request_ref))

    case Mint.HTTP.stream_request_body(state.conn, request_ref, head) do
      {:ok, conn} ->
        queue = :queue.in_r({request_ref, tail, from}, state.request_stream_queue)

        new_state =
          state
          |> State.update_conn(conn)
          |> State.update_request_stream_queue(queue)

        {:noreply, new_state}

      {:error, conn, error} ->
        if from != nil do
          # We need an explicit reply here because the process that called this GenServer
          # isn't the same one that's expecting the reply.
          GenServer.reply(from, {:error, error})
        else
          :ok =
            state
            |> State.stream_response_pid(request_ref)
            |> StreamResponseProcess.consume(:error, error)
        end

        {:noreply, State.update_conn(state, conn)}
    end
  end

  defp stream_body_and_reply({request_ref, body, from}, state) do
    send_eof? = is_nil(from)

    case stream_body(state.conn, request_ref, body, send_eof?) do
      {:ok, conn} ->
        if not send_eof? do
          GenServer.reply(from, :ok)
        end

        check_request_stream_queue(State.update_conn(state, conn))

      {:error, conn, error} ->
        if not send_eof? do
          GenServer.reply(from, {:error, error})
        else
          :ok =
            state
            |> State.stream_response_pid(request_ref)
            |> StreamResponseProcess.consume(:error, error)
        end

        check_request_stream_queue(State.update_conn(state, conn))
    end
  end

  defp stream_body(conn, request_ref, body, true = _stream_eof?) do
    case Mint.HTTP.stream_request_body(conn, request_ref, body) do
      {:ok, conn} -> Mint.HTTP.stream_request_body(conn, request_ref, :eof)
      error -> error
    end
  end

  defp stream_body(conn, request_ref, body, false = _stream_eof?) do
    Mint.HTTP.stream_request_body(conn, request_ref, body)
  end

  def check_request_stream_queue(state) do
    if :queue.is_empty(state.request_stream_queue) do
      {:noreply, state}
    else
      {:noreply, state, {:continue, :process_request_stream_queue}}
    end
  end

  defp chunk_body(body, bytes_length) do
    case body do
      <<head::binary-size(bytes_length), tail::binary>> -> {head, tail}
      _other -> {body, <<>>}
    end
  end

  def get_window_size(conn, ref) do
    min(
      Mint.HTTP2.get_window_size(conn, {:request, ref}),
      Mint.HTTP2.get_window_size(conn, :connection)
    )
  end

  defp finish_all_pending_requests(state) do
    new_state =
      state.request_stream_queue
      |> :queue.to_list()
      |> Enum.reduce(state, fn {request_ref, _, _} = request, acc_state ->
        case request do
          {ref, _body, nil} ->
            acc_state
            |> State.stream_response_pid(ref)
            |> send_connection_close_and_end_stream_response()

          {ref, _body, from} ->
            acc_state
            |> State.stream_response_pid(ref)
            |> send_connection_close_and_end_stream_response()

            GenServer.reply(from, {:error, @connection_closed_error})
        end

        {_request_data, new_state} = State.pop_ref(acc_state, request_ref)
        new_state
      end)

    # Inform the parent that the connection is down
    send(new_state.parent, {:elixir_grpc, :connection_down, self()})

    new_state.requests
    |> Enum.each(fn {ref, _} ->
      new_state
      |> State.stream_response_pid(ref)
      |> send_connection_close_and_end_stream_response()
    end)

    {:noreply, State.update_request_stream_queue(%{new_state | requests: %{}}, :queue.new())}
  end

  defp send_connection_close_and_end_stream_response(pid) do
    :ok = StreamResponseProcess.consume(pid, :error, @connection_closed_error)
    :ok = StreamResponseProcess.done(pid)
  end

  defp check_connection_status(state) do
    if Mint.HTTP.open?(state.conn) do
      check_request_stream_queue(state)
    else
      finish_all_pending_requests(state)
    end
  end
end
