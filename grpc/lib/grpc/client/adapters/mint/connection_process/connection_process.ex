if Code.ensure_loaded?(Mint.HTTP) do
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
    require State

    @connection_closed_error "the connection is closed"
    @stream_response_dead_event [:grpc, :client, :mint, :stream_response, :dead]
    @reconnect_stop_event [:grpc, :client, :mint, :reconnect, :stop]
    @reconnect_error_event [:grpc, :client, :mint, :reconnect, :error]
    @reconnect_exhausted_event [:grpc, :client, :mint, :reconnect, :exhausted]

    @doc """
    Starts and link connection process
    """
    def start_link(scheme, host, port, opts \\ []) do
      opts = Keyword.put(opts, :parent, self())
      GenServer.start_link(__MODULE__, {scheme, host, port, opts})
    end

    @doc """
    Sends a request to the connected server.

    ## Options

      * :stream_response_pid (required) - the process to where send the responses coming from the connection will be sent to be processed
    """
    def request(pid, method, path, headers, body, opts \\ []) do
      GenServer.call(pid, {:request, method, path, headers, body, opts})
    end

    @doc """
    Closes the given connection.
    """
    def disconnect(pid) do
      GenServer.call(pid, :disconnect)
    end

    @doc """
    Streams a chunk of the request body on the connection or signals the end of the body.
    """
    def stream_request_body(pid, request_ref, body) do
      GenServer.call(pid, {:stream_body, request_ref, body})
    end

    @doc """
    cancels an open request request
    """
    def cancel(pid, request_ref) do
      GenServer.call(pid, {:cancel_request, request_ref})
    end

    ## Callbacks

    @impl true
    def init({scheme, host, port, opts}) do
      {retry, opts} = Keyword.pop(opts, :retry, 0)

      case Mint.HTTP.connect(scheme, host, port, opts) do
        {:ok, conn} ->
          state_opts = [
            parent: opts[:parent],
            scheme: scheme,
            host: host,
            port: port,
            connect_opts: opts,
            retry: retry
          ]

          {:ok, State.new(conn, state_opts)}

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

    def handle_call({:cancel_request, request_ref}, _from, state)
        when State.has_request_ref(state, request_ref) do
      state = process_response({:done, request_ref}, state)

      case Mint.HTTP2.cancel_request(state.conn, request_ref) do
        {:ok, conn} -> {:reply, :ok, State.update_conn(state, conn)}
        {:error, conn, error} -> {:reply, {:error, error}, State.update_conn(state, conn)}
      end
    end

    def handle_call({:cancel_request, _request_ref}, _from, state) do
      {:reply, :ok, state}
    end

    @impl true
    def handle_info(:reconnect, state) do
      attempt_reconnect(state)
    end

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
        window_size == 0 ->
          {:noreply, state}

        IO.iodata_length(body) > window_size ->
          chunk_body_and_enqueue_rest(request, dequeued_state)

        true ->
          stream_body_and_reply(request, dequeued_state)
      end
    end

    @impl true
    def terminate(_reason, _state) do
      :normal
    end

    # Frames may still arrive for a stream whose state was already dropped
    # (e.g. cancelled after its response process died mid-batch). Ignore them
    # instead of crashing the whole connection.
    defp process_response(response, state)
         when not State.has_request_ref(state, elem(response, 1)) do
      state
    end

    defp process_response({:status, request_ref, status}, state) do
      State.update_response_status(state, request_ref, status)
    end

    defp process_response({:headers, request_ref, headers}, state) do
      if State.empty_headers?(state, request_ref) do
        new_state = State.update_response_headers(state, request_ref, headers)
        consume_or_cancel_stream(new_state, request_ref, :headers, headers)
      else
        consume_or_cancel_stream(state, request_ref, :trailers, headers)
      end
    end

    defp process_response({:data, request_ref, new_data}, state) do
      consume_or_cancel_stream(state, request_ref, :data, new_data)
    end

    defp process_response({:error, request_ref, reason}, state) do
      state
      |> State.stream_response_pid(request_ref)
      |> end_stream_response(reason)

      {_ref, new_state} = State.pop_ref(state, request_ref)
      new_state
    end

    defp process_response({:done, request_ref}, state) do
      state
      |> State.stream_response_pid(request_ref)
      |> StreamResponseProcess.done()

      {_ref, new_state} = State.pop_ref(state, request_ref)
      new_state
    end

    defp consume_or_cancel_stream(state, request_ref, type, data) do
      pid = State.stream_response_pid(state, request_ref)

      case StreamResponseProcess.consume(pid, type, data) do
        :ok -> state
        {:error, reason} -> cancel_dead_stream(state, request_ref, pid, reason)
      end
    end

    defp cancel_dead_stream(state, request_ref, pid, reason) do
      Logger.warning(
        "stream response process #{inspect(pid)} is not alive (#{inspect(reason)}), cancelling request #{inspect(request_ref)}"
      )

      :telemetry.execute(@stream_response_dead_event, %{}, %{
        request_ref: request_ref,
        stream_response_pid: pid,
        reason: reason
      })

      {_ref, state} = State.pop_ref(state, request_ref)
      state = drop_queued_request_chunks(state, request_ref)

      case Mint.HTTP2.cancel_request(state.conn, request_ref) do
        {:ok, conn} -> State.update_conn(state, conn)
        {:error, conn, _error} -> State.update_conn(state, conn)
      end
    end

    defp drop_queued_request_chunks(state, request_ref) do
      {dropped, kept} =
        state.request_stream_queue
        |> :queue.to_list()
        |> Enum.split_with(&match?({^request_ref, _body, _from}, &1))

      for {_ref, _body, from} <- dropped, not is_nil(from) do
        GenServer.reply(from, {:error, "the request was cancelled"})
      end

      State.update_request_stream_queue(state, :queue.from_list(kept))
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
        <<head::binary-size(^bytes_length), tail::binary>> -> {head, tail}
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
              |> end_stream_response(@connection_closed_error)

            {ref, _body, from} ->
              acc_state
              |> State.stream_response_pid(ref)
              |> end_stream_response(@connection_closed_error)

              GenServer.reply(from, {:error, @connection_closed_error})
          end

          {_request_data, new_state} = State.pop_ref(acc_state, request_ref)
          new_state
        end)

      new_state.requests
      |> Enum.each(fn {ref, _} ->
        new_state
        |> State.stream_response_pid(ref)
        |> end_stream_response(@connection_closed_error)
      end)

      clean_state = State.update_request_stream_queue(%{new_state | requests: %{}}, :queue.new())

      if clean_state.retry > 0 do
        attempt_reconnect(clean_state)
      else
        send(clean_state.parent, {:elixir_grpc, :connection_down, self()})
        {:noreply, clean_state}
      end
    end

    defp end_stream_response(pid, error) do
      StreamResponseProcess.consume(pid, :error, error)
      StreamResponseProcess.done(pid)
    end

    defp attempt_reconnect(%{retry: max, retry_attempt: attempt} = state)
         when attempt >= max do
      Logger.warning(
        "Connection retry exhausted (#{attempt}/#{max}) for #{state.scheme}://#{state.host}:#{state.port}"
      )

      :telemetry.execute(
        @reconnect_exhausted_event,
        %{},
        reconnect_metadata(state, attempt)
      )

      send(state.parent, {:elixir_grpc, :connection_down, self()})
      {:noreply, state}
    end

    defp attempt_reconnect(state) do
      next_attempt = state.retry_attempt + 1

      Logger.info(
        "Attempting reconnection #{next_attempt}/#{state.retry} to #{state.scheme}://#{state.host}:#{state.port}"
      )

      start = System.monotonic_time()

      case Mint.HTTP.connect(state.scheme, state.host, state.port, state.connect_opts) do
        {:ok, conn} ->
          Logger.info("Reconnected successfully to #{state.scheme}://#{state.host}:#{state.port}")

          :telemetry.execute(
            @reconnect_stop_event,
            %{duration: System.monotonic_time() - start},
            reconnect_metadata(state, next_attempt)
          )

          new_state = %{state | conn: conn, retry_attempt: 0}
          {:noreply, new_state}

        {:error, reason} ->
          Logger.warning(
            "Reconnection attempt #{next_attempt}/#{state.retry} failed: #{inspect(reason)}"
          )

          :telemetry.execute(
            @reconnect_error_event,
            %{duration: System.monotonic_time() - start},
            Map.put(reconnect_metadata(state, next_attempt), :reason, reason)
          )

          timeout = state.retry_timeout_ms || retry_timeout(next_attempt)
          Process.send_after(self(), :reconnect, timeout)
          {:noreply, %{state | retry_attempt: next_attempt}}
      end
    end

    defp reconnect_metadata(state, attempt) do
      %{
        scheme: state.scheme,
        host: state.host,
        port: state.port,
        attempt: attempt,
        max: state.retry
      }
    end

    @doc false
    def retry_timeout(attempt) do
      timeout =
        if attempt < 11 do
          :math.pow(1.6, attempt - 1) * 1000
        else
          120_000
        end

      jitter = (:rand.uniform_real() - 0.5) / 2.5
      round(timeout + jitter * timeout)
    end

    defp check_connection_status(state) do
      if Mint.HTTP.open?(state.conn) do
        check_request_stream_queue(state)
      else
        finish_all_pending_requests(state)
      end
    end
  end
end
