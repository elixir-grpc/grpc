if Code.ensure_loaded?(Mint.HTTP) do
  defmodule GRPC.Client.Adapters.Mint.ConnectionProcess do
    @moduledoc false

    #  This module is responsible for managing a connection with a gRPC server.
    #  It's also responsible for managing requests, which also includes checks for the
    #  connection/request window size, splitting a given payload into appropriate sized chunks
    #  and streaming those to the server using an internal queue.

    use GenServer

    alias GRPC.Client.Adapter
    alias GRPC.Client.Adapters.Mint.ConnectionProcess.State
    alias GRPC.Client.Adapters.Mint.StreamResponseProcess
    alias GRPC.Client.Telemetry

    require Logger
    require State

    @connection_closed_error GRPC.RPCError.exception(
                               GRPC.Status.unavailable(),
                               "the connection is closed"
                             )
    @request_cancelled_error GRPC.RPCError.exception(
                               GRPC.Status.cancelled(),
                               "the request was cancelled"
                             )
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

    If the call exits (the connection process is busy past the call timeout, or
    dead), the exit reason has the headers and body replaced with `:redacted`.

    ## Options

      * :stream_response_pid (required) - the process to where send the responses coming from the connection will be sent to be processed
    """
    def request(pid, method, path, headers, body, opts \\ []) do
      Adapter.call(pid, {:request, method, path, headers, body, opts})
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
      Adapter.call(pid, {:stream_body, request_ref, body})
    end

    @doc """
    cancels an open request request

    `timeout` bounds the wait for the connection process to acknowledge the
    cancellation, for callers that are already past a deadline of their own.
    """
    def cancel(pid, request_ref, timeout \\ 5_000) do
      GenServer.call(pid, {:cancel_request, request_ref}, timeout)
    end

    ## Callbacks

    @impl true
    def init({scheme, host, port, opts}) do
      {retry, opts} = Keyword.pop(opts, :retry, 0)
      {metadata, opts} = Keyword.pop(opts, :grpc_transport_metadata, %{})
      metadata = Map.put(metadata, :transport_ref, self())
      Telemetry.execute(:started, %{generation: 0, active_streams: 0}, metadata)
      {keepalive_opts, opts} = Keyword.split(opts, [:keepalive, :keepalive_tolerance])

      case Mint.HTTP.connect(scheme, host, port, opts) do
        {:ok, conn} ->
          state_opts = [
            parent: opts[:parent],
            scheme: scheme,
            host: host,
            port: port,
            connect_opts: opts,
            retry: retry,
            telemetry_metadata: metadata
          ]

          Telemetry.execute(:connected, %{generation: 1}, Map.put(metadata, :reconnect, false))
          {:ok, conn |> State.new(state_opts ++ keepalive_opts) |> schedule_keepalive()}

        {:error, reason} ->
          Logger.error(
            "unable to establish a connection to #{scheme}://#{host}:#{port}. reason: #{inspect(reason)}"
          )

          connect_failed(metadata, reason)
          {:stop, reason}
      end
    catch
      :exit, reason ->
        Logger.error(
          "unable to establish a connection to #{scheme}://#{host}:#{port}. reason: #{inspect(reason)}"
        )

        metadata = Map.put(opts[:grpc_transport_metadata] || %{}, :transport_ref, self())
        connect_failed(metadata, reason)
        {:stop, reason}
    end

    @impl true
    def handle_call(:disconnect, _from, state) do
      # TODO add a code to if disconnect is brutal we just  stop if is friendly we wait for pending requests
      {:ok, conn} = Mint.HTTP.close(state.conn)
      {:stop, :normal, :ok, State.update_conn(state, conn)}
    end

    def handle_call(_request, _from, %{conn: %Mint.HTTP2{state: :closed}} = state) do
      {:reply, {:error, @connection_closed_error}, state}
    end

    def handle_call(
          {:request, method, path, headers, :stream, opts},
          _from,
          state
        ) do
      case Mint.HTTP.request(state.conn, method, path, headers, :stream) do
        {:ok, conn, request_ref} ->
          stream_response_pid = opts[:stream_response_pid]
          monitor_ref = if is_pid(stream_response_pid), do: Process.monitor(stream_response_pid)

          new_state =
            state
            |> State.update_conn(conn)
            |> State.put_empty_ref_state(request_ref, stream_response_pid, monitor_ref)

          emit_streams(new_state)
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
          stream_response_pid = opts[:stream_response_pid]
          monitor_ref = if is_pid(stream_response_pid), do: Process.monitor(stream_response_pid)

          new_state =
            state
            |> State.update_conn(conn)
            |> State.update_request_stream_queue(queue)
            |> State.put_empty_ref_state(request_ref, stream_response_pid, monitor_ref)

          emit_streams(new_state)

          {:reply, {:ok, %{request_ref: request_ref}}, new_state,
           {:continue, :process_request_stream_queue}}

        {:error, conn, reason} ->
          new_state = State.update_conn(state, conn)
          {:reply, {:error, reason}, new_state}
      end
    end

    def handle_call({:stream_body, request_ref, :eof}, _from, state) do
      if State.has_request_ref(state, request_ref) do
        case Mint.HTTP.stream_request_body(state.conn, request_ref, :eof) do
          {:ok, conn} ->
            {:reply, :ok, State.update_conn(state, conn)}

          {:error, conn, %Mint.HTTPError{reason: :unknown_request_to_stream}} ->
            {:reply, :ok, State.update_conn(state, conn)}

          {:error, conn, error} ->
            {:reply, {:error, error}, State.update_conn(state, conn)}
        end
      else
        {:reply, :ok, state}
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
      maybe_attempt_reconnect(state)
    end

    def handle_info(:keepalive, state) do
      state
      |> schedule_keepalive()
      |> handle_keepalive()
    end

    def handle_info({:DOWN, monitor_ref, :process, pid, reason}, state) do
      case State.find_request_ref_by_monitor_ref(state, monitor_ref) do
        nil ->
          {:noreply, state}

        request_ref ->
          new_state = cancel_dead_stream(state, request_ref, pid, reason)
          {:noreply, new_state}
      end
    end

    def handle_info(message, state) do
      case Mint.HTTP.stream(state.conn, message) do
        :unknown ->
          Logger.debug(fn -> "Received unknown message: " <> inspect(message) end)
          {:noreply, state}

        {:ok, conn, responses} ->
          state =
            state
            |> State.update_conn(conn)
            |> acknowledge_pongs(responses)

          state =
            case state.requests do
              requests when map_size(requests) == 0 ->
                state

              _ ->
                Enum.reduce(responses, state, &process_response/2)
            end

          state = observe_settings(state)
          check_connection_status(state, nil)

        {:error, conn, error, _responses} ->
          state = State.update_conn(state, conn)
          check_connection_status(state, error)
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
    def terminate(reason, state) do
      Telemetry.execute(
        :stopped,
        %{streams_terminated: map_size(state.requests)},
        Map.put(state.telemetry_metadata, :reason, reason)
      )

      :normal
    end

    defp schedule_keepalive(%{keepalive: interval} = state) when is_integer(interval) do
      Process.send_after(self(), :keepalive, interval)
      state
    end

    defp schedule_keepalive(state), do: state

    defp handle_keepalive(%{conn: %Mint.HTTP2{state: :closed}} = state), do: {:noreply, state}

    defp handle_keepalive(%{pings_unacked: unacked, keepalive_tolerance: tolerance} = state)
         when is_integer(tolerance) and unacked >= tolerance do
      Logger.warning(
        "Closing connection to #{state.scheme}://#{state.host}:#{state.port} after #{unacked} unacknowledged keepalive pings"
      )

      close_connection(state, :keepalive_timeout)
    end

    defp handle_keepalive(state) do
      case Mint.HTTP2.ping(state.conn) do
        {:ok, conn, _ref} ->
          {:noreply, %{State.update_conn(state, conn) | pings_unacked: state.pings_unacked + 1}}

        {:error, conn, reason} ->
          Logger.warning(
            "Closing connection to #{state.scheme}://#{state.host}:#{state.port} after failing to send keepalive ping: #{inspect(reason)}"
          )

          state
          |> State.update_conn(conn)
          |> close_connection(reason)
      end
    end

    defp close_connection(state, reason) do
      {:ok, conn} = Mint.HTTP.close(state.conn)

      state
      |> State.update_conn(conn)
      |> finish_all_pending_requests(reason)
    end

    defp acknowledge_pongs(state, responses) do
      pongs = Enum.count(responses, &match?({:pong, _ref}, &1))
      %{state | pings_unacked: state.pings_unacked - pongs}
    end

    # Frames may still arrive for a stream whose state was already dropped
    # (e.g. cancelled after its response process died mid-batch). Ignore them
    # instead of crashing the whole connection.
    defp process_response(response, state)
         when not State.has_request_ref(state, elem(response, 1)) do
      state
    end

    defp process_response({:status, request_ref, 200}, state) do
      State.update_response_status(state, request_ref, 200)
    end

    defp process_response({:status, request_ref, status}, state) do
      state
      |> State.update_response_status(request_ref, status)
      |> consume_or_cancel_stream(
        request_ref,
        :error,
        GRPC.RPCError.exception(GRPC.Status.internal(), "status got is #{status} instead of 200")
      )
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

      drop_request(state, request_ref)
    end

    defp process_response({:done, request_ref}, state) do
      state
      |> State.stream_response_pid(request_ref)
      |> StreamResponseProcess.done()

      drop_request(state, request_ref)
    end

    defp consume_or_cancel_stream(state, request_ref, type, data) do
      pid = State.stream_response_pid(state, request_ref)

      case StreamResponseProcess.consume(pid, type, data) do
        :ok -> state
        :cancel -> cancel_refused_stream(state, request_ref)
        {:error, reason} -> cancel_dead_stream(state, request_ref, pid, reason)
      end
    end

    # The response process refused the response (a message over
    # `:max_message_length`) and has queued the error. Reset the request so the
    # server stops sending, and ignore what is already in flight.
    defp cancel_refused_stream(state, request_ref) do
      state = drop_request(state, request_ref)

      case Mint.HTTP2.cancel_request(state.conn, request_ref) do
        {:ok, conn} -> State.update_conn(state, conn)
        {:error, conn, _error} -> State.update_conn(state, conn)
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

      state = drop_request(state, request_ref)

      case Mint.HTTP2.cancel_request(state.conn, request_ref) do
        {:ok, conn} -> State.update_conn(state, conn)
        {:error, conn, _error} -> State.update_conn(state, conn)
      end
    end

    defp drop_request(state, request_ref) do
      {_ref, state} = State.pop_ref(state, request_ref)
      emit_streams(state)
      drop_queued_request_chunks(state, request_ref)
    end

    defp drop_queued_request_chunks(state, request_ref) do
      {dropped, kept} =
        state.request_stream_queue
        |> :queue.to_list()
        |> Enum.split_with(&match?({^request_ref, _body, _from}, &1))

      for {_ref, _body, from} <- dropped, not is_nil(from) do
        GenServer.reply(from, {:error, @request_cancelled_error})
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

    defp finish_all_pending_requests(state, reason) do
      count = map_size(state.requests)
      metadata = Map.put(state.telemetry_metadata, :reason, reason)
      Telemetry.execute(:down, %{streams_terminated: count}, metadata)

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

      if count > 0, do: emit_streams(clean_state)

      # A failed attempt already scheduled the next retry; ignore duplicate close events.
      if clean_state.retry_attempt > 0 do
        {:noreply, clean_state}
      else
        maybe_attempt_reconnect(clean_state)
      end
    end

    defp end_stream_response(pid, error) do
      StreamResponseProcess.consume(pid, :error, error)
      StreamResponseProcess.done(pid)
    end

    defp maybe_attempt_reconnect(%{retry: 0} = state) do
      send(state.parent, {:elixir_grpc, :connection_down, self()})
      {:stop, :normal, state}
    end

    defp maybe_attempt_reconnect(%{retry_attempt: attempt} = state)
         when State.retries_exhausted?(state) do
      Logger.warning(
        "Connection retry exhausted (#{attempt}/#{state.retry}) for #{state.scheme}://#{state.host}:#{state.port}"
      )

      :telemetry.execute(
        @reconnect_exhausted_event,
        %{},
        reconnect_metadata(state, attempt)
      )

      send(state.parent, {:elixir_grpc, :connection_down, self()})
      {:stop, :normal, state}
    end

    defp maybe_attempt_reconnect(state) do
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

          generation = state.generation + 1

          Telemetry.execute(
            :connected,
            %{generation: generation},
            Map.put(state.telemetry_metadata, :reconnect, true)
          )

          new_state = %{
            state
            | conn: conn,
              retry_attempt: 0,
              generation: generation,
              max_concurrent_streams: :unknown,
              settings_known?: false,
              pings_unacked: 0
          }

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

          maybe_connect_error(state.telemetry_metadata, reason)
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

    defp check_connection_status(state, reason) do
      if Mint.HTTP.open?(state.conn) do
        check_request_stream_queue(state)
      else
        finish_all_pending_requests(state, reason || @connection_closed_error)
      end
    end

    defp observe_settings(state) do
      max =
        normalize_max_streams(Mint.HTTP2.get_server_setting(state.conn, :max_concurrent_streams))

      if not state.settings_known? or max != state.max_concurrent_streams do
        Telemetry.execute(
          :settings,
          %{},
          Map.put(state.telemetry_metadata, :max_concurrent_streams, max)
        )
      end

      %{state | settings_known?: true, max_concurrent_streams: max}
    end

    defp emit_streams(state) do
      Telemetry.execute(:streams, %{active: map_size(state.requests)}, state.telemetry_metadata)
    end

    # Mint 1.x reports 100 both before an explicit peer value and when the peer
    # explicitly advertises 100. Treat it as unknown rather than inventing
    # finite capacity; an upstream SETTINGS-presence hook would remove this gap.
    defp normalize_max_streams(100), do: :unknown
    defp normalize_max_streams(:infinity), do: :infinity
    defp normalize_max_streams(value) when is_integer(value) and value > 0, do: value
    defp normalize_max_streams(_value), do: :unknown

    defp connect_failed(metadata, reason) do
      maybe_connect_error(metadata, reason)
      Telemetry.execute(:stopped, %{streams_terminated: 0}, Map.put(metadata, :reason, reason))
    end

    defp maybe_connect_error(metadata, reason) do
      if stage = connect_stage(reason) do
        Telemetry.execute(
          :connect_error,
          %{},
          Map.merge(metadata, %{stage: stage, reason: reason})
        )
      end
    end

    defp connect_stage(%Mint.TransportError{reason: reason}), do: connect_stage(reason)
    defp connect_stage(%Mint.HTTPError{}), do: :http2
    defp connect_stage(reason) when reason in [:nxdomain, :host_not_found], do: :resolve

    defp connect_stage(reason) when reason in [:econnrefused, :enetunreach, :ehostunreach],
      do: :tcp

    defp connect_stage({:tls_alert, _detail}), do: :tls
    defp connect_stage({_tag, reason}), do: connect_stage(reason)
    defp connect_stage(_reason), do: nil
  end
end
