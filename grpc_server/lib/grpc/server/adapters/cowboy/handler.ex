defmodule GRPC.Server.Adapters.Cowboy.Handler do
  @moduledoc """
  A cowboy handler accepting all requests and calls corresponding functions defined by users.
  """

  alias GRPC.Server.Adapters.ReportException
  alias GRPC.Transport.HTTP2
  alias GRPC.RPCError
  require Logger

  @behaviour :cowboy_loop

  @adapter GRPC.Server.Adapters.Cowboy
  @default_trailers HTTP2.server_trailers()
  @trailers_flag 0b1000_0000

  @abort_published :"$grpc_abort_published"

  # 4 MB – matches gRPC-Go's default max receive message size.
  # Override per-server with the :max_body_size option (bytes).
  @default_max_body_size 4 * 1024 * 1024
  # Cowboy's own default for how much `read_body` collects before it returns.
  @default_read_length 8_000_000

  @doc """
  This function is meant to be called whenever a new request arrives to an existing connection.
  This handler works mainly with two linked processes.
  One of them is the process started by cowboy which internally we'll refer to it as `stream_pid`,
  this process is responsible to interface the interactions with the open socket.
  The second process is the one we start in this function, we'll refer to it as `server_rpc_pid`,
  which is the point where we call the functions implemented by users (aka the modules who use
  the `GRPC.Server` macro)
  """
  def init(req, {endpoint, {_name, server}, route, opts} = state) do
    http_method = extract_http_method(req)
    exception_log_filter = extract_exception_log_filter_opt(opts)

    with {:ok, access_mode, sub_type, content_type} <- find_content_type_subtype(req, opts),
         {:ok, codec} <- find_codec(sub_type, content_type, server),
         {:ok, compressor} <- find_compressor(req, server) do
      stream_pid = self()
      started_at = System.monotonic_time()
      http_transcode = access_mode == :http_transcoding
      request_headers = :cowboy_req.headers(req)

      # Decode the inbound deadline ONCE, before building the stream, so it can be
      # surfaced to interceptors/handlers and reused for the cancellation timer.
      timeout_ms =
        case Map.get(request_headers, "grpc-timeout") do
          t when is_binary(t) -> GRPC.Transport.Utils.decode_timeout(t)
          _ -> nil
        end

      deadline = timeout_ms && System.monotonic_time(:millisecond) + timeout_ms

      stream = %GRPC.Server.Stream{
        server: server,
        endpoint: endpoint,
        adapter: @adapter,
        payload: %{pid: stream_pid, send_timeout: opts[:send_timeout], deadline: deadline},
        local: opts[:local],
        codec: codec,
        http_method: http_method,
        http_request_headers: request_headers,
        http_transcode: http_transcode,
        compressor: compressor,
        is_preflight?: preflight?(req),
        access_mode: access_mode,
        deadline: deadline
      }

      server_rpc_pid = :proc_lib.spawn_link(__MODULE__, :call_rpc, [server, route, stream])
      Process.flag(:trap_exit, true)

      req = :cowboy_req.set_resp_headers(HTTP2.server_headers(stream), req)

      timer_ref =
        if timeout_ms do
          Process.send_after(self(), {:handling_timeout, self()}, timeout_ms)
        end

      max_body_size = Map.get(opts, :max_body_size, @default_max_body_size)

      {
        :cowboy_loop,
        req,
        %{
          pid: server_rpc_pid,
          stream: stream,
          endpoint: endpoint,
          route: route,
          started_at: started_at,
          handling_timer: timer_ref,
          pending_reader: nil,
          access_mode: access_mode,
          codec: codec,
          exception_log_filter: exception_log_filter,
          max_body_size: max_body_size,
          body_bytes_read: 0,
          # Position in the gRPC framing of the request body: the bytes left in the
          # current message, and the part of the next 5-byte prefix seen so far.
          frame_remaining: 0,
          frame_prefix: <<>>
        }
      }
    else
      {:error, error} ->
        Logger.error(fn -> inspect(error) end)
        trailers = HTTP2.server_trailers(error.status, error.message)
        req = send_error_trailers(req, 200, trailers, state)
        {:ok, req, state}
    end
  end

  defp extract_exception_log_filter_opt(opts) do
    case opts[:exception_log_filter] do
      {module, func_name} when is_atom(module) and is_atom(func_name) ->
        {module, func_name}

      nil ->
        nil

      invalid ->
        raise ArgumentError, "invalid exception log filter: #{inspect(invalid)}"
    end
  end

  defp find_codec(subtype, content_type, server) do
    if codec = Enum.find(server.__meta__(:codecs), nil, fn c -> c.name() == subtype end) do
      {:ok, codec}
    else
      {:error,
       RPCError.exception(
         status: :unimplemented,
         message: "No codec registered for content-type #{content_type}"
       )}
    end
  end

  defp find_content_type_subtype(req, opts) do
    content_type =
      case :cowboy_req.header("content-type", req) do
        :undefined ->
          :cowboy_req.header("accept", req)

        content_type ->
          content_type
      end

    http_method = extract_http_method(req)

    {:ok, access_mode, subtype} =
      case extract_subtype(content_type) do
        {:ok, :unknown, "unknown"} ->
          if http_method == :post do
            {:ok, :grpc, "proto"}
          else
            {:ok, :http_transcoding, "json"}
          end

        resp ->
          resp
      end

    access_mode = resolve_access_mode(req, access_mode, subtype)
    {access_mode, subtype} = pin_transcoded_route(opts, req, access_mode, subtype)
    {:ok, access_mode, subtype, content_type}
  end

  # A route from an HttpRule is always transcoded, whatever the Content-Type says.
  # Only the transcoding path applies the route's path bindings, so a request that
  # chose another path with its header could set a field the URL is meant to fix
  # (GHSA-xc5v-89vq-hv5m). A CORS preflight stays a preflight.
  defp pin_transcoded_route(%{http_transcode_route: true}, req, access_mode, subtype) do
    if preflight?(req), do: {access_mode, subtype}, else: {:http_transcoding, "json"}
  end

  defp pin_transcoded_route(_opts, _req, access_mode, subtype), do: {access_mode, subtype}

  defp extract_http_method(req) do
    case :cowboy_req.method(req) do
      "POST" -> :post
      "GET" -> :get
      "PUT" -> :put
      "DELETE" -> :delete
      "HEAD" -> :head
      "OPTIONS" -> :options
      "PATCH" -> :patch
      other -> String.to_existing_atom(String.downcase(other))
    end
  end

  defp find_compressor(req, server) do
    encoding = :cowboy_req.header("grpc-encoding", req)

    if is_binary(encoding) do
      compressor = Enum.find(server.__meta__(:compressors), nil, fn c -> c.name() == encoding end)

      if compressor do
        {:ok, compressor}
      else
        {:error,
         RPCError.exception(
           status: :unimplemented,
           message: "Not found compressor registered for grpc-encoding #{encoding}"
         )}
      end
    else
      {:ok, nil}
    end
  end

  # APIs begin
  @doc """
  Synchronously reads the whole body content of a given request.
  Raise in case of a timeout.
  """
  def read_full_body(pid) do
    sync_call(pid, :read_full_body)
  end

  @doc """
  Synchronously reads a chunk of body content of a given request.
  Raise in case of a timeout.
  """
  def read_body(pid) do
    sync_call(pid, :read_body)
  end

  @doc """
  Send back to client a chunk of `data`, when `http_transcode?` is true, the
  data is sent back as it's, with no transformation of protobuf binaries to http2 data frames.

  Returns once the connection has taken the chunk, so a client that reads slowly slows the
  caller. Waits at most `opts[:send_timeout]` (default `:infinity`), then raises a
  `:deadline_exceeded` `GRPC.RPCError`. Exits with `:shutdown` if the stream's process is gone.

  The reply is already queued when the wait ends, so it is still sent later.
  """
  def stream_body(pid, data, opts, is_fin, http_transcode? \\ false) do
    monitor = Process.monitor(pid)
    # The stream process sends the ack to the alias. Dropping the alias on a timeout
    # keeps a late ack out of the caller's mailbox.
    reply_alias = :erlang.alias([:reply])
    send(pid, {:stream_body, data, opts, is_fin, http_transcode?, reply_alias})
    await_stream_body_ack(reply_alias, monitor, Keyword.get(opts, :send_timeout, :infinity))
  end

  @doc """
  How long a send may wait for the connection: the `:send_timeout` the server was started with,
  or the time left to the RPC's deadline, whichever is shorter.

  Raises a `:deadline_exceeded` `GRPC.RPCError` if the deadline has already passed. The call
  is expired then, and an ack can't arrive in no time, so no reply is queued.
  """
  def send_timeout(payload) do
    configured = Map.get(payload, :send_timeout) || :infinity

    case Map.get(payload, :deadline) do
      nil ->
        configured

      deadline ->
        left = deadline - System.monotonic_time(:millisecond)

        if left <= 0 do
          raise RPCError, status: :deadline_exceeded, message: "Deadline expired"
        end

        if configured == :infinity, do: left, else: min(configured, left)
    end
  end

  @doc """
  Asynchronously send back to the client the http status and the headers for a given request.
  """
  def stream_reply(pid, status, headers) do
    send(pid, {:stream_reply, status, headers})
    :ok
  end

  @doc """
  Asynchronously set the headers for a given request. This function does not send any
  data back to the client. It simply appends the headers to be used in the response.
  """
  def set_resp_headers(pid, headers) do
    send(pid, {:set_resp_headers, headers})
    :ok
  end

  @doc """
  Asynchronously set the trailer headers for a given request. This function does not send any
  data back to the client. It simply appends the trailer headers to be used in the response.
  """
  def set_resp_trailers(pid, trailers) do
    send(pid, {:set_resp_trailers, trailers})
    :ok
  end

  @doc """
  Asynchronously set the compressor algorithm to be used for compress the responses. This checks if
  the `grpc-accept-encoding` header is present on the original request, otherwise no compression
  is applied.
  """
  def set_compressor(pid, compressor) do
    send(pid, {:set_compressor, compressor})
    :ok
  end

  @doc """
  Asynchronously stream the given trailers of request back to client.
  """
  def stream_trailers(pid, trailers) do
    send(pid, {:stream_trailers, trailers})
    :ok
  end

  @doc """
  Return all request headers.
  """
  def get_headers(pid) do
    sync_call(pid, :get_headers)
  end

  @doc """
  Return the peer IP address and port number
  """
  def get_peer(pid) do
    sync_call(pid, :get_peer)
  end

  @doc """
  Return the client TLS certificate. `:undefined` is returned if no certificate was specified
  when establishing the connection.
  """
  def get_cert(pid) do
    sync_call(pid, :get_cert)
  end

  @doc """
  Return the query string for the request URI.
  """
  def get_qs(pid) do
    sync_call(pid, :get_qs)
  end

  @doc """
  Return all bindings of a given request.
  """
  def get_bindings(pid) do
    sync_call(pid, :get_bindings)
  end

  # The stream process acks after cowboy has taken the data, and cowboy withholds that
  # while its send buffer is full. A `:reply` alias is off once the ack arrives. On a timeout
  # or a `:DOWN`, turn it off, then drop an ack that came before that.
  defp await_stream_body_ack(reply_alias, monitor, timeout) do
    receive do
      [^reply_alias | :stream_body_ack] ->
        Process.demonitor(monitor, [:flush])
        :ok

      {:DOWN, ^monitor, :process, _pid, _reason} ->
        drop_stream_body_ack(reply_alias)
        exit(:shutdown)
    after
      timeout ->
        Process.demonitor(monitor, [:flush])
        drop_stream_body_ack(reply_alias)

        raise RPCError,
          status: :deadline_exceeded,
          message: "Timed out waiting for the client to read a reply"
    end
  end

  defp drop_stream_body_ack(reply_alias) do
    :erlang.unalias(reply_alias)

    receive do
      [^reply_alias | :stream_body_ack] -> :ok
    after
      0 -> :ok
    end
  end

  defp sync_call(pid, key) do
    ref = make_ref()
    send(pid, {key, ref, self()})

    receive do
      {^ref, msg} -> msg
    end
  end

  # APIs end

  def info({:read_full_body, ref, pid}, req, state) do
    limits = %{max_bytes: state.max_body_size, check_prefix?: state.access_mode == :grpc}
    {s, body, req} = read_full_body(req, [], 0, state[:handling_timer], limits)
    send(pid, {ref, {s, body}})
    {:ok, req, state}
  catch
    :exit, :timeout ->
      Logger.warning("Timeout when reading full body")
      info({:handling_timeout, self()}, req, state)

    :throw, {:body_too_large, _size} ->
      Logger.warning("Request body exceeded max_body_size (#{state.max_body_size} bytes)")
      error = RPCError.exception(status: :resource_exhausted, message: "Request body too large")
      req = send_error(req, error, state, :body_too_large)
      {:stop, req, state}

    :throw, {:message_too_large, error} ->
      message_too_large(req, state, error)
  end

  def info({:read_body, ref, pid}, req, state) do
    opts = read_length_opt(state, timeout_left_opt(state[:handling_timer]))

    case async_read_body(req, opts) do
      {:send, {s, body, req}} ->
        case track_body_bytes(state, body) do
          {:ok, state} ->
            send(pid, {ref, {s, body}})
            {:ok, req, state}

          {:too_large, total} ->
            body_too_large(req, state, total)

          {:message_too_large, error} ->
            message_too_large(req, state, error)
        end

      {:wait, read_ref} ->
        {:ok, req, %{state | pending_reader: {read_ref, pid, ref}}}
    end
  end

  def info({:request_body, ref, :nofin, body}, req, %{pending_reader: {ref, pid, reader_ref}} = s) do
    case track_body_bytes(s, body) do
      {:ok, s} ->
        send(pid, {reader_ref, {:more, body}})
        {:ok, req, %{s | pending_reader: nil}}

      {:too_large, total} ->
        body_too_large(req, s, total)

      {:message_too_large, error} ->
        message_too_large(req, s, error)
    end
  end

  def info(
        {:request_body, ref, :fin, body_length, body},
        %{headers: headers} = req,
        %{pending_reader: {ref, pid, reader_ref}} = s
      ) do
    case track_body_bytes(s, body) do
      {:ok, s} ->
        send(pid, {reader_ref, {:ok, body}})

        # cowboy_req's set_body_length
        req =
          Map.merge(req, %{
            headers: Map.put(headers, "content-length", Integer.to_string(body_length)),
            body_length: body_length,
            has_read_body: true
          })

        {:ok, req, s}

      {:too_large, total} ->
        body_too_large(req, s, total)

      {:message_too_large, error} ->
        message_too_large(req, s, error)
    end
  end

  def info({:get_headers, ref, pid}, req, state) do
    headers = :cowboy_req.headers(req)
    send(pid, {ref, headers})
    {:ok, req, state}
  end

  def info({:get_peer, ref, pid}, req, state) do
    peer = :cowboy_req.peer(req)
    send(pid, {ref, peer})
    {:ok, req, state}
  end

  def info({:get_cert, ref, pid}, req, state) do
    peer = :cowboy_req.cert(req)
    send(pid, {ref, peer})
    {:ok, req, state}
  end

  def info({:get_qs, ref, pid}, req, state) do
    qs = :cowboy_req.qs(req)
    send(pid, {ref, qs})
    {:ok, req, state}
  end

  def info({:get_bindings, ref, pid}, req, state) do
    bindings = :cowboy_req.bindings(req)
    send(pid, {ref, bindings})
    {:ok, req, state}
  end

  # Runs the clauses below, then credits the sender whether or not the send worked.
  def info({:stream_body, data, opts, is_fin, http_transcode, reply_alias}, req, state) do
    result = info({:stream_body, data, opts, is_fin, http_transcode}, req, state)
    send(reply_alias, [reply_alias | :stream_body_ack])
    result
  end

  # Handle http/json transcoded response
  def info({:stream_body, data, _opts, is_fin, _http_transcode = true}, req, state) do
    # TODO Compress
    req = check_sent_resp(req)
    :cowboy_req.stream_body(data, is_fin, req)
    {:ok, req, state}
  end

  def info({:stream_body, data, opts, is_fin, _}, req, state) do
    # If compressor exists, compress is true by default
    compressor =
      if opts[:compress] == false do
        nil
      else
        state[:compressor]
      end

    accepted_encodings =
      case :cowboy_req.header("grpc-accept-encoding", req) do
        s when is_binary(s) ->
          String.split(s, ",")

        _ ->
          []
      end

    if compressor && !Enum.member?(accepted_encodings, compressor.name()) do
      msg =
        "A unaccepted encoding #{compressor.name()} is set, valid are: #{:cowboy_req.header("grpc-accept-encoding", req)}"

      error = RPCError.exception(status: :internal, message: msg)
      publish_send_error(state, error)
      req = send_error(req, error, state, :rpc_error)

      {:stop, req, state}
    else
      max_length = GRPC.Server.max_message_length()

      to_data_opts = [
        compressor: compressor,
        codec: opts[:codec],
        max_message_length: max_length
      ]

      case GRPC.Message.to_data(data, to_data_opts) do
        {:ok, data, _size} ->
          req = check_sent_resp(req)
          :cowboy_req.stream_body(data, is_fin, req)
          {:ok, req, state}

        {:error, msg} ->
          error =
            RPCError.exception(
              status: :resource_exhausted,
              message: "#{msg}, the limit is #{max_length} bytes"
            )

          publish_send_error(state, error)
          req = send_error(req, error, state, :rpc_error)
          {:stop, req, state}
      end
    end
  catch
    # A compressor or codec raised while it packed the reply.
    kind, reason ->
      stacktrace = __STACKTRACE__
      publish_send_error(state, kind, reason, stacktrace)

      error = RPCError.exception(status: :internal, message: "Internal Server Error")
      req = send_error(req, error, state, :rpc_error)

      [req: req]
      |> ReportException.new(reason, stacktrace, kind)
      |> maybe_log_error(state.exception_log_filter, stacktrace)

      {:stop, req, state}
  end

  def info({:stream_reply, status, headers}, req, state) do
    req = :cowboy_req.stream_reply(status, headers, req)
    {:ok, req, state}
  end

  def info({:set_resp_headers, headers}, req, state) do
    req = :cowboy_req.set_resp_headers(headers, req)
    {:ok, req, state}
  end

  def info({:set_resp_trailers, trailers}, req, state) do
    {:ok, req, Map.put(state, :resp_trailers, trailers)}
  end

  def info({:stream_trailers, trailers}, req, state) do
    metadata =
      state
      |> Map.get(:resp_trailers, %{})
      |> GRPC.Transport.HTTP2.encode_metadata()

    all_trailers = Map.merge(trailers, metadata)

    req = check_sent_resp(req)
    stream_grpcweb_trailers(req, all_trailers, state)
    :cowboy_req.stream_trailers(all_trailers, req)

    {:ok, req, state}
  end

  def info({:handling_timeout, _}, req, state) do
    error = %RPCError{status: GRPC.Status.deadline_exceeded(), message: "Deadline expired"}
    req = send_error(req, error, state, :timeout)

    [req: req]
    |> ReportException.new(error)
    |> maybe_log_error(state.exception_log_filter)

    {:stop, req, state}
  end

  def info({:set_compressor, compressor}, req, state) do
    accept_encoding = :cowboy_req.header("grpc-accept-encoding", req)

    if is_binary(accept_encoding) do
      req = :cowboy_req.set_resp_headers(%{"grpc-encoding" => compressor.name()}, req)
      {:ok, req, Map.put(state, :compressor, compressor)}
    else
      {:ok, req, state}
    end
  end

  def info({:EXIT, server_rpc_pid, reason}, req, state = %{pid: server_rpc_pid})
      when reason in [:normal, :shutdown] do
    {:stop, req, state}
  end

  # expected error raised from user to return error immediately
  def info({:EXIT, pid, {:shutdown, {%RPCError{} = error, stacktrace}}}, req, state = %{pid: pid}) do
    req = send_error(req, error, state, :rpc_error)

    [req: req]
    |> ReportException.new(error, stacktrace)
    |> maybe_log_error(state.exception_log_filter, stacktrace)

    {:stop, req, state}
  end

  # unknown error raised from rpc
  def info({:EXIT, pid, {:handle_error, error}}, req, state = %{pid: pid}) do
    %{kind: kind, reason: reason, stack: stack} = error
    rpc_error = %RPCError{status: GRPC.Status.unknown(), message: "Internal Server Error"}
    req = send_error(req, rpc_error, state, :error)

    [req: req]
    |> ReportException.new(reason, stack, kind)
    |> maybe_log_error(state.exception_log_filter, stack)

    {:stop, req, state}
  end

  def info({:EXIT, pid, {reason, stacktrace}}, req, state = %{pid: pid}) do
    error = %RPCError{status: GRPC.Status.unknown(), message: "Internal Server Error"}
    req = send_error(req, error, state, reason)

    [req: req]
    |> ReportException.new(reason, stacktrace)
    |> maybe_log_error(state.exception_log_filter, stacktrace)

    {:stop, req, state}
  end

  def terminate(reason, _req, state = %{pid: _pid}) do
    abort_rpc(state, reason)
    :ok
  end

  def terminate(_reason, _req, _state) do
    :ok
  end

  def call_rpc(server, path, stream) do
    started_at = System.monotonic_time()

    result =
      try do
        case do_call_rpc(server, path, stream, started_at) do
          {:error, _} = err ->
            err

          _ ->
            :ok
        end
      catch
        # A reply sent after the handler returned, such as the unary reply, is outside
        # `server_span`. Keep its status, as for an error raised by the handler.
        :error, %GRPC.RPCError{} = e ->
          {:rpc_error, e, __STACKTRACE__}

        kind, e ->
          reason = Exception.normalize(kind, e, __STACKTRACE__)

          {:error, %{kind: kind, reason: reason, stack: __STACKTRACE__}}
      end

    case result do
      {:rpc_error, e, stacktrace} ->
        exit({:shutdown, {e, stacktrace}})

      {:error, %GRPC.RPCError{} = e} ->
        exit({:shutdown, {e, _stacktrace = []}})

      {:error, %{kind: _kind, reason: _reason, stack: _stack} = e} ->
        exit({:handle_error, e})

      other ->
        other
    end
  end

  defp do_call_rpc(server, path, %{http_method: http_method} = stream, started_at) do
    case server.__call_rpc__(path, http_method, stream) do
      {:ok, stream, :noreply} ->
        report_send_failure(server, path, stream, started_at, fn ->
          GRPC.Server.send_trailers(stream, @default_trailers)
        end)

        {:ok, stream}

      {:ok, stream, response} ->
        report_send_failure(server, path, stream, started_at, fn ->
          stream
          |> GRPC.Server.send_reply(response)
          |> GRPC.Server.send_trailers(@default_trailers)
        end)

        {:ok, stream}

      {:ok, stream} ->
        report_send_failure(server, path, stream, started_at, fn ->
          GRPC.Server.send_trailers(stream, @default_trailers)
        end)

        {:ok, stream}

      error ->
        error
    end
  end

  # The telemetry span of the handler is closed here. Publish `:send_error`,
  # then raise the error again.
  defp report_send_failure(server, path, stream, started_at, send_fun) do
    send_fun.()
  catch
    kind, reason ->
      stacktrace = __STACKTRACE__

      execute_send_error(started_at, %{
        stream: stream,
        server: server,
        endpoint: stream.endpoint,
        path: path,
        kind: kind,
        reason: reason,
        stacktrace: stacktrace
      })

      :erlang.raise(kind, reason, stacktrace)
  end

  # Streaming reads arrive in chunks, so the cap is applied to the running total
  # rather than to any single chunk. read_full_body/5 does the same for unary calls.
  # Each chunk is also walked for message length prefixes, so a message that claims
  # to be over `:max_message_length` is refused when its prefix arrives.
  defp track_body_bytes(state, body) do
    total = Map.get(state, :body_bytes_read, 0) + byte_size(body)

    if total > state.max_body_size do
      {:too_large, total}
    else
      case scan_frames(state, body) do
        {:ok, state} -> {:ok, %{state | body_bytes_read: total}}
        {:message_too_large, _error} = too_large -> too_large
      end
    end
  end

  # Only gRPC bodies are framed; transcoded JSON and gRPC-web text are not.
  defp scan_frames(%{access_mode: :grpc} = state, data), do: do_scan_frames(state, data)
  defp scan_frames(state, _data), do: {:ok, state}

  defp do_scan_frames(state, <<>>), do: {:ok, state}

  defp do_scan_frames(%{frame_remaining: remaining} = state, data) when remaining > 0 do
    skip = min(remaining, byte_size(data))
    rest = binary_part(data, skip, byte_size(data) - skip)
    do_scan_frames(%{state | frame_remaining: remaining - skip}, rest)
  end

  defp do_scan_frames(%{frame_prefix: prefix} = state, data) do
    take = min(5 - byte_size(prefix), byte_size(data))
    head = binary_part(data, 0, take)
    rest = binary_part(data, take, byte_size(data) - take)

    case prefix <> head do
      <<_flag::8, length::32>> ->
        case GRPC.Server.message_too_large(length) do
          nil -> do_scan_frames(%{state | frame_prefix: <<>>, frame_remaining: length}, rest)
          error -> {:message_too_large, error}
        end

      partial ->
        {:ok, %{state | frame_prefix: partial}}
    end
  end

  # Asks cowboy for a chunk as soon as the next length prefix can be complete, or the
  # current message and the next prefix are in, so each prefix is checked early. With
  # cowboy's default of 8 MB, a chunk comes only after 8 MB or 15 s.
  defp read_length_opt(%{access_mode: :grpc} = state, opts) do
    length =
      case state.frame_remaining do
        0 -> 5 - byte_size(state.frame_prefix)
        remaining -> min(remaining + 5, @default_read_length)
      end

    Map.put(opts, :length, length)
  end

  defp read_length_opt(_state, opts), do: opts

  defp message_too_large(req, state, error) do
    req = send_error(req, error, state, :message_too_large)
    {:stop, req, state}
  end

  defp body_too_large(req, state, total) do
    Logger.warning(
      "Request body exceeded max_body_size (#{state.max_body_size} bytes, got #{total})"
    )

    error = RPCError.exception(status: :resource_exhausted, message: "Request body too large")
    req = send_error(req, error, state, :body_too_large)
    {:stop, req, state}
  end

  # With a gRPC body, the first chunk that holds the 5-byte length prefix is checked
  # and a message over the limit stops the read there, so the rest of it is never
  # requested from cowboy.
  defp read_full_body(req, chunks, size, timer, limits) do
    opts = timer |> timeout_left_opt() |> prefix_read_opts(size, limits)

    case :cowboy_req.read_body(req, opts) do
      {:ok, data, req} ->
        check_body_chunk(chunks, data, size, limits)
        {:ok, IO.iodata_to_binary([chunks, data]), req}

      {:more, data, req} ->
        size = check_body_chunk(chunks, data, size, limits)
        read_full_body(req, [chunks, data], size, timer, limits)
    end
  end

  # Until the prefix is in, ask for a chunk as soon as 5 bytes are there.
  defp prefix_read_opts(opts, size, %{check_prefix?: true}) when size < 5,
    do: Map.put(opts, :length, 5)

  defp prefix_read_opts(opts, _size, _limits), do: opts

  defp check_body_chunk(chunks, data, size, %{max_bytes: max_bytes, check_prefix?: check?}) do
    new_size = size + byte_size(data)

    if new_size > max_bytes, do: throw({:body_too_large, new_size})

    if check? and size < 5 and new_size >= 5 do
      <<_flag::8, length::32, _::binary>> = IO.iodata_to_binary([chunks, data])

      if error = GRPC.Server.message_too_large(length),
        do: throw({:message_too_large, error})
    end

    new_size
  end

  defp check_sent_resp(req, status \\ 200)

  defp check_sent_resp(%{has_sent_resp: _} = req, _status), do: req

  defp check_sent_resp(req, status) do
    :cowboy_req.stream_reply(status, req)
  end

  defp send_error_trailers(req, status, trailers, state) do
    # stream grpcweb trailers (if required) before sending
    # the normal HTTP2 trailers.
    #
    # sending those trailers may (or may not) change the
    # sending state of the connection, so pattern match
    # on the req after (maybe) streaming grpcweb trailers.
    #
    # when a resp has been already initiated, then just
    # append the trailers by calling `stream_trailers`.
    # otherwise, a full reply must be initiated.
    case stream_grpcweb_trailers(req, trailers, state) do
      %{has_sent_resp: _} = req ->
        :cowboy_req.stream_trailers(trailers, req)
        req

      req ->
        :cowboy_req.reply(status, trailers, req)
    end
  end

  def exit_handler(pid, reason) do
    if Process.alive?(pid) do
      Process.exit(pid, reason)
    end
  end

  # Stops an RPC that is still running, and publishes it. The exit signal does
  # not unwind the RPC process, so `:telemetry.span/3` around the call publishes
  # neither `:stop` nor `:exception`; this process outlives it and publishes
  # `:abort` before signalling.
  #
  # Both abort paths can run for one call — `send_error/4` signals the RPC
  # process and cowboy then calls `terminate/3` — and the signal is
  # asynchronous, so the process may still be alive for the second. The flag
  # keeps the event to one per call; it lives in the cowboy request process,
  # which handles this call only.
  defp abort_rpc(state, reason) do
    case Map.get(state, :pid) do
      pid when is_pid(pid) ->
        if Process.alive?(pid) do
          unless Process.put(@abort_published, true) do
            publish_abort(state, pid, reason)
          end

          exit_handler(pid, reason)
        end

      _no_rpc_process ->
        :ok
    end

    :ok
  end

  # The error was not raised, so use the current stacktrace.
  defp publish_send_error(state, %RPCError{} = error) do
    {:current_stacktrace, [_process_info | stacktrace]} =
      Process.info(self(), :current_stacktrace)

    publish_send_error(state, :error, error, stacktrace)
  end

  defp publish_send_error(state, kind, reason, stacktrace) do
    %{stream: stream, endpoint: endpoint, route: route, started_at: started_at} = state

    execute_send_error(started_at, %{
      stream: stream,
      server: stream.server,
      endpoint: endpoint,
      path: route,
      kind: kind,
      reason: reason,
      stacktrace: stacktrace
    })
  end

  defp execute_send_error(started_at, metadata) do
    now = System.monotonic_time()

    :telemetry.execute(
      GRPC.Telemetry.server_rpc_prefix() ++ [:send_error],
      %{duration: now - started_at, monotonic_time: now},
      metadata
    )
  end

  defp publish_abort(state, pid, reason) do
    %{stream: stream, endpoint: endpoint, route: route, started_at: started_at} = state

    :telemetry.execute(
      GRPC.Telemetry.server_rpc_prefix() ++ [:abort],
      %{duration: System.monotonic_time() - started_at},
      %{
        stream: stream,
        server: stream.server,
        endpoint: endpoint,
        path: route,
        pid: pid,
        reason: reason
      }
    )
  end

  defp timeout_left_opt(timer, opts \\ %{}) do
    case timer do
      nil ->
        # No grpc-timeout header was supplied. Do not override cowboy's built-in
        # per-chunk read timeout (15 s by default) with :infinity, which would
        # allow a slow-trickle client to hold the connection open indefinitely.
        opts

      timer ->
        case Process.read_timer(timer) do
          ms when is_integer(ms) ->
            Map.put(opts, :timeout, ms)

          _ ->
            Map.put(opts, :timeout, 0)
        end
    end
  end

  defp extract_subtype("application/json"), do: {:ok, :http_transcoding, "json"}
  defp extract_subtype("application/json;" <> _), do: {:ok, :http_transcoding, "json"}
  defp extract_subtype("application/json " <> _), do: {:ok, :http_transcoding, "json"}
  defp extract_subtype("application/grpc"), do: {:ok, :grpc, "proto"}
  defp extract_subtype("application/grpc+"), do: {:ok, :grpc, "proto"}
  defp extract_subtype("application/grpc;"), do: {:ok, :grpc, "proto"}
  defp extract_subtype(<<"application/grpc+", rest::binary>>), do: {:ok, :grpc, rest}
  defp extract_subtype(<<"application/grpc;", rest::binary>>), do: {:ok, :grpc, rest}

  defp extract_subtype("application/grpc-web"), do: {:ok, :grpcweb, "proto"}
  defp extract_subtype("application/grpc-web+"), do: {:ok, :grpcweb, "proto"}
  defp extract_subtype("application/grpc-web;"), do: {:ok, :grpcweb, "proto"}
  defp extract_subtype("application/grpc-web-text"), do: {:ok, :grpcweb, "text"}
  defp extract_subtype("application/grpc-web+" <> rest), do: {:ok, :grpcweb, rest}
  defp extract_subtype("application/grpc-web-text+" <> rest), do: {:ok, :grpcweb, rest}

  defp extract_subtype(type) do
    Logger.warning("""
    Received invalid content-type: "#{type}".
    This usually means the request is missing a proper Content-Type header,
    or it's using a non-standard value. This may cause request parsing or response
    encoding errors.
    """)

    {:ok, :unknown, "unknown"}
  end

  defp resolve_access_mode(%{version: "HTTP/1.1"}, _detected_access_mode, _type_subtype),
    do: :http_transcoding

  defp resolve_access_mode(%{method: "OPTIONS"}, _detected_access_mode, _type_subtype),
    do: :grpcweb

  defp resolve_access_mode(_req, detected_access_mode, _type_subtype), do: detected_access_mode

  defp preflight?(%{method: "OPTIONS"}), do: true
  defp preflight?(_), do: false

  defp send_error(req, error, state, reason) do
    trailers = HTTP2.server_trailers(error.status, error.message, error.details)

    status =
      if state.access_mode == :http_transcoding,
        do: GRPC.Status.http_code(error.status),
        else: 200

    abort_rpc(state, reason)

    send_error_trailers(req, status, trailers, state)
  end

  # Similar with cowboy's read_body, but we need to receive the message
  # in `info` callback.
  defp async_read_body(%{has_body: false} = req, _opts) do
    {:send, {:ok, <<>>, req}}
  end

  defp async_read_body(%{has_read_body: true} = req, _opts) do
    {:send, {:ok, <<>>, req}}
  end

  defp async_read_body(req, opts) do
    length = opts[:length] || @default_read_length
    period = opts[:period] || 15000
    ref = make_ref()

    :cowboy_req.cast({:read_body, self(), ref, length, period}, req)

    {:wait, ref}
  end

  defp maybe_log_error(exception, filter, stacktrace \\ [])

  defp maybe_log_error(
         %ReportException{} = exception,
         {module, func_name},
         stacktrace
       ) do
    if apply(module, func_name, [exception]) do
      log_error(exception, stacktrace)
    else
      :ok
    end
  end

  defp maybe_log_error(exception, nil, stacktrace) do
    log_error(exception, stacktrace)
  end

  defp log_error(%ReportException{kind: kind} = exception, stacktrace) do
    crash_reason = GRPC.Logger.crash_reason(kind, exception, stacktrace)

    kind
    |> Exception.format(exception, stacktrace)
    |> Logger.error(crash_reason: crash_reason)

    :ok
  end

  defp stream_grpcweb_trailers(req, trailers, %{access_mode: :grpcweb} = state) do
    # grpc_web requires trailers be sent as the last
    # message block rather than in the HTTP trailers
    # as javascript runtimes do not propagate trailers
    #
    # trailers are instead denoted with the "trailer flag"
    # which has the MSB set to 1.
    {:ok, data, _length} =
      trailers
      |> Enum.map_join("\r\n", fn {k, v} -> "#{k}: #{v}" end)
      |> GRPC.Message.to_data(message_flag: @trailers_flag)

    packed =
      if function_exported?(state.codec, :pack_for_channel, 1) do
        state.codec.pack_for_channel(data)
      else
        data
      end

    req = check_sent_resp(req)
    :cowboy_req.stream_body(packed, :nofin, req)
    req
  end

  defp stream_grpcweb_trailers(req, _trailers, _not_grpcweb), do: req
end
