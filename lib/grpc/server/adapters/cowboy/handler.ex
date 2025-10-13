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

  @type init_state :: {
          endpoint :: atom(),
          server :: {name :: String.t(), module()},
          route :: String.t(),
          opts :: keyword()
        }

  @type pending_reader :: {
          cowboy_read_ref :: reference,
          server_rpc_pid :: pid,
          server_rpc_reader_reference :: reference
        }
  @type stream_state :: %{
          pid: server_rpc_pid :: pid,
          handling_timer: timeout_timer_ref :: reference,
          pending_reader: nil | pending_reader,
          access_mode: GRPC.Server.Stream.access_mode()
        }
  @type init_result ::
          {:cowboy_loop, :cowboy_req.req(), stream_state} | {:ok, :cowboy_req.req(), init_state}

  @type is_fin :: :fin | :nofin

  @type stream_body_opts :: {:code, module()} | {:compress, boolean()}

  @type headers :: %{binary() => binary()}

  @doc """
  This function is meant to be called whenever a new request arrives to an existing connection.
  This handler works mainly with two linked processes.
  One of them is the process started by cowboy which internally we'll refer to it as `stream_pid`,
  this process is responsible to interface the interactions with the open socket.
  The second process is the one we start in this function, we'll refer to it as `server_rpc_pid`,
  which is the point where we call the functions implemented by users (aka the modules who use
  the `GRPC.Server` macro)
  """
  @spec init(:cowboy_req.req(), state :: init_state) :: init_result
  def init(req, {endpoint, {_name, server}, route, opts} = state) do
    http_method = extract_http_method(req) |> String.to_existing_atom()

    with {:ok, access_mode, sub_type, content_type} <- find_content_type_subtype(req),
         {:ok, codec} <- find_codec(sub_type, content_type, server),
         {:ok, compressor} <- find_compressor(req, server) do
      stream_pid = self()
      http_transcode = access_mode == :http_transcoding
      request_headers = :cowboy_req.headers(req)

      stream = %GRPC.Server.Stream{
        server: server,
        endpoint: endpoint,
        adapter: @adapter,
        payload: %{pid: stream_pid},
        local: opts[:local],
        codec: codec,
        http_method: http_method,
        http_request_headers: request_headers,
        http_transcode: http_transcode,
        compressor: compressor,
        is_preflight?: preflight?(req),
        access_mode: access_mode
      }

      server_rpc_pid = :proc_lib.spawn_link(__MODULE__, :call_rpc, [server, route, stream])
      Process.flag(:trap_exit, true)

      req = :cowboy_req.set_resp_headers(HTTP2.server_headers(stream), req)

      timeout = Map.get(request_headers, "grpc-timeout")

      timer_ref =
        if is_binary(timeout) do
          Process.send_after(
            self(),
            {:handling_timeout, self()},
            GRPC.Transport.Utils.decode_timeout(timeout)
          )
        end

      {
        :cowboy_loop,
        req,
        %{
          pid: server_rpc_pid,
          handling_timer: timer_ref,
          pending_reader: nil,
          access_mode: access_mode
        }
      }
    else
      {:error, error} ->
        Logger.error(fn -> inspect(error) end)
        trailers = HTTP2.server_trailers(error.status, error.message)
        req = send_error_trailers(req, 200, trailers)
        {:ok, req, state}
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

  defp find_content_type_subtype(req) do
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
          if http_method == "post" do
            {:ok, :grpc, "proto"}
          else
            {:ok, :http_transcoding, "json"}
          end

        resp ->
          resp
      end

    access_mode = resolve_access_mode(req, access_mode, subtype)
    {:ok, access_mode, subtype, content_type}
  end

  defp extract_http_method(req) do
    req
    |> :cowboy_req.method()
    |> String.downcase()
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
  @spec read_full_body(stream_pid :: pid) :: binary()
  def read_full_body(pid) do
    sync_call(pid, :read_full_body)
  end

  @doc """
  Synchronously reads a chunk of body content of a given request.
  Raise in case of a timeout.
  """
  @spec read_body(stream_pid :: pid) :: binary()
  def read_body(pid) do
    sync_call(pid, :read_body)
  end

  @doc """
  Asynchronously send back to client a chunk of `data`, when `http_transcode?` is true, the
  data is sent back as it's, with no transformation of protobuf binaries to http2 data frames.
  """
  @spec stream_body(
          stream_pid :: pid,
          data :: iodata,
          opts :: list(stream_body_opts),
          is_fin,
          http_transcode? :: boolean()
        ) :: :ok
  def stream_body(pid, data, opts, is_fin, http_transcode? \\ false) do
    send(pid, {:stream_body, data, opts, is_fin, http_transcode?})
    :ok
  end

  @doc """
  Asynchronously send back to the client the http status and the headers for a given request.
  """
  @spec stream_reply(stream_pid :: pid, status :: non_neg_integer(), headers :: headers) :: :ok
  def stream_reply(pid, status, headers) do
    send(pid, {:stream_reply, status, headers})
    :ok
  end

  @doc """
  Asynchronously set the headers for a given request. This function does not send any
  data back to the client. It simply appends the headers to be used in the response.
  """
  @spec set_resp_headers(stream_pid :: pid, headers :: headers) :: :ok
  def set_resp_headers(pid, headers) do
    send(pid, {:set_resp_headers, headers})
    :ok
  end

  @doc """
  Asynchronously set the trailer headers for a given request. This function does not send any
  data back to the client. It simply appends the trailer headers to be used in the response.
  """
  @spec set_resp_trailers(stream_pid :: pid, trailers :: headers) :: :ok
  def set_resp_trailers(pid, trailers) do
    send(pid, {:set_resp_trailers, trailers})
    :ok
  end

  @doc """
  Asynchronously set the compressor algorithm to be used for compress the responses. This checks if
  the `grpc-accept-encoding` header is present on the original request, otherwise no compression
  is applied.
  """
  @spec set_compressor(stream_pid :: pid, compressor :: module) :: :ok
  def set_compressor(pid, compressor) do
    send(pid, {:set_compressor, compressor})
    :ok
  end

  @doc """
  Asynchronously stream the given trailers of request back to client.
  """
  @spec stream_trailers(stream_pid :: pid, trailers :: headers) :: :ok
  def stream_trailers(pid, trailers) do
    send(pid, {:stream_trailers, trailers})
    :ok
  end

  @doc """
  Return all request headers.
  """
  @spec get_headers(stream_pid :: pid) :: :cowboy.http_headers()
  def get_headers(pid) do
    sync_call(pid, :get_headers)
  end

  @doc """
  Return the peer IP address and port number
  """
  @spec get_peer(stream_pid :: pid) :: {:inet.ip_address(), :inet.port_number()}
  def get_peer(pid) do
    sync_call(pid, :get_peer)
  end

  @doc """
  Return the client TLS certificate. `:undefined` is returned if no certificate was specified
  when establishing the connection.
  """
  @spec get_cert(stream_pid :: pid) :: binary() | :undefined
  def get_cert(pid) do
    sync_call(pid, :get_cert)
  end

  @doc """
  Return the query string for the request URI.
  """
  @spec get_qs(stream_pid :: pid) :: binary()
  def get_qs(pid) do
    sync_call(pid, :get_qs)
  end

  @doc """
  Return all bindings of a given request.
  """
  @spec get_bindings(stream_pid :: pid) :: :cowboy_router.bindings()
  def get_bindings(pid) do
    sync_call(pid, :get_bindings)
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
    {s, body, req} = read_full_body(req, "", state[:handling_timer])
    send(pid, {ref, {s, body}})
    {:ok, req, state}
  catch
    :exit, :timeout ->
      Logger.warning("Timeout when reading full body")
      info({:handling_timeout, self()}, req, state)
  end

  def info({:read_body, ref, pid}, req, state) do
    opts = timeout_left_opt(state[:handling_timer])

    case async_read_body(req, opts) do
      {:send, {s, body, req}} ->
        send(pid, {ref, {s, body}})
        {:ok, req, state}

      {:wait, read_ref} ->
        {:ok, req, %{state | pending_reader: {read_ref, pid, ref}}}
    end
  end

  def info({:request_body, ref, :nofin, body}, req, %{pending_reader: {ref, pid, reader_ref}} = s) do
    send(pid, {reader_ref, {:more, body}})

    {:ok, req, %{s | pending_reader: nil}}
  end

  def info(
        {:request_body, ref, :fin, body_length, body},
        %{headers: headers} = req,
        %{pending_reader: {ref, pid, reader_ref}} = s
      ) do
    send(pid, {reader_ref, {:ok, body}})

    # cowboy_req's set_body_length
    req =
      Map.merge(req, %{
        headers: Map.put(headers, "content-length", Integer.to_string(body_length)),
        body_length: body_length,
        has_read_body: true
      })

    {:ok, req, s}
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
      req = send_error(req, error, state, :rpc_error)

      {:stop, req, state}
    else
      case GRPC.Message.to_data(data, compressor: compressor, codec: opts[:codec]) do
        {:ok, data, _size} ->
          req = check_sent_resp(req)
          :cowboy_req.stream_body(data, is_fin, req)
          {:ok, req, state}

        {:error, msg} ->
          error = RPCError.exception(status: :internal, message: msg)
          req = send_error(req, error, state, :rpc_error)
          {:stop, req, state}
      end
    end
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
    metadata = Map.get(state, :resp_trailers, %{})
    metadata = GRPC.Transport.HTTP2.encode_metadata(metadata)
    send_stream_trailers(req, Map.merge(metadata, trailers))
    {:ok, req, state}
  end

  def info({:handling_timeout, _}, req, state) do
    error = %RPCError{status: GRPC.Status.deadline_exceeded(), message: "Deadline expired"}
    req = send_error(req, error, state, :timeout)

    [req: req]
    |> ReportException.new(error)
    |> log_error()

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
  def info({:EXIT, pid, {%RPCError{} = error, stacktrace}}, req, state = %{pid: pid}) do
    req = send_error(req, error, state, :rpc_error)

    [req: req]
    |> ReportException.new(error, stacktrace)
    |> log_error(stacktrace)

    {:stop, req, state}
  end

  # unknown error raised from rpc
  def info({:EXIT, pid, {:handle_error, error}}, req, state = %{pid: pid}) do
    %{kind: kind, reason: reason, stack: stack} = error
    rpc_error = %RPCError{status: GRPC.Status.unknown(), message: "Internal Server Error"}
    req = send_error(req, rpc_error, state, :error)

    [req: req]
    |> ReportException.new(reason, stack, kind)
    |> log_error(stack)

    {:stop, req, state}
  end

  def info({:EXIT, pid, {reason, stacktrace}}, req, state = %{pid: pid}) do
    error = %RPCError{status: GRPC.Status.unknown(), message: "Internal Server Error"}
    req = send_error(req, error, state, reason)

    [req: req]
    |> ReportException.new(reason, stacktrace)
    |> log_error(stacktrace)

    {:stop, req, state}
  end

  def terminate(reason, _req, %{pid: pid}) do
    exit_handler(pid, reason)
    :ok
  end

  def terminate(_reason, _req, _state) do
    :ok
  end

  def call_rpc(server, path, stream) do
    result =
      try do
        case do_call_rpc(server, path, stream) do
          {:error, _} = err ->
            err

          _ ->
            :ok
        end
      catch
        kind, e ->
          reason = Exception.normalize(kind, e, __STACKTRACE__)

          {:error, %{kind: kind, reason: reason, stack: __STACKTRACE__}}
      end

    case result do
      {:error, %GRPC.RPCError{} = e} ->
        exit({e, _stacktrace = []})

      {:error, %{kind: _kind, reason: _reason, stack: _stack} = e} ->
        exit({:handle_error, e})

      other ->
        other
    end
  end

  defp do_call_rpc(server, path, %{http_method: http_method} = stream) do
    result = server.__call_rpc__(path, http_method, stream)

    case result do
      {:ok, stream, response} ->
        stream
        |> GRPC.Server.send_reply(response)
        |> GRPC.Server.send_trailers(@default_trailers)

        {:ok, stream}

      {:ok, stream} ->
        GRPC.Server.send_trailers(stream, @default_trailers)
        {:ok, stream}

      error ->
        error
    end
  end

  defp read_full_body(req, body, timer) do
    result = :cowboy_req.read_body(req, timeout_left_opt(timer))

    case result do
      {:ok, data, req} -> {:ok, body <> data, req}
      {:more, data, req} -> read_full_body(req, body <> data, timer)
    end
  end

  defp send_stream_trailers(req, trailers) do
    req = check_sent_resp(req)
    :cowboy_req.stream_trailers(trailers, req)
  end

  defp check_sent_resp(%{has_sent_resp: _} = req) do
    req
  end

  defp check_sent_resp(req) do
    :cowboy_req.stream_reply(200, req)
  end

  defp send_error_trailers(%{has_sent_resp: _} = req, _, trailers) do
    :cowboy_req.stream_trailers(trailers, req)
  end

  defp send_error_trailers(req, status, trailers) do
    :cowboy_req.reply(status, trailers, req)
  end

  def exit_handler(pid, reason) do
    if Process.alive?(pid) do
      Process.exit(pid, reason)
    end
  end

  defp timeout_left_opt(timer, opts \\ %{}) do
    case timer do
      nil ->
        Map.put(opts, :timeout, :infinity)

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
    Logger.warning("Got unknown content-type #{type}, please create an issue. ")

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

    if pid = Map.get(state, :pid) do
      exit_handler(pid, reason)
    end

    send_error_trailers(req, status, trailers)
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
    length = opts[:length] || 8_000_000
    period = opts[:period] || 15000
    ref = make_ref()

    :cowboy_req.cast({:read_body, self(), ref, length, period}, req)

    {:wait, ref}
  end

  defp log_error(%ReportException{kind: kind} = exception, stacktrace \\ []) do
    crash_reason = GRPC.Logger.crash_reason(kind, exception, stacktrace)

    kind
    |> Exception.format(exception, stacktrace)
    |> Logger.error(crash_reason: crash_reason)
  end
end
