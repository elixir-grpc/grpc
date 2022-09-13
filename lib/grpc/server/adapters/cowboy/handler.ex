defmodule GRPC.Server.Adapters.Cowboy.Handler do
  @moduledoc false

  # A cowboy handler accepting all requests and calls corresponding functions
  # defined by users.

  alias GRPC.Transport.HTTP2
  alias GRPC.RPCError
  require Logger

  @adapter GRPC.Server.Adapters.Cowboy
  @default_trailers HTTP2.server_trailers()

  @spec init(
          map(),
          state ::
            {endpoint :: atom(), server :: {String.t(), module()}, route :: String.t(),
             opts :: keyword()}
        ) :: {:cowboy_loop, map(), map()}
  def init(req, {endpoint, {_name, server}, route, opts} = state) do
    with {:ok, codec} <- find_codec(req, server),
         {:ok, compressor} <- find_compressor(req, server) do
      stream = %GRPC.Server.Stream{
        server: server,
        endpoint: endpoint,
        adapter: @adapter,
        payload: %{pid: self()},
        local: opts[:local],
        codec: codec,
        compressor: compressor
      }

      pid = spawn_link(__MODULE__, :call_rpc, [server, route, stream])
      Process.flag(:trap_exit, true)

      req = :cowboy_req.set_resp_headers(HTTP2.server_headers(stream), req)

      timeout = :cowboy_req.header("grpc-timeout", req)

      timer_ref =
        if is_binary(timeout) do
          Process.send_after(
            self(),
            {:handling_timeout, self()},
            GRPC.Transport.Utils.decode_timeout(timeout)
          )
        end

      {:cowboy_loop, req, %{pid: pid, handling_timer: timer_ref, pending_reader: nil}}
    else
      {:error, error} ->
        Logger.error(fn -> inspect(error) end)
        trailers = HTTP2.server_trailers(error.status, error.message)
        req = send_error_trailers(req, trailers)
        {:ok, req, state}
    end
  end

  defp find_codec(req, server) do
    req_content_type = :cowboy_req.header("content-type", req)

    with {:ok, subtype} <- extract_subtype(req_content_type),
         codec when not is_nil(codec) <-
           Enum.find(server.__meta__(:codecs), nil, fn c -> c.name() == subtype end) do
      {:ok, codec}
    else
      err ->
        Logger.error(fn -> inspect(err) end)

        {:error,
         RPCError.exception(
           status: :unimplemented,
           message: "No codec registered for content-type #{req_content_type}"
         )}
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
  def read_full_body(pid) do
    sync_call(pid, :read_full_body)
  end

  def read_body(pid) do
    sync_call(pid, :read_body)
  end

  def stream_body(pid, data, opts, is_fin, http_transcode \\ false) do
    send(pid, {:stream_body, data, opts, is_fin, http_transcode})
  end

  def stream_reply(pid, status, headers) do
    send(pid, {:stream_reply, status, headers})
  end

  def set_resp_headers(pid, headers) do
    send(pid, {:set_resp_headers, headers})
  end

  def set_resp_trailers(pid, trailers) do
    send(pid, {:set_resp_trailers, trailers})
  end

  def set_compressor(pid, compressor) do
    send(pid, {:set_compressor, compressor})
  end

  def stream_trailers(pid, trailers) do
    send(pid, {:stream_trailers, trailers})
  end

  def get_headers(pid) do
    sync_call(pid, :get_headers)
  end

  def get_peer(pid) do
    sync_call(pid, :get_peer)
  end

  def get_cert(pid) do
    sync_call(pid, :get_cert)
  end

  def get_qs(pid) do
    sync_call(pid, :get_qs)
  end

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
      Logger.warn("Timeout when reading full body")
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

      req = send_error(req, state, msg)
      {:stop, req, state}
    else
      case GRPC.Message.to_data(data, compressor: compressor, codec: opts[:codec]) do
        {:ok, data, _size} ->
          req = check_sent_resp(req)
          :cowboy_req.stream_body(data, is_fin, req)
          {:ok, req, state}

        {:error, msg} ->
          req = send_error(req, state, msg)
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

  def info({:handling_timeout, _}, req, state = %{pid: pid}) do
    error = %RPCError{status: GRPC.Status.deadline_exceeded(), message: "Deadline expired"}
    trailers = HTTP2.server_trailers(error.status, error.message)
    exit_handler(pid, :timeout)
    req = send_error_trailers(req, trailers)
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

  def info({:EXIT, pid, :normal}, req, state = %{pid: pid}) do
    exit_handler(pid, :normal)
    {:stop, req, state}
  end

  # expected error raised from user to return error immediately
  def info({:EXIT, pid, {%RPCError{} = error, _stacktrace}}, req, state = %{pid: pid}) do
    trailers = HTTP2.server_trailers(error.status, error.message)
    exit_handler(pid, :rpc_error)
    req = send_error_trailers(req, trailers)
    {:stop, req, state}
  end

  # unknown error raised from rpc
  def info({:EXIT, pid, {:handle_error, _kind}}, req, state = %{pid: pid}) do
    error = %RPCError{status: GRPC.Status.unknown(), message: "Internal Server Error"}
    trailers = HTTP2.server_trailers(error.status, error.message)
    exit_handler(pid, :error)
    req = send_error_trailers(req, trailers)
    {:stop, req, state}
  end

  def info({:EXIT, pid, {reason, stacktrace}}, req, state = %{pid: pid}) do
    Logger.error(Exception.format(:error, reason, stacktrace))
    error = %RPCError{status: GRPC.Status.unknown(), message: "Internal Server Error"}
    trailers = HTTP2.server_trailers(error.status, error.message)
    exit_handler(pid, reason)
    req = send_error_trailers(req, trailers)
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
          Logger.error(Exception.format(kind, e, __STACKTRACE__))

          exit({:handle_error, kind})
      end

    case result do
      {:error, %GRPC.RPCError{} = e} ->
        exit({e, ""})

      {:error, %{kind: kind}} ->
        exit({:handle_error, kind})

      other ->
        other
    end
  end

  defp do_call_rpc(server, path, stream) do
    result = server.__call_rpc__(path, stream)

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

  defp send_error_trailers(%{has_sent_resp: _} = req, trailers) do
    :cowboy_req.stream_trailers(trailers, req)
  end

  defp send_error_trailers(req, trailers) do
    :cowboy_req.reply(200, trailers, req)
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

  defp extract_subtype("application/json"), do: {:ok, "json"}
  defp extract_subtype("application/grpc"), do: {:ok, "proto"}
  defp extract_subtype("application/grpc+"), do: {:ok, "proto"}
  defp extract_subtype("application/grpc;"), do: {:ok, "proto"}
  defp extract_subtype(<<"application/grpc+", rest::binary>>), do: {:ok, rest}
  defp extract_subtype(<<"application/grpc;", rest::binary>>), do: {:ok, rest}

  defp extract_subtype("application/grpc-web"), do: {:ok, "proto"}
  defp extract_subtype("application/grpc-web+"), do: {:ok, "proto"}
  defp extract_subtype("application/grpc-web;"), do: {:ok, "proto"}
  defp extract_subtype("application/grpc-web-text"), do: {:ok, "text"}
  defp extract_subtype("application/grpc-web+" <> rest), do: {:ok, rest}
  defp extract_subtype("application/grpc-web-text+" <> rest), do: {:ok, rest}

  defp extract_subtype(type) do
    Logger.warn("Got unknown content-type #{type}, please create an issue.")
    {:ok, "proto"}
  end

  defp send_error(req, %{pid: pid}, msg) do
    error = RPCError.exception(status: :internal, message: msg)
    trailers = HTTP2.server_trailers(error.status, error.message)

    exit_handler(pid, :rpc_error)
    send_error_trailers(req, trailers)
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
end
