defmodule GRPC.Adapter.Cowboy.Handler do
  @moduledoc false

  # A cowboy handler accepting all requests and calls corresponding functions
  # defined by users.

  alias GRPC.Transport.HTTP2
  alias GRPC.RPCError
  require Logger

  @adapter GRPC.Adapter.Cowboy
  @default_trailers HTTP2.server_trailers()
  @type state :: %{pid: pid, handling_timeout: reference, resp_trailers: map}

  @spec init(map, {GRPC.Server.servers_map(), keyword}) :: {:cowboy_loop, map, state}
  def init(req, {servers, _opts}) do
    path = :cowboy_req.path(req)
    server = Map.get(servers, GRPC.Server.service_name(path))
    stream = %GRPC.Server.Stream{server: server, adapter: @adapter, payload: %{pid: self()}}
	  pid = spawn_link(__MODULE__, :call_rpc, [server, path, stream])

    req = :cowboy_req.set_resp_headers(HTTP2.server_headers(), req)
    Process.flag(:trap_exit, true)

    timeout = :cowboy_req.header("grpc-timeout", req)
    timer_ref = if timeout do
      Process.send_after(self(), {:handling_timeout, self()}, GRPC.Transport.Utils.decode_timeout(timeout))
    end

    {:cowboy_loop, req, %{pid: pid, handling_timeout: timer_ref}}
  end

  # APIs begin
  def read_full_body(pid) do
    sync_call(pid, :read_full_body)
  end

  def read_body(pid) do
    sync_call(pid, :read_body)
  end

  def stream_body(pid, data, is_fin) do
    send(pid, {:stream_body, self(), data, is_fin})
  end

  def stream_reply(pid, status, headers) do
    send(pid, {:stream_reply, self(), status, headers})
  end

  def set_resp_headers(pid, headers) do
    send(pid, {:set_resp_headers, self(), headers})
  end

  def set_resp_trailers(pid, trailers) do
    send(pid, {:set_resp_trailers, self(), trailers})
  end

  def stream_trailers(pid, trailers) do
    send(pid, {:stream_trailers, self(), trailers})
  end

  def get_headers(pid) do
    sync_call(pid, :get_headers)
  end

  defp sync_call(pid, key) do
    send(pid, {key, self()})
    receive do
      msg -> msg
    end
  end
  # APIs end

  def info({:read_full_body, pid}, req, state = %{pid: pid}) do
    {s, body, req} = read_full_body(req, "")
    send(pid, {s, body})
    {:ok, req, state}
  end
  def info({:read_body, pid}, req, state = %{pid: pid}) do
    {s, body, req} = :cowboy_req.read_body(req)
    send(pid, {s, body})
    {:ok, req, state}
  end
  def info({:stream_body, pid, data, is_fin}, req, state = %{pid: pid}) do
    req = check_sent_resp(req)
    :cowboy_req.stream_body(data, is_fin, req)
    {:ok, req, state}
  end
  def info({:stream_reply, pid, status, headers}, req, state = %{pid: pid}) do
    req = :cowboy_req.stream_reply(status, headers, req)
    {:ok, req, state}
  end
  def info({:set_resp_headers, pid, headers}, req, state = %{pid: pid}) do
    req = :cowboy_req.set_resp_headers(headers, req)
    {:ok, req, state}
  end
  def info({:set_resp_trailers, pid, trailers}, req, state = %{pid: pid}) do
    {:ok, req, Map.put(state, :resp_trailers, trailers)}
  end
  def info({:stream_trailers, pid, trailers}, req, state = %{pid: pid}) do
    metadata = Map.get(state, :resp_trailers, %{})
    metadata = GRPC.Transport.HTTP2.encode_metadata(metadata)
    send_stream_trailers(req, Map.merge(metadata, trailers))
    {:ok, req, state}
  end
  def info({:get_headers, pid}, req, state = %{pid: pid}) do
    headers = :cowboy_req.headers(req)
    send(pid, headers)
    {:ok, req, state}
  end

  def info({:handling_timeout, _}, req, state = %{pid: pid}) do
    error = %RPCError{status: GRPC.Status.deadline_exceeded(), message: "Deadline expired"}
    trailers = HTTP2.server_trailers(error.status, error.message)
    exit_handler(pid, :timeout)
    send_error_trailers(req, trailers)
    {:stop, req, state}
  end

  def info({:EXIT, pid, :normal}, req, state = %{pid: pid}) do
    exit_handler(pid, :normal)
    {:stop, req, state}
  end
  # expected error raised from user to return error immediately
  def info({:EXIT, pid, {%RPCError{} = error, _stacktrace}}, req, state = %{pid: pid}) do
    trailers = HTTP2.server_trailers(error.status, error.message)
    exit_handler(pid, :rpc_error)
    send_error_trailers(req, trailers)
    {:stop, req, state}
  end
  # unknown error raised from rpc
  def info({:EXIT, pid, {:handle_error, _kind}}, req, state = %{pid: pid}) do
    error = %RPCError{status: GRPC.Status.unknown(), message: "Internal Server Error"}
    trailers = HTTP2.server_trailers(error.status, error.message)
    exit_handler(pid, :error)
    send_error_trailers(req, trailers)
    {:stop, req, state}
  end
  def info({:EXIT, pid, {reason, stacktrace}}, req, state = %{pid: pid}) do
    Logger.error(Exception.format(:error, reason, stacktrace))
    error = %RPCError{status: GRPC.Status.unknown(), message: "Internal Server Error"}
    trailers = HTTP2.server_trailers(error.status, error.message)
    exit_handler(pid, :reason)
    send_error_trailers(req, trailers)
    {:stop, req, state}
  end

  def call_rpc(server, path, stream) do
    try do
      do_call_rpc(server, path, stream)
    rescue
      e in RPCError ->
        exit({e, ""})
    catch
      kind, e ->
        Logger.error(Exception.format(kind, e))
        exit({:handle_error, kind})
    end
  end

  defp do_call_rpc(server, path, stream) do
    case server.__call_rpc__(path, stream) do
      {:ok, stream, response} ->
        stream
        |> GRPC.Server.send_reply(response)
        |> GRPC.Server.send_trailers(@default_trailers)

        {:ok, stream}

      {:ok, stream} ->
        GRPC.Server.send_trailers(stream, @default_trailers)
        {:ok, stream}
    end
  end

  defp read_full_body(req, body) do
    case :cowboy_req.read_body(req) do
      {:ok, data, req} -> {:ok, body <> data, req}
      {:more, data, req} -> read_full_body(req, body <> data)
    end
  end

  defp send_stream_trailers(req, trailers) do
    req = check_sent_resp(req)
    :cowboy_req.stream_trailers(trailers, req)
  end

  defp check_sent_resp(req) do
    if req[:has_sent_resp] == nil do
      :cowboy_req.stream_reply(200, req)
    else
      req
    end
  end

  defp send_error_trailers(req, trailers) do
    if req[:has_sent_resp] == nil do
      :cowboy_req.stream_reply(200, trailers, req)
    else
      :cowboy_req.stream_trailers(trailers, req)
    end
  end

  def exit_handler(pid, reason) do
    if Process.alive?(pid) do
      Process.exit(pid, reason)
    end
  end
end
