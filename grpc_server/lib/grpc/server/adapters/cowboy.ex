defmodule GRPC.Server.Adapters.Cowboy do
  @moduledoc """
  A server (`b:GRPC.Server.Adapter`) adapter using `:cowboy`.

  Cowboy requests will be stored in the `:payload` field of the `GRPC.Server.Stream`.
  """

  @behaviour GRPC.Server.Adapter

  require Logger
  alias GRPC.Server.Adapters.Cowboy.ConnectionAge
  alias GRPC.Server.Adapters.Cowboy.Handler

  @default_num_acceptors 20
  @default_max_connections 16384

  @doc """
  Starts a Cowboy server. Only used in starting a server manually using `GRPC.Server.start(servers)`. Otherwise `GRPC.Server.Adapters.Cowboy.child_spec/4` is used.

  The available options below are a subset of [`ranch_tcp`](https://ninenines.eu/docs/en/ranch/1.7/manual/ranch_tcp/)'s options.

  ## Options
    * `:net` - If using `:inet` (IPv4 only - the default) or `:inet6` (IPv6)
    * `:ip` - The IP to bind the server to.
      Must be either a tuple in the format `{a, b, c, d}` with each value in `0..255` for IPv4,
      or a tuple in the format `{a, b, c, d, e, f, g, h}` with each value in `0..65535` for IPv6,
      or a tuple in the format `{:local, path}` for a unix socket at the given `path`.
      If both `:net` and `:ip` options are given, make sure they are compatible
      (i.e. give a IPv4 for `:inet` and IPv6 for `:inet6`). The default is to listen on all interfaces.
    * `:ipv6_v6only` - If true, only bind on IPv6 addresses (default: `false`).
    * `:max_connection_age` - Milliseconds after which the server closes a connection
      gracefully (default: `:infinity`). The server sends GOAWAY, lets open RPCs finish and
      refuses new ones. The age has up to ±10% jitter, picked once for each connection, and
      counts from the first RPC on the connection. See the "Connection age" guide.
    * `:max_connection_age_grace` - Milliseconds that open RPCs have to finish after GOAWAY.
      Then the server closes the connection and ends the RPCs that remain (default: `:infinity`,
      which keeps Cowboy's `goaway_complete_timeout`, 3000 ms). This also applies when the
      server stops.
    * `:max_connection_idle` - Milliseconds that a connection has no open RPC before the server
      closes it gracefully (default: `:infinity`). The close is the same as for
      `:max_connection_age`. The time counts from the end of the last RPC. It has no jitter.
      A9 counts from the creation of the connection. This option counts from the first RPC,
      because Cowboy has no hook before the first stream. A connection that sends no RPC is
      not closed by this option. Only Cowboy's `:idle_timeout`, which defaults to `:infinity`
      here, limits it. See the "Connection age" guide.
    * `:send_timeout` - How long `GRPC.Server.send_reply/3` waits for the client to read earlier
      replies, in milliseconds, before it raises a `:deadline_exceeded` `GRPC.RPCError`
      (default: `:infinity`). A call with a deadline waits no longer than the deadline.
    * `:max_stream_buffer_size` and `:max_connection_buffer_size` - Cowboy options that limit the
      bytes queued for a client that doesn't read, per stream and per connection (defaults: 8,000,000
      and 16,000,000). `send_reply/3` waits once either is reached.

  Accepted sockets have `TCP_NODELAY` on, which is Ranch's default for both `ranch_tcp` and `ranch_ssl`. Small
  writes go out at once instead of waiting on Nagle's algorithm and delayed ACKs.
  """
  @impl true
  def start(endpoint, servers, port, opts) do
    [_ref, _trans_opts, proto_opts] =
      start_args = cowboy_start_args(endpoint, servers, port, opts)

    start_func = if cred_opts(proto_opts), do: :start_tls, else: :start_clear

    case apply(:cowboy, start_func, start_args) do
      {:ok, pid} ->
        port = :ranch.get_port(servers_name(endpoint, servers))
        {:ok, pid, port}

      {:error, reason} = error ->
        log_start_error(endpoint, servers, port, reason)
        error

      other ->
        other
    end
  end

  @doc """
  Return a child_spec to start server. See `GRPC.Server.Adapters.Cowboy.start/4` for a list of supported options.
  """
  def child_spec(endpoint, servers, port, opts) do
    [ref, trans_opts, proto_opts] = cowboy_start_args(endpoint, servers, port, opts)
    trans_opts = Map.put(trans_opts, :connection_type, :supervisor)

    cred_opts = cred_opts(proto_opts)

    {transport, protocol} =
      if cred_opts do
        {:ranch_ssl, :cowboy_tls}
      else
        {:ranch_tcp, :cowboy_clear}
      end

    scheme = if cred_opts, do: :https, else: :http

    # Ideally, we would just update Ranch, but compatibility issues with cowboy hold us back on this
    # So we just support both child spec versions here instead
    case :ranch.child_spec(ref, transport, trans_opts, protocol, proto_opts) do
      {ref, mfa, type, timeout, kind, modules} ->
        %{
          id: ref,
          start: wrap_start(scheme, endpoint, servers, port, mfa),
          restart: type,
          shutdown: timeout,
          type: kind,
          modules: modules
        }

      %{start: mfa} = child_spec ->
        %{child_spec | start: wrap_start(scheme, endpoint, servers, port, mfa)}
    end
  end

  # Wrap the real mfa to log when the server starts, or why it did not
  defp wrap_start(scheme, endpoint, servers, port, mfa) do
    {__MODULE__, :start_link, [scheme, endpoint, servers, port, mfa]}
  end

  # spec: :supervisor.mfargs doesn't work
  def start_link(scheme, endpoint, servers, port, {m, f, [ref | _] = a}) do
    case apply(m, f, a) do
      {:ok, pid} ->
        Logger.info(running_info(scheme, endpoint, servers, ref))
        {:ok, pid}

      {:error, reason} = error ->
        case log_start_error(endpoint, servers, port, reason) do
          nil -> error
          message -> {:error, {:listen_failed, message, reason}}
        end

      other ->
        other
    end
  end

  @impl true
  def stop(endpoint, servers) do
    :cowboy.stop_listener(servers_name(endpoint, servers))
  end

  def read_body(%{pid: pid}) do
    Handler.read_full_body(pid)
  end

  def reading_stream(%{pid: pid}) do
    Stream.unfold(%{pid: pid, need_more: true, buffer: <<>>}, fn acc -> read_stream(acc) end)
  end

  defp read_stream(%{buffer: <<>>, finished: true}), do: nil

  defp read_stream(%{pid: pid, buffer: buffer, need_more: true} = s) do
    case Handler.read_body(pid) do
      {:ok, data} ->
        new_data = buffer <> data
        new_s = %{pid: pid, finished: true, need_more: false, buffer: new_data}
        read_stream(new_s)

      {:more, data} ->
        data = buffer <> data
        new_s = s |> Map.put(:need_more, false) |> Map.put(:buffer, data)
        read_stream(new_s)
    end
  end

  defp read_stream(%{buffer: buffer} = s) do
    case GRPC.Message.get_message(buffer) do
      {message, rest} ->
        new_s = s |> Map.put(:buffer, rest)
        {message, new_s}

      _ ->
        read_stream_next(s)
    end
  end

  defp read_stream_next(%{finished: true}) do
    raise GRPC.RPCError, status: :internal, message: "The request body ends inside a message"
  end

  defp read_stream_next(s), do: read_stream(Map.put(s, :need_more, true))

  @impl true
  def send_reply(%{pid: pid} = payload, data, opts) do
    http_transcode = Keyword.get(opts, :http_transcode)
    opts = Keyword.put(opts, :send_timeout, Handler.send_timeout(payload))
    Handler.stream_body(pid, data, opts, :nofin, http_transcode)
  end

  @impl true
  def send_headers(%{pid: pid}, headers) do
    Handler.stream_reply(pid, 200, headers)
  end

  def set_headers(%{pid: pid}, headers) do
    Handler.set_resp_headers(pid, headers)
  end

  def set_resp_trailers(%{pid: pid}, trailers) do
    Handler.set_resp_trailers(pid, trailers)
  end

  def send_trailers(%{pid: pid}, trailers) do
    Handler.stream_trailers(pid, trailers)
  end

  def get_headers(%{pid: pid}) do
    Handler.get_headers(pid)
  end

  def get_peer(%{pid: pid}) do
    Handler.get_peer(pid)
  end

  def get_cert(%{pid: pid}) do
    Handler.get_cert(pid)
  end

  def get_qs(%{pid: pid}) do
    Handler.get_qs(pid)
  end

  def get_bindings(%{pid: pid}) do
    Handler.get_bindings(pid)
  end

  def set_compressor(%{pid: pid}, compressor) do
    Handler.set_compressor(pid, compressor)
  end

  # The handler takes the send timeout from its route options, not from the adapter options.
  defp put_send_timeout(opts, adapter_opts) do
    case Keyword.get(adapter_opts, :send_timeout, :infinity) do
      timeout when timeout == :infinity or (is_integer(timeout) and timeout >= 0) ->
        Keyword.put(opts, :send_timeout, timeout)

      other ->
        raise ArgumentError,
              "invalid :send_timeout #{inspect(other)}, expected :infinity or a non-negative integer in milliseconds"
    end
  end

  defp build_handlers(endpoint, servers, opts) do
    Enum.flat_map(servers, fn {_name, server_mod} = server ->
      routes = server_mod.__meta__(:routes)
      Enum.map(routes, &build_route(&1, endpoint, server, opts))
    end)
  end

  defp build_route({:grpc, path}, endpoint, server, opts) do
    {path, GRPC.Server.Adapters.Cowboy.Handler, {endpoint, server, path, Enum.into(opts, %{})}}
  end

  defp build_route({:http_transcode, {_method, path, match}}, endpoint, server, opts) do
    {match, GRPC.Server.Adapters.Cowboy.Handler, {endpoint, server, path, Enum.into(opts, %{})}}
  end

  defp cowboy_start_args(endpoint, servers, port, opts) do
    # Custom handler to be able to listen in the same port, more info:
    # https://github.com/containous/traefik/issues/6211
    {adapter_opts, opts} = Keyword.pop(opts, :adapter_opts, [])
    status_handler = Keyword.get(adapter_opts, :status_handler)

    handlers = build_handlers(endpoint, servers, put_send_timeout(opts, adapter_opts))

    handlers =
      if status_handler do
        [status_handler | handlers]
      else
        handlers
      end

    dispatch = GRPC.Server.Adapters.Cowboy.Router.compile([{:_, handlers}])
    dispatch_key = dispatch_key(endpoint, servers)
    :persistent_term.put(dispatch_key, dispatch)

    idle_timeout = Keyword.get(adapter_opts, :idle_timeout) || :infinity
    num_acceptors = Keyword.get(adapter_opts, :num_acceptors) || @default_num_acceptors
    max_connections = Keyword.get(adapter_opts, :max_connections) || @default_max_connections

    # https://ninenines.eu/docs/en/cowboy/2.7/manual/cowboy_http2/
    merged_adapter_opts =
      Map.merge(
        %{
          env: %{dispatch: {:persistent_term, dispatch_key}},
          idle_timeout: idle_timeout,
          inactivity_timeout: idle_timeout,
          settings_timeout: idle_timeout,
          stream_handlers: [:grpc_stream_h],
          # The default option is small
          # https://github.com/ninenines/cowboy/issues/1398
          # If there are 1000 streams in one connection, then 1000/s frames per stream.
          max_received_frame_rate: {10_000_000, 10_000},
          max_reset_stream_rate: {10_000, 10_000}
        },
        Enum.into(adapter_opts, %{})
      )
      |> put_connection_window_default()
      |> ConnectionAge.configure()

    [
      servers_name(endpoint, servers),
      %{
        num_acceptors: num_acceptors,
        max_connections: max_connections,
        socket_opts: socket_opts(port, adapter_opts)
      },
      merged_adapter_opts
    ]
  end

  # HTTP/2 default stream window (RFC 9113 §6.9.2), which cowboy advertises unless
  # `initial_stream_window_size` is set.
  @default_stream_window 65_535
  @max_window 0x7FFFFFFF
  @connection_window_streams 16

  # A stream's request data is credited back only as its handler reads it, so a stream whose
  # handler stops reading also holds its share of the connection window. With the connection
  # window equal to the stream window, one or two stalled streams take all of it and every other
  # stream on the connection waits. Unless set, the connection window is 16 stream windows.
  defp put_connection_window_default(opts) do
    stream_window = Map.get(opts, :initial_stream_window_size, @default_stream_window)
    window = min(@connection_window_streams * stream_window, @max_window)

    opts
    |> Map.put_new(:initial_connection_window_size, window)
    |> Map.put_new(:connection_window_margin_size, window)
  end

  defp socket_opts(port, opts) do
    socket_opts = [port: port]

    # https://ninenines.eu/docs/en/ranch/1.7/manual/ranch_tcp/
    socket_opts =
      Enum.reduce(opts, socket_opts, fn
        {k, v}, acc when k in [:ip, :ipv6_v6only] and not is_nil(v) -> [{k, v} | acc]
        {:net, v}, acc when not is_nil(v) -> [v | acc]
        _, acc -> acc
      end)

    cred_opts = cred_opts(opts)

    if cred_opts do
      cred_opts.ssl ++
        [
          # These NPN/ALPN options are hardcoded in :cowboy.start_tls/3 (when calling start/3),
          # but not in :ranch.child_spec/5 (when calling child_spec/3). We must make sure they
          # are always provided.
          {:next_protocols_advertised, ["h2", "http/1.1"]},
          {:alpn_preferred_protocols, ["h2", "http/1.1"]}
          | socket_opts
        ]
    else
      socket_opts
    end
  end

  defp cred_opts(opts) do
    Kernel.get_in(opts, [:cred])
  end

  # Logs why the listener did not start, and returns the message, when the reason is a listen
  # failure. Ranch reports it as `{:listen_error, ref, reason}` nested in supervisor errors,
  # or as the bare reason from `:cowboy.start_clear/3`.
  defp log_start_error(endpoint, servers, port, reason) do
    case listen_error_reason(reason) do
      nil ->
        nil

      listen_reason ->
        message =
          "Could not start #{servers_name(endpoint, servers)}: " <>
            listen_error_message(port, listen_reason)

        Logger.error(message)
        message
    end
  end

  defp listen_error_message(port, :eaddrinuse), do: "port #{port} is already in use (eaddrinuse)"

  defp listen_error_message(port, reason) do
    "cannot listen on port #{port}: #{:inet.format_error(reason)} (#{reason})"
  end

  # :eaddrinuse is looked up anywhere in the term, so it is found whatever
  # the supervisor tree around the listener wraps it in.
  defp listen_error_reason(reason) do
    if contains?(reason, :eaddrinuse), do: :eaddrinuse, else: find_listen_error(reason)
  end

  defp find_listen_error({:listen_error, _ref, reason}) when is_atom(reason), do: reason

  defp find_listen_error(term) when is_tuple(term),
    do: term |> Tuple.to_list() |> find_listen_error()

  defp find_listen_error(term) when is_list(term), do: Enum.find_value(term, &find_listen_error/1)
  defp find_listen_error(_), do: nil

  defp contains?(term, term), do: true
  defp contains?(term, needle) when is_tuple(term), do: contains?(Tuple.to_list(term), needle)
  defp contains?(term, needle) when is_list(term), do: Enum.any?(term, &contains?(&1, needle))
  defp contains?(_, _), do: false

  defp running_info(scheme, endpoint, servers, ref) do
    {addr, port} = :ranch.get_addr(ref)

    addr_str =
      case addr do
        :undefined ->
          raise "undefined address for ranch server"

        addr ->
          "#{:inet.ntoa(addr)}:#{port}"
      end

    "Running #{servers_name(endpoint, servers)} with Cowboy using #{scheme}://#{addr_str}"
  end

  # Without an endpoint, `Module.concat(nil, Dispatch)` is one key for all listeners.
  # Each listener needs its own routes, so the key uses the listener name.
  defp dispatch_key(nil, servers), do: {__MODULE__, :dispatch, servers_name(nil, servers)}
  defp dispatch_key(endpoint, _servers), do: Module.concat(endpoint, Dispatch)

  defp servers_name(nil, servers) do
    servers |> Map.values() |> Enum.map(fn s -> inspect(s) end) |> Enum.join(",")
  end

  defp servers_name(endpoint, _) do
    inspect(endpoint)
  end
end
