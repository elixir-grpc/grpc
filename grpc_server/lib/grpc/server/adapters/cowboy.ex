defmodule GRPC.Server.Adapters.Cowboy do
  @moduledoc """
  A server (`b:GRPC.Server.Adapter`) adapter using `:cowboy`.

  Cowboy requests will be stored in the `:payload` field of the `GRPC.Server.Stream`.
  """

  @behaviour GRPC.Server.Adapter

  require Logger
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

      other ->
        other
    end
  end

  @doc """
  Return a child_spec to start server. See `GRPC.Server.Adapters.Cowboy.start/4` for a list of supported options.
  """
  @spec child_spec(atom(), %{String.t() => [module()]}, non_neg_integer(), Keyword.t()) ::
          Supervisor.child_spec()
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

    # Ideally, we would just update Ranch, but compatibility issues with cowboy hold us back on this
    # So we just support both child spec versions here instead
    case :ranch.child_spec(ref, transport, trans_opts, protocol, proto_opts) do
      {ref, mfa, type, timeout, kind, modules} ->
        scheme = if cred_opts, do: :https, else: :http
        # Wrap real mfa to print starting log
        wrapped_mfa = {__MODULE__, :start_link, [scheme, endpoint, servers, mfa]}

        %{
          id: ref,
          start: wrapped_mfa,
          restart: type,
          shutdown: timeout,
          type: kind,
          modules: modules
        }

      child_spec when is_map(child_spec) ->
        child_spec
    end
  end

  # spec: :supervisor.mfargs doesn't work
  @spec start_link(atom(), atom(), %{String.t() => [module()]}, any()) ::
          {:ok, pid()} | {:error, any()}
  def start_link(scheme, endpoint, servers, {m, f, [ref | _] = a}) do
    case apply(m, f, a) do
      {:ok, pid} ->
        Logger.info(running_info(scheme, endpoint, servers, ref))
        {:ok, pid}

      {:error, {:shutdown, {_, _, {{_, {:error, :eaddrinuse}}, _}}}} = error ->
        Logger.error([
          running_info(scheme, endpoint, servers, ref),
          " failed, port already in use"
        ])

        error

      {:error, _} = error ->
        error
    end
  end

  @impl true
  def stop(endpoint, servers) do
    :cowboy.stop_listener(servers_name(endpoint, servers))
  end

  @spec read_body(GRPC.Server.Adapter.state()) :: {:ok, binary()}
  def read_body(%{pid: pid}) do
    Handler.read_full_body(pid)
  end

  @spec reading_stream(GRPC.Server.Adapter.state()) :: Enumerable.t()
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
        read_stream(Map.put(s, :need_more, true))
    end
  end

  @impl true
  def send_reply(%{pid: pid}, data, opts) do
    http_transcode = Keyword.get(opts, :http_transcode)
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

    handlers = build_handlers(endpoint, servers, opts)

    handlers =
      if status_handler do
        [status_handler | handlers]
      else
        handlers
      end

    dispatch = GRPC.Server.Adapters.Cowboy.Router.compile([{:_, handlers}])
    dispatch_key = Module.concat(endpoint, Dispatch)
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

  defp servers_name(nil, servers) do
    servers |> Map.values() |> Enum.map(fn s -> inspect(s) end) |> Enum.join(",")
  end

  defp servers_name(endpoint, _) do
    inspect(endpoint)
  end
end
