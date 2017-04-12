defmodule GRPC.Adapter.Cowboy do
  @moduledoc """
  A server(`GRPC.Server`) adapter using Cowboy.

  Cowboy req will be stored in `:payload` of `GRPC.Server.Stream`.
  """

  require Logger

  @num_acceptors 100

  @spec start(atom, non_neg_integer, keyword) :: {:ok, pid, non_neg_integer}
  def start(server, port, opts) do
    server_args = cowboy_start_args(server, port, opts)
    {:ok, pid} =
      if opts[:cred] do
        apply(:cowboy, :start_tls, server_args)
      else
        apply(:cowboy, :start_clear, server_args)
      end
    port = :ranch.get_port(server)
    {:ok, pid, port}
  end

  @spec child_spec(module, non_neg_integer, Keyword.t) :: Supervisor.Spec.spec
  def child_spec(server, port, opts) do
    args = cowboy_start_args(server, port, opts)
    {ref, mfa, type, timeout, kind, modules} =
      if opts[:cred] do
        tls_ranch_start_args(args)
      else
        clear_ranch_start_args(args)
      end
    scheme = if opts[:cred], do: :https, else: :http
    mfa = {__MODULE__, :start_link, [scheme, server, mfa]}
    {ref, mfa, type, timeout, kind, modules}
  end

  @spec start_link(atom, module, [any]) :: {:ok, pid} | {:error, any}
  def start_link(scheme, server, {m, f, [ref | _] = a}) do
    case apply(m, f, a) do
      {:ok, pid} ->
        Logger.info running_info(scheme, server, ref)
        {:ok, pid}

      {:error, {:shutdown, {_, _, {{_, {:error, :eaddrinuse}}, _}}}} = error ->
        Logger.error [running_info(scheme, server, ref), " failed, port already in use"]
        error

      {:error, _} = error ->
        error
    end
  end

  @spec stop(atom) :: :ok | {:error, :not_found}
  def stop(server) do
    :cowboy.stop_listener(server)
  end

  @spec read_body(GRPC.Client.Stream.t) :: {:ok, binary, GRPC.Client.Stream.t}
  def read_body(%{payload: req} = stream) do
    {:ok, data, req} = read_body("", req)
    {:ok, data, %{stream | payload: req}}
  end

  defp read_body(body, req) do
    case :cowboy_req.read_body(req) do
      {:ok, data, req} -> {:ok, body <> data, req}
      {:more, data, req} -> read_body(body <> data, req)
    end
  end

  @spec reading_stream(GRPC.Client.Stream.t, (binary -> struct)) :: Enumerable.t
  def reading_stream(stream, func) do
    Stream.unfold(stream, fn nil -> nil; %{payload: req} = acc ->
      case :cowboy_req.read_body(req) do
        {:ok, "", _} ->
          nil
        {atom, data, req} when atom == :ok or atom == :more ->
          request = func.(data)
          new_stream = %{acc | payload: req}
          {request, new_stream}
      end
    end)
  end

  @spec stream_send(GRPC.Client.Stream.t, binary) :: any
  def stream_send(%{payload: req}, data) do
    :cowboy_req.stream_body(data, :nofin, req)
  end

  defp cowboy_start_args(server, port, opts) do
    dispatch = :cowboy_router.compile([
      {:_, [{:_, GRPC.Adapter.Cowboy.Handler, {server, opts}}]}
    ])
    [server, @num_acceptors, transport_opts(port, opts),
     %{env: %{dispatch: dispatch}, http2_recv_timeout: :infinity,
       stream_handler: {GRPC.Adapter.Cowboy.StreamHandler, :supervisor}}
    ]
  end

  defp transport_opts(port, opts) do
    list = [port: port]
    list = if opts[:ip], do: [{:ip, opts[:ip]}|list], else: list
    if opts[:cred] do
      tls_cred = opts[:cred].tls
      [{:certfile, tls_cred.cert_path}, {:keyfile, tls_cred.key_path}|list]
    else
      list
    end
  end

  defp clear_ranch_start_args([ref, num_acceptors, trans_opts, proto_opts])
        when is_integer(num_acceptors) and num_acceptors > 0 do
    trans_opts = [connection_type(proto_opts) | trans_opts]
    :ranch.child_spec(ref, num_acceptors, :ranch_tcp, trans_opts, :cowboy_clear, proto_opts)
  end

  defp tls_ranch_start_args([ref, num_acceptors, trans_opts, proto_opts])
        when is_integer(num_acceptors) and num_acceptors > 0 do
    trans_opts = [
      connection_type(proto_opts),
      {:next_protocols_advertised, ["h2", "http/1.1"]},
      {:alpn_preferred_protocols, ["h2", "http/1.1"]}
    | trans_opts]
    :ranch.child_spec(ref, num_acceptors, :ranch_ssl, trans_opts, :cowboy_tls, proto_opts)
  end

  defp connection_type(proto_opts) do
    {_, type} = Map.get(proto_opts, :stream_handler, {:cowboy_stream_h, :supervisor})
    {:connection_type, type}
  end

  defp running_info(scheme, server, ref) do
    {addr, port} = :ranch.get_addr(ref)
    addr_str = :inet.ntoa(addr)
    "Running #{inspect server} with Cowboy using #{scheme}://#{addr_str}:#{port}"
  end
end
