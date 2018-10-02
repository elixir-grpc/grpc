defmodule GRPC.Adapter.Cowboy do
  @moduledoc false

  # A server(`GRPC.Server`) adapter using Cowboy.
  # Cowboy req will be stored in `:payload` of `GRPC.Server.Stream`.

  require Logger
  alias GRPC.Adapter.Cowboy.Handler, as: Handler

  @default_num_acceptors 20

  # Only used in starting a server manually using `GRPC.Server.start(servers)`
  @spec start(GRPC.Server.servers_map(), non_neg_integer, keyword) :: {:ok, pid, non_neg_integer}
  def start(servers, port, opts) do
    start_args = cowboy_start_args(servers, port, opts)
    start_func = if opts[:cred], do: :start_tls, else: :start_clear

    {:ok, pid} = apply(:cowboy, start_func, start_args)

    port = :ranch.get_port(servers_name(servers))
    {:ok, pid, port}
  end

  @spec child_spec(GRPC.Server.servers_map(), non_neg_integer, Keyword.t()) ::
          Supervisor.Spec.spec()
  def child_spec(servers, port, opts) do
    [ref, trans_opts, proto_opts] = cowboy_start_args(servers, port, opts)
    trans_opts = Map.put(trans_opts, :connection_type, :supervisor)

    {transport, protocol} =
      if opts[:cred] do
        {:ranch_ssl, :cowboy_tls}
      else
        {:ranch_tcp, :cowboy_clear}
      end

    {ref, mfa, type, timeout, kind, modules} =
      :ranch.child_spec(ref, transport, trans_opts, protocol, proto_opts)

    scheme = if opts[:cred], do: :https, else: :http
    # Wrap real mfa to print starting log
    mfa = {__MODULE__, :start_link, [scheme, servers, mfa]}
    {ref, mfa, type, timeout, kind, modules}
  end

  # spec: :supervisor.mfargs doesn't work
  @spec start_link(atom, GRPC.Server.servers_map(), any) :: {:ok, pid} | {:error, any}
  def start_link(scheme, servers, {m, f, [ref | _] = a}) do
    case apply(m, f, a) do
      {:ok, pid} ->
        Logger.info(running_info(scheme, servers, ref))
        {:ok, pid}

      {:error, {:shutdown, {_, _, {{_, {:error, :eaddrinuse}}, _}}}} = error ->
        Logger.error([running_info(scheme, servers, ref), " failed, port already in use"])
        error

      {:error, _} = error ->
        error
    end
  end

  @spec stop(GRPC.Server.servers_map()) :: :ok | {:error, :not_found}
  def stop(servers) do
    :cowboy.stop_listener(servers_name(servers))
  end

  @spec read_body(GRPC.Adapter.Cowboy.Handler.state()) :: {:ok, binary}
  def read_body(%{pid: pid}) do
    Handler.read_full_body(pid)
  end

  @spec reading_stream(GRPC.Adapter.Cowboy.Handler.state()) :: Enumerable.t()
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

  @spec send_reply(GRPC.Adapter.Cowboy.Handler.state(), binary) :: any
  def send_reply(%{pid: pid}, data) do
    Handler.stream_body(pid, data, :nofin)
  end

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

  defp cowboy_start_args(servers, port, opts) do
    dispatch =
      :cowboy_router.compile([
        {:_, [{:_, GRPC.Adapter.Cowboy.Handler, {servers, Enum.into(opts, %{})}}]}
      ])

    idle_timeout = Keyword.get(opts, :idle_timeout, :infinity)

    [
      servers_name(servers),
      %{
        num_acceptors: @default_num_acceptors,
        socket_opts: socket_opts(port, opts)
      },
      %{
        env: %{dispatch: dispatch},
        inactivity_timeout: idle_timeout,
        settings_timeout: idle_timeout,
        stream_handlers: [:grpc_stream_h]
      }
    ]
  end

  defp socket_opts(port, opts) do
    socket_opts = [port: port]
    socket_opts = if opts[:ip], do: [{:ip, opts[:ip]} | socket_opts], else: socket_opts

    if opts[:cred] do
      opts[:cred].ssl ++
        [
          {:next_protocols_advertised, ["h2", "http/1.1"]},
          {:alpn_preferred_protocols, ["h2", "http/1.1"]}
          | socket_opts
        ]
    else
      socket_opts
    end
  end

  defp running_info(scheme, servers, ref) do
    {addr, port} = :ranch.get_addr(ref)

    addr_str =
      case addr do
        :local ->
          port

        addr ->
          "#{:inet.ntoa(addr)}:#{port}"
      end

    "Running #{servers_name(servers)} with Cowboy using #{scheme}://#{addr_str}"
  end

  defp servers_name(servers) do
    servers |> Map.values() |> Enum.map(fn s -> inspect(s) end) |> Enum.join(",")
  end
end
