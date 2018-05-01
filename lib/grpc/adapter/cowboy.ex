defmodule GRPC.Adapter.Cowboy do
  @moduledoc false

  # A server(`GRPC.Server`) adapter using Cowboy.
  # Cowboy req will be stored in `:payload` of `GRPC.Server.Stream`.

  require Logger
  alias GRPC.Adapter.Cowboy.Handler, as: Handler

  @default_num_acceptors 20

  # Only used in starting a server manually using `GRPC.Server.start(servers)`
  @spec start(atom, GRPC.Server.servers_map(), non_neg_integer, keyword) :: {:ok, pid, non_neg_integer}
  def start(endpoint, servers, port, opts) do
    server_args = cowboy_start_args(endpoint, servers, port, opts)

    {:ok, pid} =
      if opts[:cred] do
        apply(:cowboy, :start_tls, server_args)
      else
        apply(:cowboy, :start_clear, server_args)
      end

    port = :ranch.get_port(servers_name(endpoint, servers))
    {:ok, pid, port}
  end

  @spec child_spec(atom, GRPC.Server.servers_map(), non_neg_integer, Keyword.t()) ::
          Supervisor.Spec.spec()
  def child_spec(endpoint, servers, port, opts) do
    args = cowboy_start_args(endpoint, servers, port, opts)

    {ref, mfa, type, timeout, kind, modules} =
      if opts[:cred] do
        tls_ranch_start_args(args)
      else
        clear_ranch_start_args(args)
      end

    scheme = if opts[:cred], do: :https, else: :http
    # Wrap real mfa to print starting log
    mfa = {__MODULE__, :start_link, [scheme, endpoint, servers, mfa]}
    {ref, mfa, type, timeout, kind, modules}
  end

  # spec: :supervisor.mfargs doesn't work
  @spec start_link(atom, atom, GRPC.Server.servers_map(), any) :: {:ok, pid} | {:error, any}
  def start_link(scheme, endpoint, servers, {m, f, [ref | _] = a}) do
    case apply(m, f, a) do
      {:ok, pid} ->
        Logger.info(running_info(scheme, endpoint, servers, ref))
        {:ok, pid}

      {:error, {:shutdown, {_, _, {{_, {:error, :eaddrinuse}}, _}}}} = error ->
        Logger.error([running_info(scheme, endpoint, servers, ref), " failed, port already in use"])
        error

      {:error, _} = error ->
        error
    end
  end

  @spec stop(atom, GRPC.Server.servers_map()) :: :ok | {:error, :not_found}
  def stop(endpoint, servers) do
    :cowboy.stop_listener(servers_name(endpoint, servers))
  end

  @spec read_body(GRPC.Adapter.Cowboy.Handler.state()) :: {:ok, binary}
  def read_body(%{pid: pid}) do
    Handler.read_full_body(pid)
  end

  @spec reading_stream(GRPC.Adapter.Cowboy.Handler.state(), ([binary] -> [struct])) :: Enumerable.t()
  def reading_stream(%{pid: pid}, func) do
    Stream.unfold(%{pid: pid, frames: [], buffer: ""}, fn acc -> read_stream(acc, func) end)
  end

  defp read_stream(nil, _), do: nil
  defp read_stream(%{frames: [], finished: true}, _), do: nil

  defp read_stream(%{frames: [curr | rest]} = s, _) do
    {curr, Map.put(s, :frames, rest)}
  end

  defp read_stream(%{pid: pid, frames: [], buffer: buffer} = s, func) do
    case Handler.read_body(pid) do
      {:ok, data} ->
        new_data = buffer <> data
        if byte_size(new_data) > 0 do
          [request | rest] = func.(new_data)
          new_s = s |> Map.put(:frames, rest) |> Map.put(:finished, true)
          {request, new_s}
        else
          nil
        end

      {:more, data} ->
        data = buffer <> data

        if GRPC.Message.complete?(data) do
          [request | rest] = func.(data)
          new_s = s |> Map.put(:frames, rest) |> Map.put(:buffer, "")
          {request, new_s}
        else
          read_stream(Map.put(s, :buffer, data), func)
        end
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

  defp cowboy_start_args(endpoint, servers, port, opts) do
    dispatch =
      :cowboy_router.compile([
        {:_, [{:_, GRPC.Adapter.Cowboy.Handler, {endpoint, servers, opts}}]}
      ])

    [
      servers_name(endpoint, servers),
      transport_opts(port, opts),
      %{
        env: %{dispatch: dispatch},
        inactivity_timeout: :infinity,
        stream_handlers: [:grpc_stream_h]
      }
    ]
  end

  defp transport_opts(port, opts) do
    list = [port: port, num_acceptors: @default_num_acceptors]
    list = if opts[:ip], do: [{:ip, opts[:ip]} | list], else: list

    if opts[:cred] do
      opts[:cred].ssl ++ list
    else
      list
    end
  end

  defp clear_ranch_start_args([ref, trans_opts, proto_opts]) do
    trans_opts = [connection_type(proto_opts) | trans_opts]
    :ranch.child_spec(ref, :ranch_tcp, trans_opts, :cowboy_clear, proto_opts)
  end

  defp tls_ranch_start_args([ref, trans_opts, proto_opts]) do
    trans_opts = [
      connection_type(proto_opts),
      {:next_protocols_advertised, ["h2", "http/1.1"]},
      {:alpn_preferred_protocols, ["h2", "http/1.1"]}
      | trans_opts
    ]

    :ranch.child_spec(ref, :ranch_ssl, trans_opts, :cowboy_tls, proto_opts)
  end

  defp connection_type(_) do
    {:connection_type, :supervisor}
  end

  defp running_info(scheme, endpoint, servers, ref) do
    {addr, port} = :ranch.get_addr(ref)
    addr_str = :inet.ntoa(addr)
    "Running #{servers_name(endpoint, servers)} with Cowboy using #{scheme}://#{addr_str}:#{port}"
  end

  defp servers_name(nil, servers) do
    servers |> Map.values() |> Enum.map(fn s -> inspect(s) end) |> Enum.join(",")
  end
  defp servers_name(endpoint, _) do
    inspect(endpoint)
  end
end
