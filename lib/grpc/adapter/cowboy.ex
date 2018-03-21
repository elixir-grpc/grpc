defmodule GRPC.Adapter.Cowboy do
  @moduledoc false

  # A server(`GRPC.Server`) adapter using Cowboy.
  # Cowboy req will be stored in `:payload` of `GRPC.Server.Stream`.

  require Logger

  @default_num_acceptors 100

  @spec start(GRPC.Server.servers_map(), non_neg_integer, keyword) :: {:ok, pid, non_neg_integer}
  def start(servers, port, opts) do
    server_args = cowboy_start_args(servers, port, opts)

    {:ok, pid} =
      if opts[:cred] do
        apply(:cowboy, :start_tls, server_args)
      else
        apply(:cowboy, :start_clear, server_args)
      end

    port = :ranch.get_port(servers_name(servers))
    {:ok, pid, port}
  end

  @spec child_spec(GRPC.Server.servers_map(), non_neg_integer, Keyword.t()) ::
          Supervisor.Spec.spec()
  def child_spec(servers, port, opts) do
    args = cowboy_start_args(servers, port, opts)

    {ref, mfa, type, timeout, kind, modules} =
      if opts[:cred] do
        tls_ranch_start_args(args)
      else
        clear_ranch_start_args(args)
      end

    scheme = if opts[:cred], do: :https, else: :http
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

  @spec read_body(GRPC.Client.Stream.t()) :: {:ok, binary, GRPC.Client.Stream.t()}
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

  @spec reading_stream(GRPC.Client.Stream.t(), ([binary] -> [struct])) :: Enumerable.t()
  def reading_stream(stream, func) do
    Stream.unfold({stream, %{frames: [], buffer: ""}}, fn acc -> read_stream(acc, func) end)
  end

  defp read_stream(nil, _), do: nil
  defp read_stream({_, %{frames: [], finished: true}}, _), do: nil

  defp read_stream({stream, %{frames: [curr | rest]} = s}, _) do
    new_s = Map.put(s, :frames, rest)
    {curr, {stream, new_s}}
  end

  defp read_stream({%{payload: req0} = st, %{frames: [], buffer: buffer} = s}, func) do
    case :cowboy_req.read_body(req0) do
      {:ok, "", _} ->
        nil

      {:ok, data, req} ->
        [request | rest] = func.(buffer <> data)
        new_stream = %{st | payload: req}
        new_s = s |> Map.put(:frames, rest) |> Map.put(:finished, true)
        {request, {new_stream, new_s}}

      {:more, data, req} ->
        data = buffer <> data

        if GRPC.Message.complete?(data) do
          [request | rest] = func.(data)
          new_stream = %{st | payload: req}
          new_s = s |> Map.put(:frames, rest) |> Map.put(:buffer, "")
          {request, {new_stream, new_s}}
        else
          read_stream({%{st | payload: req}, Map.put(s, :buffer, data)}, func)
        end
    end
  end

  @spec send_reply(GRPC.Client.Stream.t(), binary) :: any
  def send_reply(%{payload: req}, data) do
    :cowboy_req.stream_body(data, :nofin, req)
  end

  @doc false
  @spec flow_control(GRPC.Client.Stream.t(), non_neg_integer) :: any
  def flow_control(%{payload: req}, size) do
    pid = req[:pid]
    send(pid, {{pid, req[:streamid]}, {:flow, size}})
  end

  def has_sent_headers?(%{payload: req}) do
    req[:has_sent_resp] != nil
  end

  def send_headers(%{payload: req} = stream, headers) do
    req = :cowboy_req.stream_reply(200, headers, req)
    %{stream | payload: req}
  end

  def set_headers(%{payload: req} = stream, headers) do
    req = :cowboy_req.set_resp_headers(headers, req)
    %{stream | payload: req}
  end

  def send_trailers(%{payload: req} = stream, trailers) do
    :cowboy_req.stream_trailers(trailers, req)
    %{stream | payload: req}
  end

  def get_headers(%{payload: req}) do
    :cowboy_req.headers(req)
  end

  defp cowboy_start_args(servers, port, opts) do
    dispatch =
      :cowboy_router.compile([
        {:_, [{:_, GRPC.Adapter.Cowboy.Handler, {servers, opts}}]}
      ])

    [
      servers_name(servers),
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

  defp running_info(scheme, servers, ref) do
    {addr, port} = :ranch.get_addr(ref)
    addr_str = :inet.ntoa(addr)
    "Running #{servers_name(servers)} with Cowboy using #{scheme}://#{addr_str}:#{port}"
  end

  defp servers_name(servers) do
    servers |> Map.values() |> Enum.map(fn s -> inspect(s) end) |> Enum.join(",")
  end
end
