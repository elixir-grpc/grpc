defmodule GRPC.Adapter.Cowboy do
  @moduledoc """
  A server(`GRPC.Server`) adapter using Cowboy.

  Cowboy req will be stored in `:payload` of `GRPC.Server.Stream`.
  """

  @num_acceptors 100

  @spec start(atom, non_neg_integer, keyword) :: {:ok, pid, non_neg_integer}
  def start(server, port, opts) do
    server_args = start_args(server, port, opts)
    {:ok, pid} =
      if opts[:cred] do
        apply(:cowboy, :start_tls, server_args)
      else
        apply(:cowboy, :start_clear, server_args)
      end
    port = :ranch.get_port(server)
    {:ok, pid, port}
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

  defp start_args(server, port, opts) do
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
end
