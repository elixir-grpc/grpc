defmodule GRPC.Adapter.Cowboy do
  @moduledoc """
  A server(`GRPC.Server`) adapter using Cowboy.

  Cowboy req will be stored in `:payload` of `GRPC.Server.Stream`.
  """

  @spec start(atom, non_neg_integer, keyword) :: {:ok, pid, non_neg_integer}
  def start(server, port, opts) do
    dispatch = :cowboy_router.compile([
      {:_, [{:_, GRPC.Adapter.Cowboy.Handler, {server, opts}}]}
    ])
    transport_opts = [port: port]
    transport_opts = if opts[:ip], do: [{:ip, opts[:ip]}|transport_opts], else: transport_opts
    {:ok, _} = :cowboy.start_clear(server, 100,
      transport_opts, %{env: %{dispatch: dispatch},
                      stream_handler: {GRPC.Adapter.Cowboy.StreamHandler, :supervisor},
                      http2_recv_timeout: :infinity }
    )
    {:ok, sup_pid} = GRPC.Adapter.Cowboy.ServerSup.start_link
    port = :ranch.get_port(server)
    {:ok, sup_pid, port}
  end

  @spec stop(atom) :: :ok | {:error, :not_found}
  def stop(server) do
    :cowboy.stop_listener(server)
  end

  @spec read_body(GRPC.Client.Stream.t) :: {:ok, binary, GRPC.Client.Stream.t}
  def read_body(%{payload: req} = stream) do
    {:ok, data, req} = :cowboy_req.read_body(req)
    {:ok, data, %{stream | payload: req}}
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
end
