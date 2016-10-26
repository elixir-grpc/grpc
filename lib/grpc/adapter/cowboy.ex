defmodule GRPC.Adapter.Cowboy do
  def start(server, host, port, opts) do
    dispatch = :cowboy_router.compile([
      {host, [{:_, GRPC.Adapter.Cowboy.Handler, {server, opts}}]}
    ])
    {:ok, _} = :cowboy.start_clear(server, 100,
      [port: port], %{:env => %{dispatch: dispatch},
                      :stream_handler => {GRPC.Adapter.Cowboy.StreamHandler, :supervisor}}
    )
    GRPC.Adapter.Cowboy.ServerSup.start_link
  end

  def stop(server) do
    :cowboy.stop_listener(server)
  end

  def read_body(%{payload: req} = stream) do
    {:ok, data, req} = :cowboy_req.read_body(req)
    {:ok, data, %{stream | payload: req}}
  end

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

  def stream_send(%{payload: req}, data) do
    :cowboy_req.stream_body(data, :nofin, req)
  end
end
