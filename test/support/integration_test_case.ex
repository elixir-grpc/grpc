defmodule GRPC.Integration.TestCase do
  use ExUnit.CaseTemplate, async: true

  require Logger

  using do
    quote do
      import GRPC.Integration.TestCase
    end
  end

  defmodule ErrorHandler do
    @behaviour :cowboy_handler

    @impl :cowboy_handler
    def init(req, error_code) do
      req = :cowboy_req.reply(error_code, %{"content-type" => "text/plain"}, "", req)

      {:ok, req, error_code}
    end
  end

  def run_error_server(error_code, func, port \\ 0) do
    dispatch =
      :cowboy_router.compile([
        {:_, [{:_, ErrorHandler, error_code}]}
      ])

    {:ok, _} = :cowboy.start_clear("error_server", [port: port], %{env: %{dispatch: dispatch}})
    port = :ranch.get_port("error_server")

    try do
      func.(port)
      :ok
    after
      :ok = :cowboy.stop_listener("error_server")
    end
  end

  def run_server(servers, func, port \\ 0, opts \\ []) do
    {:ok, _pid, port} =
      start_supervised(%{
        id: {GRPC.Server, System.unique_integer([:positive])},
        start: {GRPC.Server, :start, [servers, port, opts]},
        type: :worker,
        restart: :permanent,
        shutdown: 500
      })

    try do
      func.(port)
    after
      :ok = GRPC.Server.stop(servers)
    end
  end

  def run_endpoint(endpoint, func, port \\ 0) do
    {:ok, _pid, port} =
      start_supervised(%{
        id: {GRPC.Server, System.unique_integer([:positive])},
        start: {GRPC.Server, :start_endpoint, [endpoint, port]},
        type: :worker,
        restart: :permanent,
        shutdown: 500
      })

    try do
      func.(port)
    after
      :ok = GRPC.Server.stop_endpoint(endpoint, [])
    end
  end

  def reconnect_server(server, port, retry \\ 3) do
    result = GRPC.Server.start(server, port)

    case result do
      {:ok, _, ^port} ->
        result

      {:error, :eaddrinuse} ->
        Logger.warning("Got eaddrinuse when reconnecting to #{server}:#{port}. retry: #{retry}")

        if retry >= 1 do
          Process.sleep(500)
          reconnect_server(server, port, retry - 1)
        else
          result
        end

      _ ->
        result
    end
  end

  def attach_events(event_names) do
    test_pid = self()

    handler_id = "handler-#{inspect(test_pid)}"

    :telemetry.attach_many(
      handler_id,
      event_names,
      fn name, measurements, metadata, [] ->
        send(test_pid, {name, measurements, metadata})
      end,
      []
    )

    on_exit(fn ->
      :telemetry.detach(handler_id)
    end)
  end
end
