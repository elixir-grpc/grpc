defmodule GRPC.Integration.TestCase do
  use ExUnit.CaseTemplate, async: true

  require Logger

  using do
    quote do
      import GRPC.Integration.TestCase
    end
  end

  def run_server(servers, func, port \\ 0) do
    {:ok, _pid, port} = GRPC.Server.start(servers, port)

    try do
      func.(port)
    after
      :ok = GRPC.Server.stop(servers)
    end
  end

  def run_endpoint(endpoint, func, port \\ 0) do
    {:ok, _pid, port} = GRPC.Server.start_endpoint(endpoint, port)

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
        Logger.warn("Got eaddrinuse when reconnecting to #{server}:#{port}. retry: #{retry}")

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
end
