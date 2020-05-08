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

  defmodule ErrorHandler do
    @behaviour :gen_event

    def init(test_case_pid), do: {:ok, %{test_case_pid: test_case_pid}}

    def handle_call(_, state) do
      {:ok, :ok, state}
    end

    def handle_event({:error_report, _gl, {_pid, _type, [message | _]}}, state)
        when is_list(message) do
      error_info = message[:error_info]
      send(state.test_case_pid, {:error_report, error_info})

      {:ok, state}
    end

    def handle_event({_level, _gl, _event}, state) do
      {:ok, state}
    end
  end

  def attach_error_handler do
    :error_logger.add_report_handler(ErrorHandler, self())

    on_exit(fn ->
      :error_logger.delete_report_handler(ErrorHandler)
    end)
  end
end
