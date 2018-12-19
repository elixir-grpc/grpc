defmodule GRPC.Integration.TestCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      import GRPC.Integration.TestCase,
        only: [run_server: 2, run_server: 3, run_endpoint: 2, run_endpoint: 3]
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
end
