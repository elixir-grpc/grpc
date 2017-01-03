defmodule GRPC.Integration.TestCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      import GRPC.Integration.TestCase, only: [run_server: 2, run_server: 3]
    end
  end

  def run_server(server, func, port \\ 0) do
    {:ok, _pid, port} = GRPC.Server.start(server, port)
    try do
      func.(port)
    after
      :ok = GRPC.Server.stop(server)
    end
  end
end
