defmodule Benchmark.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {GRPC.Client.Supervisor, []}
    ]

    opts = [strategy: :one_for_one, name: Benchmark.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
