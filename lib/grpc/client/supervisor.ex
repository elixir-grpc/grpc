defmodule GRPC.Client.Supervisor do
  @moduledoc """
  A DynamicSupervisor responsible for managing gRPC client connections (`GRPC.Client.Connection`).

  This supervisor allows you to dynamically start and stop gRPC client connections at runtime.
  Each connection is run as a separate `GenServer` under this supervisor, which ensures proper
  supervision and isolation between connections.

  ## Starting the Supervisor

  Typically, you start this supervisor as part of your application's supervision tree:

      children = [
        {GRPC.Client.Supervisor, []}
      ]

      opts = [strategy: :one_for_one, name: MyApp.Supervisor]
      Supervisor.start_link(children, opts)

  You can also start it manually in scripts or test environments:

      {:ok, _pid} = DynamicSupervisor.start_link(strategy: :one_for_one, name: GRPC.Client.Supervisor)

  ## Supervision Strategy

  This supervisor uses `:one_for_one` strategy:

    * If a connection process crashes, only that process is restarted.
    * Other running connections remain unaffected.

  ## Establishing a gRPC Connection

  To create a new gRPC connection, you typically use the `GRPC.Stub.connect/1` function,
  which internally starts a `GRPC.Client.Connection` process under this supervisor. For example:

      iex> {:ok, ch} = GRPC.Stub.connect("127.0.0.1:50051")
      iex> Grpc.Testing.TestService.Stub.empty_call(ch, %{})

  ## Notes

    * You can dynamically start multiple connections under the supervisor for different targets.
    * Each connection runs in isolation as its own GenServer.
  """
  use DynamicSupervisor

  def start_link(opts) do
    DynamicSupervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end
end
