defmodule GRPC.Server.Drainer do
  @moduledoc """
  Process to drain connections.

  When using `GRPC.Server.Supervisor`, this drainer will be added to the supervisor.
  If you have your own supervisor, you need to add this child after all endpoints.
  See `GRPC.Server.Supervisor` for examples.

  ## Options
    * `:shutdown` - How long to wait for connections to drain. Defaults to 10000(ms).
    * `:drain_check_interval` - How frequently to check if a listener's connections have been drained.
      Defaults to 1000ms.
    * `:adapter` - server adapter. Defaults to `GRPC.Adapter.Cowboy`.

  Refer to https://ninenines.eu/docs/en/ranch/2.0/guide/connection_draining/
  and https://github.com/elixir-plug/plug_cowboy/blob/master/lib/plug/cowboy/drainer.ex
  """

  use GenServer
  @default_adapter GRPC.Adapter.Cowboy
  require Logger

  @doc false
  @spec child_spec(opts :: Keyword.t()) :: Supervisor.child_spec()
  def child_spec({endpoint, opts}) when is_list(opts) do
    child_spec(endpoint, opts)
  end

  def child_spec(endpoint) do
    child_spec(endpoint, [])
  end

  def child_spec(endpoint, opts) when is_list(opts) do
    {spec_opts, opts} = Keyword.split(opts, [:id, :shutdown])

    Supervisor.child_spec(
      %{
        id: __MODULE__,
        start: {__MODULE__, :start_link, [endpoint, opts]},
        shutdown: 10000
      },
      spec_opts
    )
  end

  @doc false
  def start_link(endpoint, opts) do
    GenServer.start_link(__MODULE__, {endpoint, opts})
  end

  @doc false
  @impl true
  def init({endpoint, opts} = args) do
    Process.flag(:trap_exit, true)
    {:ok, args}
  end

  @doc false
  @impl true
  def terminate(:normal, {endpoint, opts}) do
    Logger.warn("Drainer got normal exit.")
  end

  def terminate(reason, {endpoint, opts}) do
    Logger.warn("Drainer terminates for #{inspect(reason)} and begin draining..")
    {adapter, opts} = Keyword.pop(opts, :adapter, @default_adapter)
    adapter.drain(endpoint, opts)
  end
end
