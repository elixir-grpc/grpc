defmodule GRPC.Client.Pool.Server do
  @moduledoc false

  require Logger

  use GenServer

  alias GRPC.Client.Pool.Config
  alias GRPC.Client.Pool.Implementation

  defmodule DeriveAccess do
    defmacro __using__(_opts) do
      quote do
        @behaviour Access
        defdelegate fetch(term, key), to: Map
        defdelegate get(term, key, default), to: Map
        defdelegate get_and_update(term, key, fun), to: Map
        defdelegate pop(term, key), to: Map
      end
    end
  end

  defmodule State do
    use GRPC.Client.Pool.Server.DeriveAccess

    defmodule Channel do
      use GRPC.Client.Pool.Server.DeriveAccess

      defstruct [:id, :open_streams, :channel]

      @type id :: reference()

      @type t :: %__MODULE__{
              id: id(),
              open_streams: non_neg_integer(),
              channel: GRPC.Channel.t()
            }
    end

    defmodule Lease do
      use GRPC.Client.Pool.Server.DeriveAccess

      defstruct [:id, :monitor_ref, :caller_pid]

      @type t :: %__MODULE__{
              id: GRPC.Client.Pool.Server.State.Channel.id(),
              monitor_ref: reference(),
              caller_pid: pid()
            }
    end

    defstruct channels: %{}, config: %{}, leases_by_channel_id: %{}, leases_by_monitor_ref: %{}

    @type t :: %__MODULE__{
            channels: %{Channel.id() => Channel.t()},
            config: GRPC.Client.Pool.Config.t(),
            leases_by_channel_id: %{Channel.id() => [Lease.t()]},
            leases_by_monitor_ref: %{reference() => Lease.t()}
          }
  end

  @spec start_link(Config.t()) :: GenServer.on_start()
  def start_link(%Config{} = config) do
    GenServer.start_link(__MODULE__, %State{config: config}, name: via_tuple(config.pool_ref))
  end

  @impl GenServer
  @spec init(State.t()) :: {:ok, State.t()}
  def init(state) do
    Process.flag(:trap_exit, true)
    {:ok, Implementation.init(state)}
  end

  @impl GenServer
  def handle_call(:take_channel, {caller_pid, _tag} = _from, state) do
    {channel, _monitor_ref, state} = Implementation.take_channel(state, caller_pid)
    {:reply, channel, state}
  end

  @impl GenServer
  def handle_call({:take_channel, channel_id}, {caller_pid, _tag} = _from, state) do
    {channel, _maybe_monitor_ref, state} =
      Implementation.take_channel(state, channel_id, caller_pid)

    {:reply, channel, state}
  end

  @impl GenServer
  def handle_cast({:return_channel, wrapped_channel, caller_pid}, state) do
    {state, monitor_ref} = Implementation.return_channel(state, wrapped_channel, caller_pid)
    if monitor_ref, do: Process.demonitor(monitor_ref, [:flush])
    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:DOWN, ref, :process, _pid, _reason}, state) do
    {state, _monitor_ref} = Implementation.return_channel(state, ref)
    {:noreply, state}
  end

  @impl GenServer
  # Gun sends this when the HTTP/2 session drops (process still alive, connection ended).
  # Treat it the same as a connection crash — remove the channel and its leases.
  def handle_info({:gun_down, pid, _protocol, _reason, _killed_streams}, state) do
    {state, monitor_refs} = Implementation.handle_connection_process_crush(state, pid)
    Enum.each(monitor_refs, &Process.demonitor/1)
    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:gun_up, _pid, _protocol}, state), do: {:noreply, state}

  @impl GenServer
  def handle_info({:elixir_grpc, :connection_down, pid}, state) do
    {state, monitor_refs} = Implementation.handle_connection_process_crush(state, pid)
    Enum.map(monitor_refs, &Process.demonitor/1)
    {:noreply, state}
  end

  @impl GenServer
  # In case that Client process crushes, GRPC ConnectionProcess is crushing as well with
  # GenServer #PID<0.1333.0> terminating
  #   ** (stop) exited in: GenServer.call(#PID<0.1369.0>, {:consume_response, {:headers, [{"content-type", "application/grpc+proto"}, {"date", "Wed, 10 Apr 2024 16:22:50 GMT"}, {"server", "Cowboy"}]}}, 5000)
  #   ** (EXIT) no process: the process is not alive or there's no process currently associated with the given name, possibly because its application isn't started
  #
  # And because our pool is linked with ConnectionProcess, it's crushing as well
  # over here, we want to prevent ourself from crushing, and instead remove the GRPC.Channel from the pool
  #
  # PID received in this info refers to %GRPC.Channel{adapter_payload: %{conn_pid: ^pid}}
  # So what we need to do is:
  #   - search all channels for this pid
  #   - fetch `channel_id`
  #   - remove channel from the list
  #   - remove by channel id from `leases_by_channel_id`
  #   - remove by references (fetched from `leases_by_channel_id`) from `leases_by_monitor_ref`
  #
  def handle_info({:EXIT, pid, _reason}, state) do
    {state, monitor_refs} = Implementation.handle_connection_process_crush(state, pid)
    Enum.map(monitor_refs, &Process.demonitor/1)
    {:noreply, state}
  end

  @impl GenServer
  def terminate(_reason, state) do
    state.channels
    |> Map.values()
    |> Enum.each(fn %State.Channel{channel: ch} -> ch.adapter.disconnect(ch) end)
  end

  defp via_tuple(pool_ref) do
    {:via, Registry, {GRPC.Client.Pool.Registry, {__MODULE__, pool_ref}}}
  end
end
