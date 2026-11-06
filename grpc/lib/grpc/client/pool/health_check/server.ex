defmodule GRPC.Client.Pool.HealthCheck.Server do
  @moduledoc """
  Job of this module is to send periodic health-check messages to single GRPC Channel.
  This server has to be alive for the whole lifespan of a given channels connection.
  In case that channel get's closed, dies or exits in some other manner, it should terminate gracefully.
  It overrides public API and invokes internal logic of Pool as it needs granular controll
  over what connection/channel_id it is using (as we want to monitor specific channel per single HC server)
  """

  use GenServer

  require Logger

  alias GRPC.Client.Pool.Server.State.Channel

  defmodule State do
    defstruct [:channel_id, :conn_pid, :pool_ref, :health_check_enabled?]

    @type t :: %__MODULE__{
            channel_id: GRPC.Client.Pool.Server.State.Channel.id(),
            conn_pid: Process.dest(),
            pool_ref: reference(),
            health_check_enabled?: boolean()
          }
  end

  @connection_health_check_interval 10 * 60 * 1_000
  @call_timeout 7_000

  @spec start_link(%{
          :channel_id => Channel.id(),
          :conn_pid => Process.dest(),
          :pool_ref => reference()
        }) ::
          :ignore | {:error, any()} | {:ok, pid()}
  def start_link(%{channel_id: channel_id, conn_pid: conn_pid, pool_ref: pool_ref}) do
    state = %State{
      channel_id: channel_id,
      conn_pid: conn_pid,
      pool_ref: pool_ref,
      health_check_enabled?: true
    }

    GenServer.start_link(__MODULE__, state, name: via_tuple(channel_id, pool_ref))
  end

  @impl GenServer
  def init(%State{conn_pid: conn_pid} = state) do
    Process.monitor(conn_pid)
    Process.send_after(self(), :send_health_check, @connection_health_check_interval)
    {:ok, state}
  end

  defp send_health_check(%State{channel_id: channel_id, pool_ref: pool_ref} = state) do
    case with_chan(pool_ref, channel_id, &send_message/1) do
      # Status 12 https://grpc.github.io/grpc/core/md_doc_statuscodes.html
      # UNIMPLEMENTED | 12 | The operation is not implemented or is not supported/enabled in this service.
      {:error, %GRPC.RPCError{status: 12}} ->
        Logger.error(
          "Received gRPC error not implemented when sending health-check message. Will disable periodic messages."
        )

        %{state | health_check_enabled?: false}

      {:error, error} ->
        Logger.warning(
          "Received gRPC error when sending health-check message. Will continue to send periodic messages.",
          error: inspect(error)
        )

        state

      {:ok, _} ->
        state

      :ok ->
        state
    end
  end

  @impl GenServer
  # For connection health-check it's enough for us to send ping message
  # if connection is healthy it will return some value, if it's not healthy
  # the connection will close, GRPC ConnectionProcess will exit and we will remove it
  # in :DOWN handler
  def handle_info(:send_health_check, %State{} = state) do
    state = send_health_check(state)

    # Here we could potentially exit, if HC get's disables self
    # due to it not being implemented on server side, but I think it's
    # okay to leave process alive as it will self terminate one connection it
    # was supposed to HC dies.
    if state.health_check_enabled? do
      Process.send_after(self(), :send_health_check, @connection_health_check_interval)
    end

    {:noreply, state}
  end

  # In case that connection process exists, we will receive :DOWN message
  # once connection process is dead there is no reason for us to be alive
  # so we just exit normally and never get restarted
  @impl GenServer
  def handle_info({:DOWN, _ref, :process, _pid, _reason}, state) do
    {:stop, :normal, state}
  end

  defp with_chan(pool_ref, channel_id, callback) do
    case Registry.lookup(GRPC.Client.Pool.Registry, {GRPC.Client.Pool.Server, pool_ref}) do
      [{pool_pid, _}] ->
        pool_pid
        |> GenServer.call({:take_channel, channel_id}, @call_timeout)
        |> maybe_execute_callback(callback, pool_pid)

      [] ->
        :ok
    end
  end

  defp maybe_execute_callback(
         %GRPC.Client.Pool.Server.State.Channel{} = wrapped_channel,
         callback,
         pid
       ),
       do: do_execute_callback(wrapped_channel, callback, pid)

  defp maybe_execute_callback(nil, _callback, _pid), do: :ok

  defp do_execute_callback(
         %GRPC.Client.Pool.Server.State.Channel{channel: channel} = wrapped_channel,
         callback,
         pid
       ) do
    try do
      callback.(channel)
    after
      GenServer.cast(pid, {:return_channel, wrapped_channel, self()})
    end
  end

  defp via_tuple(channel_id, pool_ref) do
    {:via, Registry, {GRPC.Client.Pool.Registry, {__MODULE__, channel_id, pool_ref}}}
  end

  defp send_message(%GRPC.Channel{} = channel) do
    GRPC.Health.V1.Health.Stub.check(channel, %GRPC.Health.V1.HealthCheckRequest{
      service: "healthcheck"
    })
  end
end
