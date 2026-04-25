defmodule GRPC.Client.DNSResolver do
  @moduledoc """
  Dedicated process for periodic DNS re-resolution.

  Linked to the parent `GRPC.Client.Connection` GenServer. Owns the
  resolve loop, backoff, rate limiting, and telemetry — keeping the
  Connection process focused on channel management.

  Sends `{:resolver_update, result}` to the Connection after each resolve,
  where `result` matches the return type of `GRPC.Client.Resolver.resolve/1`.

  ## Resolver contract

  The `:resolver` option must be a module implementing the
  `GRPC.Client.Resolver` behaviour — specifically the `c:GRPC.Client.Resolver.resolve/1`
  callback, which returns:

      {:ok, %{addresses: [%{address: String.t(), port: integer()}], service_config: term()}}
      | {:error, term()}
  """
  use GenServer
  require Logger

  @resolve_stop_event [:grpc, :client, :resolve, :stop]
  @resolve_error_event [:grpc, :client, :resolve, :error]

  defstruct [
    :connection_pid,
    :resolver_module,
    :target,
    :resolve_interval,
    :base_resolve_interval,
    :max_resolve_interval,
    :min_resolve_interval,
    :last_resolve_at,
    :timer_ref
  ]

  @doc """
  Starts the resolver process, linked to the calling process.

  Options:
    * `:connection_pid` — pid of the owning Connection GenServer
    * `:resolver` — module implementing `GRPC.Client.Resolver` behaviour
    * `:target` — the DNS target string
    * `:resolve_interval` — base interval between resolves (ms)
    * `:max_resolve_interval` — backoff cap (ms)
    * `:min_resolve_interval` — rate-limit floor (ms)
  """
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl GenServer
  def init(opts) do
    resolve_interval = Keyword.fetch!(opts, :resolve_interval)

    state = %__MODULE__{
      connection_pid: Keyword.fetch!(opts, :connection_pid),
      resolver_module: Keyword.fetch!(opts, :resolver),
      target: Keyword.fetch!(opts, :target),
      resolve_interval: resolve_interval,
      base_resolve_interval: resolve_interval,
      max_resolve_interval: Keyword.fetch!(opts, :max_resolve_interval),
      min_resolve_interval: Keyword.fetch!(opts, :min_resolve_interval),
      last_resolve_at: nil
    }

    {:ok, schedule(state)}
  end

  @impl GenServer
  def handle_info(:resolve, state) do
    state = do_resolve(state)
    {:noreply, schedule(state)}
  end

  def handle_info(:resolve_now, state) do
    now = System.monotonic_time(:millisecond)

    if rate_limited?(state, now) do
      Logger.debug("DNS re-resolution for #{state.target} rate-limited, skipping")
      {:noreply, state}
    else
      state = do_resolve(state)
      {:noreply, state}
    end
  end

  defp do_resolve(state) do
    start_time = System.monotonic_time()
    result = state.resolver_module.resolve(state.target)
    duration = System.monotonic_time() - start_time
    now = System.monotonic_time(:millisecond)

    state = %{state | last_resolve_at: now}

    case result do
      {:ok, %{addresses: []}} ->
        emit_error(duration, state.target, :empty_addresses)

        Logger.warning(
          "DNS re-resolution returned empty addresses for #{state.target}, keeping existing"
        )

        send(state.connection_pid, {:resolver_update, result})
        backoff(state)

      {:ok, %{addresses: addresses}} ->
        emit_success(duration, state.target, length(addresses))
        send(state.connection_pid, {:resolver_update, result})
        reset_backoff(state)

      {:error, reason} ->
        emit_error(duration, state.target, reason)
        Logger.warning("DNS re-resolution failed for #{state.target}: #{inspect(reason)}")
        send(state.connection_pid, {:resolver_update, result})
        backoff(state)
    end
  end

  defp schedule(state) do
    if state.timer_ref, do: Process.cancel_timer(state.timer_ref)
    ref = Process.send_after(self(), :resolve, state.resolve_interval)
    %{state | timer_ref: ref}
  end

  defp backoff(state) do
    new_interval = min(state.resolve_interval * 2, state.max_resolve_interval)
    %{state | resolve_interval: new_interval}
  end

  defp reset_backoff(state) do
    %{state | resolve_interval: state.base_resolve_interval}
  end

  defp rate_limited?(%{last_resolve_at: nil}, _now), do: false

  defp rate_limited?(%{last_resolve_at: last, min_resolve_interval: min}, now) do
    now - last < min
  end

  defp emit_success(duration, target, address_count) do
    :telemetry.execute(@resolve_stop_event, %{duration: duration}, %{
      target: target,
      address_count: address_count
    })
  end

  defp emit_error(duration, target, reason) do
    :telemetry.execute(@resolve_error_event, %{duration: duration}, %{
      target: target,
      reason: reason,
      address_count: 0
    })
  end
end
