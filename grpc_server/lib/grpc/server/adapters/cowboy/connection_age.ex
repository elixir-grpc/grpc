defmodule GRPC.Server.Adapters.Cowboy.ConnectionAge do
  @moduledoc false
  # Closes an HTTP/2 connection after a maximum age or a maximum idle time, in the way of
  # gRPC proposal A9:
  # https://github.com/grpc/proposal/blob/master/A9-server-side-conn-mgt.md
  #
  # `grpc_stream_h:init/3` runs in the Cowboy connection process, so `self()` is the
  # connection. On the first stream of a connection, `stream_init/2` starts a helper process
  # with a timer. When the timer ends, the helper calls `:sys.terminate/3` on the connection.
  # Cowboy then sends GOAWAY, stops new streams and drains the open ones
  # (`cowboy_http2:system_terminate/4`). `goaway_complete_timeout` ends the drain.
  #
  # A connection is idle when it has no open stream. `grpc_stream_h` counts the open streams
  # in the connection process, with `stream_opened/0` and `stream_closed/0`. The count and
  # the time of the last close are in an `:atomics` array. The helper reads the array when
  # it wakes. The array has one writer, the connection process, and one reader, the helper.

  @flag {__MODULE__, :started}
  @counters_key {__MODULE__, :counters}
  @jitter 0.1
  @sys_timeout 5_000

  # Slots of the `:atomics` array.
  @open 1
  @closed_at 2

  @doc """
  Checks and normalizes the connection options, and maps the grace to Cowboy's
  `goaway_complete_timeout`. Called with Cowboy's protocol options.
  """
  def configure(opts) do
    check_option!(opts, :max_connection_age)
    check_option!(opts, :max_connection_age_grace)
    check_option!(opts, :max_connection_idle)

    case Map.get(opts, :max_connection_age_grace, :infinity) do
      :infinity -> opts
      grace -> Map.put(opts, :goaway_complete_timeout, grace)
    end
  end

  @doc """
  Called by `grpc_stream_h:init/3` for each stream. Starts the helper on the first
  HTTP/2 stream of a connection, when an age or an idle time is set.
  """
  def stream_init(%{version: :"HTTP/2"}, opts) do
    age = Map.get(opts, :max_connection_age, :infinity)
    idle = Map.get(opts, :max_connection_idle, :infinity)

    if (is_integer(age) or is_integer(idle)) and Process.get(@flag) == nil do
      Process.put(@flag, true)
      conn = self()
      now = now()
      age_deadline = if is_integer(age), do: now + jitter(age, :rand.uniform()), else: nil

      idle_config =
        if is_integer(idle) do
          counters = new_counters(now)
          Process.put(@counters_key, counters)

          %{
            max_idle: idle,
            counters: counters,
            test_pid: Map.get(opts, :__max_connection_idle_test_pid__)
          }
        end

      spawn(fn -> watch(conn, age_deadline, idle_config, now) end)
    end

    :ok
  end

  def stream_init(_req, _opts), do: :ok

  @doc """
  Called by `grpc_stream_h:init/3` when a stream is set up. Does nothing when the
  connection has no idle time.
  """
  def stream_opened do
    case Process.get(@counters_key) do
      nil -> :ok
      counters -> stream_opened(counters)
    end
  end

  @doc """
  Called by `grpc_stream_h:terminate/3` when a stream ends. Does nothing when the
  connection has no idle time.
  """
  def stream_closed do
    case Process.get(@counters_key) do
      nil -> :ok
      counters -> stream_closed(counters, now())
    end
  end

  @doc """
  Makes the counters. The connection is idle from `now`.
  """
  def new_counters(now) do
    counters = :atomics.new(2, signed: true)
    :atomics.put(counters, @closed_at, now)
    counters
  end

  def stream_opened(counters) do
    :atomics.add(counters, @open, 1)
  end

  @doc """
  The close time goes in before the count drops. A reader that sees a count of 0
  then sees a close time that is not older than the drop.
  """
  def stream_closed(counters, now) do
    :atomics.put(counters, @closed_at, now)
    :atomics.sub(counters, @open, 1)
  end

  @doc """
  Reads the counters at `now`. Returns `{:open, wait}` when a stream is open, `{:idle, wait}`
  when the connection is idle for less than `max_idle`, and `:expired` when it is idle for
  `max_idle` or more. `wait` is the time, in milliseconds, to the next check.
  """
  def idle_check(counters, max_idle, now) do
    open = :atomics.get(counters, @open)
    closed_at = :atomics.get(counters, @closed_at)

    cond do
      open > 0 -> {:open, max_idle}
      now - closed_at >= max_idle -> :expired
      true -> {:idle, closed_at + max_idle - now}
    end
  end

  @doc """
  Spreads `age` by up to ±10%. `unit` is a number in `0..1`, `0.5` leaves `age` as it is.
  """
  def jitter(age, unit) when is_integer(age) and unit >= 0 and unit <= 1 do
    round(age * (1 - @jitter + 2 * @jitter * unit))
  end

  defp watch(conn, age_deadline, idle_config, now) do
    mon = Process.monitor(conn)

    wait =
      min_wait(
        if(age_deadline, do: age_deadline - now, else: :infinity),
        if(idle_config, do: idle_config.max_idle, else: :infinity)
      )

    loop(conn, mon, age_deadline, idle_config, wait)
  end

  defp loop(conn, mon, age_deadline, idle_config, wait) do
    receive do
      {:DOWN, ^mon, :process, ^conn, _reason} -> :ok
    after
      wait ->
        case wake(age_deadline, idle_config, now()) do
          :close ->
            close(conn)

          {:close, :idle} ->
            pause(idle_config, conn, mon)
            close(conn)

          {:wait, wait} ->
            loop(conn, mon, age_deadline, idle_config, wait)
        end
    end
  end

  defp wake(age_deadline, idle_config, now) do
    age_left = if age_deadline, do: age_deadline - now, else: :infinity

    cond do
      age_left != :infinity and age_left <= 0 ->
        :close

      idle_config == nil ->
        {:wait, age_left}

      true ->
        idle_step(
          idle_config,
          idle_check(idle_config.counters, idle_config.max_idle, now),
          age_left
        )
    end
  end

  defp idle_step(config, :expired, _age_left) do
    notify(config, :idle)
    {:close, :idle}
  end

  defp idle_step(config, {state, wait}, age_left) do
    notify(config, state)
    {:wait, min_wait(wait, age_left)}
  end

  defp min_wait(:infinity, b), do: b
  defp min_wait(a, :infinity), do: a
  defp min_wait(a, b), do: min(a, b)

  defp notify(%{test_pid: nil}, _state), do: :ok

  defp notify(%{test_pid: pid}, state) do
    send(pid, {:idle_check, state})
    :ok
  end

  # A test can hold the helper between the check and the close. A stream can open in
  # this time. The GOAWAY with the highest stream id is first, so Cowboy lets it finish.
  defp pause(%{test_pid: nil}, _conn, _mon), do: :ok

  defp pause(%{test_pid: pid}, conn, mon) do
    helper = self()
    send(pid, {:idle_close, helper})

    receive do
      {:continue, ^helper} -> :ok
      {:DOWN, ^mon, :process, ^conn, _reason} -> :ok
    end
  end

  defp close(conn) do
    # The connection may exit while it handles the call.
    try do
      :sys.terminate(conn, :shutdown, @sys_timeout)
    catch
      :exit, _ -> :ok
    end
  end

  defp now, do: System.monotonic_time(:millisecond)

  defp check_option!(opts, key) do
    case Map.get(opts, key, :infinity) do
      :infinity ->
        :ok

      value when is_integer(value) and value > 0 ->
        :ok

      value ->
        raise ArgumentError,
              "the #{inspect(key)} adapter option must be :infinity or a positive " <>
                "number of milliseconds, got: #{inspect(value)}"
    end
  end
end
