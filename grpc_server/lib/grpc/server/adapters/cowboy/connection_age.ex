defmodule GRPC.Server.Adapters.Cowboy.ConnectionAge do
  @moduledoc false
  # Closes an HTTP/2 connection after a maximum age, in the way of gRPC proposal A9:
  # https://github.com/grpc/proposal/blob/master/A9-server-side-conn-mgt.md
  #
  # `grpc_stream_h:init/3` runs in the Cowboy connection process, so `self()` is the
  # connection. On the first stream of a connection, `stream_init/2` starts a helper process
  # with a timer. When the timer ends, the helper calls `:sys.terminate/3` on the connection.
  # Cowboy then sends GOAWAY, stops new streams and drains the open ones
  # (`cowboy_http2:system_terminate/4`). `goaway_complete_timeout` ends the drain.

  @flag {__MODULE__, :started}
  @jitter 0.1
  @sys_timeout 5_000

  @doc """
  Checks and normalizes the connection age options, and maps the grace to Cowboy's
  `goaway_complete_timeout`. Called with Cowboy's protocol options.
  """
  def configure(opts) do
    check_option!(opts, :max_connection_age)
    check_option!(opts, :max_connection_age_grace)

    case Map.get(opts, :max_connection_age_grace, :infinity) do
      :infinity -> opts
      grace -> Map.put(opts, :goaway_complete_timeout, grace)
    end
  end

  @doc """
  Called by `grpc_stream_h:init/3` for each stream. Starts the age timer on the first
  HTTP/2 stream of a connection.
  """
  def stream_init(%{version: :"HTTP/2"}, %{max_connection_age: age}) when is_integer(age) do
    unless Process.put(@flag, true) do
      conn = self()
      delay = jitter(age, :rand.uniform())
      spawn(fn -> wait_and_close(conn, delay) end)
    end

    :ok
  end

  def stream_init(_req, _opts), do: :ok

  @doc """
  Spreads `age` by up to ±10%. `unit` is a number in `0..1`, `0.5` leaves `age` as it is.
  """
  def jitter(age, unit) when is_integer(age) and unit >= 0 and unit <= 1 do
    round(age * (1 - @jitter + 2 * @jitter * unit))
  end

  defp wait_and_close(conn, delay) do
    ref = Process.monitor(conn)

    receive do
      {:DOWN, ^ref, :process, ^conn, _reason} -> :ok
    after
      delay ->
        # The connection may exit while it handles the call.
        try do
          :sys.terminate(conn, :shutdown, @sys_timeout)
        catch
          :exit, _ -> :ok
        end
    end
  end

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
