defmodule GRPC.Logger.Server do
  @moduledoc """
  Print log around server rpc calls, like:

      17:18:45.151 [info]  Handled by HelloServer.say_hello
      17:18:45.151 [info]  Response :ok in 11µs

  ## Usage

      defmodule Your.Endpoint do
        use GRPC.Endpoint

        intercept GRPC.Logger.Server, level: :info
      end
  """

  require Logger

  @behaviour GRPC.ServerInterceptor

  @impl true
  def init(opts) do
    level = Keyword.get(opts, :level) || :info
    [level: level]
  end

  @impl true
  def call(req, stream, next, opts) do
    level = Keyword.fetch!(opts, :level)

    if Logger.compare_levels(level, Logger.level()) != :lt do
      Logger.metadata(request_id: Logger.metadata()[:request_id] || stream.request_id)

      Logger.log(level, fn ->
        ["Handled by ", inspect(stream.server), ".", to_string(elem(stream.rpc, 0))]
      end)

      start = System.monotonic_time()
      result = next.(req, stream)
      stop = System.monotonic_time()

      status = elem(result, 0)

      Logger.log(level, fn ->
        diff = System.convert_time_unit(stop - start, :native, :microsecond)

        ["Response ", inspect(status), " in ", formatted_diff(diff)]
      end)

      result
    else
      next.(req, stream)
    end
  end

  def formatted_diff(diff) when diff > 1000, do: [diff |> div(1000) |> Integer.to_string(), "ms"]
  def formatted_diff(diff), do: [Integer.to_string(diff), "µs"]
end
