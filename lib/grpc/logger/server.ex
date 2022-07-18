defmodule GRPC.Logger.Server do
  @moduledoc """
  Print log around server rpc calls, like:

      17:18:45.151 [info]  Handled by HelloServer.say_hello
      17:18:45.151 [info]  Response :ok in 11µs

  ## Options

    * `:level` - the desired log level. Defaults to `:info`
    * `:accepted_comparators` - a list with the accepted `Logger.compare_levels(configured_level, Logger.level())` results.
    Defaults to `[:lt, :eq]`

  ## Usage

      defmodule Your.Endpoint do
        use GRPC.Endpoint

        intercept GRPC.Logger.Server, level: :info
      end

      defmodule Your.Endpoint do
        use GRPC.Endpoint

        # logs on :info and higher priority (warn, error...)
        intercept GRPC.Logger.Server, level: :info, accepted_comparators: [:lt, :eq]
      end

      defmodule Your.Endpoint do
        use GRPC.Endpoint

        # logs only on :error
        intercept GRPC.Logger.Server, level: :error, accepted_comparators: [:eq]
      end
  """
  require Logger
  @behaviour GRPC.ServerInterceptor

  def init(opts) do
    level = Keyword.get(opts, :level) || :info
    accepted_comparators = Keyword.get(opts, :accepted_comparators) || [:lt, :eq]
    [level: level, accepted_comparators: accepted_comparators]
  end

  def call(req, stream, next, opts) do
    level = Keyword.fetch!(opts, :level)
    accepted_comparators = opts[:accepted_comparators]

    if Logger.compare_levels(level, Logger.level()) in accepted_comparators do
      Logger.metadata(request_id: Logger.metadata()[:request_id] || stream.request_id)

      Logger.log(level, "Handled by #{inspect(stream.server)}.#{elem(stream.rpc, 0)}")

      start = System.monotonic_time()
      result = next.(req, stream)
      stop = System.monotonic_time()

      status = elem(result, 0)
      diff = System.convert_time_unit(stop - start, :native, :microsecond)

      Logger.log(level, "Response #{inspect(status)} in #{formatted_diff(diff)}")

      result
    else
      next.(req, stream)
    end
  end

  def formatted_diff(diff) when diff > 1000, do: [diff |> div(1000) |> Integer.to_string(), "ms"]
  def formatted_diff(diff), do: [Integer.to_string(diff), "µs"]
end
