defmodule GRPC.Logger do
  require Logger

  def call(_req, stream, next) do
    call(stream, next)
  end
  def call(stream, next) do
    level = :info
    Logger.log(level, fn ->
      [inspect(stream.server), ".", to_string(elem(stream.rpc, 0))]
    end)

    start = System.monotonic_time()

    result = next.(stream)
    status = elem(result, 0)

    Logger.log(level, fn ->
      stop = System.monotonic_time()
      diff = System.convert_time_unit(stop - start, :native, :micro_seconds)

      ["Got ", inspect(status), " in ", formatted_diff(diff)]
    end)

    result
  end

  defp formatted_diff(diff) when diff > 1000, do: [diff |> div(1000) |> Integer.to_string(), "ms"]
  defp formatted_diff(diff), do: [Integer.to_string(diff), "µs"]
end
