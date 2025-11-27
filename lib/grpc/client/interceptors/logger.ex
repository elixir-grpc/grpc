defmodule GRPC.Client.Interceptors.Logger do
  @moduledoc """
  Print log around client rpc calls, like

      17:13:33.021 [info]  Call helloworld.Greeter.say_hello -> :ok (58 ms)
      17:13:33.021 [error]  Call helloworld.Greeter.say_hello -> %GRPC.RPCError{status: 3, message: "Invalid argument"} (58 ms)

  ## Options

    * `:level` - the desired log level. Defaults to `:info`

  ## Usage

      {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [GRPC.Client.Interceptors.Logger])

  ## Usage with custom level

      {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [{GRPC.Client.Interceptors.Logger, level: :warning}])
  """

  @behaviour GRPC.Client.Interceptor

  require Logger

  @impl GRPC.Client.Interceptor
  def init(opts) do
    level = Keyword.get(opts, :level) || :info
    [level: level]
  end

  @impl GRPC.Client.Interceptor
  def call(%{grpc_type: grpc_type} = stream, req, next, opts) do
    level = Keyword.fetch!(opts, :level)

    if Logger.compare_levels(level, Logger.level()) != :lt do
      start = System.monotonic_time()

      try do
        result = next.(stream, req)
        stop = System.monotonic_time()

        log_result(result, level, grpc_type, stream, start, stop)
        result
      rescue
        error in GRPC.RPCError ->
          stop = System.monotonic_time()
          log_error(error, stream, start, stop)

          reraise error, __STACKTRACE__
      end
    else
      next.(stream, req)
    end
  end

  defp log_error(error, stream, start, stop) do
    diff = System.convert_time_unit(stop - start, :native, :microsecond)

    Logger.log(:error, fn ->
      [
        "Call ",
        stream.service_name,
        ".",
        to_string(elem(stream.rpc, 0)),
        " -> ",
        inspect(error),
        " (",
        formatted_diff(diff),
        ")"
      ]
    end)
  end

  defp log_result(result, level, grpc_type, stream, start, stop) do
    case grpc_type do
      :unary ->
        status = elem(result, 0)

        diff = System.convert_time_unit(stop - start, :native, :microsecond)

        Logger.log(level, fn ->
          [
            "Call ",
            stream.service_name,
            ".",
            to_string(elem(stream.rpc, 0)),
            " -> ",
            inspect(status),
            " (",
            formatted_diff(diff),
            ")"
          ]
        end)

      _otherwise ->
        Logger.log(level, fn ->
          ["Call ", to_string(elem(stream.rpc, 0)), " of ", stream.service_name]
        end)
    end
  end

  def formatted_diff(diff) when diff > 1000, do: [diff |> div(1000) |> Integer.to_string(), "ms"]
  def formatted_diff(diff), do: [Integer.to_string(diff), "µs"]
end
