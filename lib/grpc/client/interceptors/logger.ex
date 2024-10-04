defmodule GRPC.Client.Interceptors.Logger do
  @moduledoc """
  Print log around client rpc calls, like

      17:13:33.021 [info]  Call helloworld.Greeter.say_hello -> :ok (58 ms)

  ## Options

    * `:level` - the desired log level. Defaults to `:info`

  ## Usage

      {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [GRPC.Client.Interceptors.Logger])

  ## Usage with custom level

      {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [{GRPC.Client.Interceptors.Logger, level: :warn}])
  """

  @behaviour GRPC.Client.Interceptor

  require Logger

  @impl GRPC.Client.Interceptor
  def init(opts) do
    level = Keyword.get(opts, :level) || :info
    [level: level]
  end

  @impl GRPC.Client.Interceptor
  def call(stream = %{grpc_type: grpc_type}, req, next, opts) do
    {time, result} = :timer.tc(next, [stream, req])
    level = Keyword.fetch!(opts, :level)

    if Logger.compare_levels(level, Logger.level()) != :lt do
      case grpc_type do
        :unary ->
          status = elem(result, 0)

          Logger.log(level, fn ->
            [
              "Call ",
              stream.service_name,
              ".",
              to_string(elem(stream.rpc, 0)),
              " -> ",
              inspect(status),
              " (",
              :io_lib.format("~.3f", [time / 1000]),
              " ms)"
            ]
          end)

          result

        _otherwise ->
          Logger.log(level, fn ->
            ["Call ", to_string(elem(stream.rpc, 0)), " of ", stream.service_name]
          end)

          result
      end
    else
      next.(stream, req)
    end
  end
end
