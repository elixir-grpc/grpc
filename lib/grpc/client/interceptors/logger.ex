defmodule GRPC.Client.Interceptors.Logger do
  @moduledoc """
  Print log around client rpc calls, like

      17:13:33.021 [info]  Call say_hello of helloworld.Greeter
      17:13:33.079 [info]  Got :ok in 58ms

  ## Options

    * `:level` - the desired log level. Defaults to `:info`

  ## Usage

      {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [GRPC.Client.Interceptors.Logger])

  ## Usage with custom level

      {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [{GRPC.Client.Interceptors.Logger, level: :warn}])
  """

  require Logger

  @behaviour GRPC.Client.Interceptor

  @impl true
  def init(opts) do
    level = Keyword.get(opts, :level) || :info
    [level: level]
  end

  @impl true
  def call(%{grpc_type: grpc_type} = stream, req, next, opts) do
    level = Keyword.fetch!(opts, :level)

    if Logger.compare_levels(level, Logger.level()) != :lt do
      Logger.log(level, fn ->
        ["Call ", to_string(elem(stream.rpc, 0)), " of ", stream.service_name]
      end)

      start = System.monotonic_time()
      result = next.(stream, req)
      stop = System.monotonic_time()

      if grpc_type == :unary do
        status = elem(result, 0)

        Logger.log(level, fn ->
          diff = System.convert_time_unit(stop - start, :native, :microsecond)

          ["Got ", inspect(status), " in ", GRPC.Server.Interceptors.Logger.formatted_diff(diff)]
        end)
      end

      result
    else
      next.(stream, req)
    end
  end
end
