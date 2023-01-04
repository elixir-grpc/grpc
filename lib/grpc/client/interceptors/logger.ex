defmodule GRPC.Client.Interceptors.Logger do
  @moduledoc """
  Print log around client rpc calls, like

      17:13:33.021 [info]  Call say_hello of helloworld.Greeter
      17:13:33.079 [info]  Got :ok in 58ms

  ## Options

    * `:level` - the desired log level. Defaults to `:info`
    * `:accepted_comparators` - a list with the accepted `Logger.compare_levels(configured_level, Logger.level())` results.
    Defaults to `[:lt, :eq]`

  ## Usage

      {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [GRPC.Client.Interceptors.Logger])
      # This will log on `:info` and greater priority
      {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [{GRPC.Client.Interceptors.Logger, level: :info}])
      # This will log only on `:info`
      {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [{GRPC.Client.Interceptors.Logger, level: :info, accepted_comparators: [:eq]}])
      # This will log on `:info` and lower priority
      {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [{GRPC.Client.Interceptors.Logger, level: :info, accepted_comparators: [:eq, :gt]}])
  """

  require Logger

  @behaviour GRPC.Client.Interceptor

  @impl true
  def init(opts) do
    level = Keyword.get(opts, :level) || :info
    accepted_comparators = Keyword.get(opts, :accepted_comparators) || [:lt, :eq]
    [level: level, accepted_comparators: accepted_comparators]
  end

  @impl true
  def call(%{grpc_type: grpc_type} = stream, req, next, opts) do
    level = Keyword.fetch!(opts, :level)
    accepted_comparators = Keyword.fetch!(opts, :accepted_comparators)

    if Logger.compare_levels(level, Logger.level()) in accepted_comparators do
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
