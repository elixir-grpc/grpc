defmodule GRPC.Logger.Client do
  require Logger

  @moduledoc """
  Print log around client rpc calls, like

      17:13:33.021 [info]  Call say_hello of helloworld.Greeter
      17:13:33.079 [info]  Got :ok in 58ms

  ## Options

    * `:level` - the desired log level. Defaults to `:info`
    * `:accepted_comparators` - a list with the accepted `Logger.compare_levels(configured_level, Logger.level())` results.
    Defaults to `[:lt, :eq]`

  ## Usage

      {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [GRPC.Logger.Client])
      # This will log on `:info` and greater priority
      {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [{GRPC.Logger.Client, level: :info}])
      # This will log only on `:info`
      {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [{GRPC.Logger.Client, level: :info, accepted_comparators: [:eq]}])
      # This will log on `:info` and lower priority
      {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [{GRPC.Logger.Client, level: :info, accepted_comparators: [:eq, :gt]}])
  """

  def init(opts) do
    level = Keyword.get(opts, :level) || :info
    accepted_comparators = Keyword.get(opts, :accepted_comparators) || [:lt, :eq]
    [level: level, accepted_comparators: accepted_comparators]
  end

  def call(%{grpc_type: grpc_type} = stream, req, next, opts) do
    level = opts[:level]
    accepted_comparators = opts[:accepted_comparators]

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

          ["Got ", inspect(status), " in ", GRPC.Logger.Server.formatted_diff(diff)]
        end)
      end

      result
    else
      next.(stream, req)
    end
  end
end
