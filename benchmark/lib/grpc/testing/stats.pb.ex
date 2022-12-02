defmodule Grpc.Testing.ServerStats do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :time_elapsed, 1, type: :double, json_name: "timeElapsed"
  field :time_user, 2, type: :double, json_name: "timeUser"
  field :time_system, 3, type: :double, json_name: "timeSystem"
  field :total_cpu_time, 4, type: :uint64, json_name: "totalCpuTime"
  field :idle_cpu_time, 5, type: :uint64, json_name: "idleCpuTime"
  field :cq_poll_count, 6, type: :uint64, json_name: "cqPollCount"
  field :core_stats, 7, type: Grpc.Core.Stats, json_name: "coreStats"
end

defmodule Grpc.Testing.HistogramParams do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :resolution, 1, type: :double
  field :max_possible, 2, type: :double, json_name: "maxPossible"
end

defmodule Grpc.Testing.HistogramData do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :bucket, 1, repeated: true, type: :uint32
  field :min_seen, 2, type: :double, json_name: "minSeen"
  field :max_seen, 3, type: :double, json_name: "maxSeen"
  field :sum, 4, type: :double
  field :sum_of_squares, 5, type: :double, json_name: "sumOfSquares"
  field :count, 6, type: :double
end

defmodule Grpc.Testing.RequestResultCount do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :status_code, 1, type: :int32, json_name: "statusCode"
  field :count, 2, type: :int64
end

defmodule Grpc.Testing.ClientStats do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :latencies, 1, type: Grpc.Testing.HistogramData
  field :time_elapsed, 2, type: :double, json_name: "timeElapsed"
  field :time_user, 3, type: :double, json_name: "timeUser"
  field :time_system, 4, type: :double, json_name: "timeSystem"

  field :request_results, 5,
    repeated: true,
    type: Grpc.Testing.RequestResultCount,
    json_name: "requestResults"

  field :cq_poll_count, 6, type: :uint64, json_name: "cqPollCount"
  field :core_stats, 7, type: Grpc.Core.Stats, json_name: "coreStats"
end