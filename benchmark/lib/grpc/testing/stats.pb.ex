defmodule Grpc.Testing.ServerStats do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          time_elapsed: float,
          time_user: float,
          time_system: float,
          total_cpu_time: non_neg_integer,
          idle_cpu_time: non_neg_integer,
          cq_poll_count: non_neg_integer,
          core_stats: Grpc.Core.Stats.t() | nil
        }
  defstruct [
    :time_elapsed,
    :time_user,
    :time_system,
    :total_cpu_time,
    :idle_cpu_time,
    :cq_poll_count,
    :core_stats
  ]

  field :time_elapsed, 1, type: :double
  field :time_user, 2, type: :double
  field :time_system, 3, type: :double
  field :total_cpu_time, 4, type: :uint64
  field :idle_cpu_time, 5, type: :uint64
  field :cq_poll_count, 6, type: :uint64
  field :core_stats, 7, type: Grpc.Core.Stats
end

defmodule Grpc.Testing.HistogramParams do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          resolution: float,
          max_possible: float
        }
  defstruct [:resolution, :max_possible]

  field :resolution, 1, type: :double
  field :max_possible, 2, type: :double
end

defmodule Grpc.Testing.HistogramData do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          bucket: [non_neg_integer],
          min_seen: float,
          max_seen: float,
          sum: float,
          sum_of_squares: float,
          count: float
        }
  defstruct [:bucket, :min_seen, :max_seen, :sum, :sum_of_squares, :count]

  field :bucket, 1, repeated: true, type: :uint32
  field :min_seen, 2, type: :double
  field :max_seen, 3, type: :double
  field :sum, 4, type: :double
  field :sum_of_squares, 5, type: :double
  field :count, 6, type: :double
end

defmodule Grpc.Testing.RequestResultCount do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          status_code: integer,
          count: integer
        }
  defstruct [:status_code, :count]

  field :status_code, 1, type: :int32
  field :count, 2, type: :int64
end

defmodule Grpc.Testing.ClientStats do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          latencies: Grpc.Testing.HistogramData.t() | nil,
          time_elapsed: float,
          time_user: float,
          time_system: float,
          request_results: [Grpc.Testing.RequestResultCount.t()],
          cq_poll_count: non_neg_integer,
          core_stats: Grpc.Core.Stats.t() | nil
        }
  defstruct [
    :latencies,
    :time_elapsed,
    :time_user,
    :time_system,
    :request_results,
    :cq_poll_count,
    :core_stats
  ]

  field :latencies, 1, type: Grpc.Testing.HistogramData
  field :time_elapsed, 2, type: :double
  field :time_user, 3, type: :double
  field :time_system, 4, type: :double
  field :request_results, 5, repeated: true, type: Grpc.Testing.RequestResultCount
  field :cq_poll_count, 6, type: :uint64
  field :core_stats, 7, type: Grpc.Core.Stats
end
