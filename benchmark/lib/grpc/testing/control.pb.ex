defmodule Grpc.Testing.PoissonParams do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          offered_load: float
        }
  defstruct [:offered_load]

  field :offered_load, 1, type: :double
end

defmodule Grpc.Testing.ClosedLoopParams do
  @moduledoc false
  use Protobuf, syntax: :proto3

  defstruct []
end

defmodule Grpc.Testing.LoadParams do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          load: {atom, any}
        }
  defstruct [:load]

  oneof :load, 0
  field :closed_loop, 1, type: Grpc.Testing.ClosedLoopParams, oneof: 0
  field :poisson, 2, type: Grpc.Testing.PoissonParams, oneof: 0
end

defmodule Grpc.Testing.SecurityParams do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          use_test_ca: boolean,
          server_host_override: String.t(),
          cred_type: String.t()
        }
  defstruct [:use_test_ca, :server_host_override, :cred_type]

  field :use_test_ca, 1, type: :bool
  field :server_host_override, 2, type: :string
  field :cred_type, 3, type: :string
end

defmodule Grpc.Testing.ChannelArg do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          value: {atom, any},
          name: String.t()
        }
  defstruct [:value, :name]

  oneof :value, 0
  field :name, 1, type: :string
  field :str_value, 2, type: :string, oneof: 0
  field :int_value, 3, type: :int32, oneof: 0
end

defmodule Grpc.Testing.ClientConfig do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          server_targets: [String.t()],
          client_type: integer,
          security_params: Grpc.Testing.SecurityParams.t() | nil,
          outstanding_rpcs_per_channel: integer,
          client_channels: integer,
          async_client_threads: integer,
          rpc_type: integer,
          load_params: Grpc.Testing.LoadParams.t() | nil,
          payload_config: Grpc.Testing.PayloadConfig.t() | nil,
          histogram_params: Grpc.Testing.HistogramParams.t() | nil,
          core_list: [integer],
          core_limit: integer,
          other_client_api: String.t(),
          channel_args: [Grpc.Testing.ChannelArg.t()],
          threads_per_cq: integer,
          messages_per_stream: integer,
          use_coalesce_api: boolean
        }
  defstruct [
    :server_targets,
    :client_type,
    :security_params,
    :outstanding_rpcs_per_channel,
    :client_channels,
    :async_client_threads,
    :rpc_type,
    :load_params,
    :payload_config,
    :histogram_params,
    :core_list,
    :core_limit,
    :other_client_api,
    :channel_args,
    :threads_per_cq,
    :messages_per_stream,
    :use_coalesce_api
  ]

  field :server_targets, 1, repeated: true, type: :string
  field :client_type, 2, type: Grpc.Testing.ClientType, enum: true
  field :security_params, 3, type: Grpc.Testing.SecurityParams
  field :outstanding_rpcs_per_channel, 4, type: :int32
  field :client_channels, 5, type: :int32
  field :async_client_threads, 7, type: :int32
  field :rpc_type, 8, type: Grpc.Testing.RpcType, enum: true
  field :load_params, 10, type: Grpc.Testing.LoadParams
  field :payload_config, 11, type: Grpc.Testing.PayloadConfig
  field :histogram_params, 12, type: Grpc.Testing.HistogramParams
  field :core_list, 13, repeated: true, type: :int32
  field :core_limit, 14, type: :int32
  field :other_client_api, 15, type: :string
  field :channel_args, 16, repeated: true, type: Grpc.Testing.ChannelArg
  field :threads_per_cq, 17, type: :int32
  field :messages_per_stream, 18, type: :int32
  field :use_coalesce_api, 19, type: :bool
end

defmodule Grpc.Testing.ClientStatus do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          stats: Grpc.Testing.ClientStats.t() | nil
        }
  defstruct [:stats]

  field :stats, 1, type: Grpc.Testing.ClientStats
end

defmodule Grpc.Testing.Mark do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          reset: boolean
        }
  defstruct [:reset]

  field :reset, 1, type: :bool
end

defmodule Grpc.Testing.ClientArgs do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          argtype: {atom, any}
        }
  defstruct [:argtype]

  oneof :argtype, 0
  field :setup, 1, type: Grpc.Testing.ClientConfig, oneof: 0
  field :mark, 2, type: Grpc.Testing.Mark, oneof: 0
end

defmodule Grpc.Testing.ServerConfig do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          server_type: integer,
          security_params: Grpc.Testing.SecurityParams.t() | nil,
          port: integer,
          async_server_threads: integer,
          core_limit: integer,
          payload_config: Grpc.Testing.PayloadConfig.t() | nil,
          core_list: [integer],
          other_server_api: String.t(),
          threads_per_cq: integer,
          resource_quota_size: integer,
          channel_args: [Grpc.Testing.ChannelArg.t()]
        }
  defstruct [
    :server_type,
    :security_params,
    :port,
    :async_server_threads,
    :core_limit,
    :payload_config,
    :core_list,
    :other_server_api,
    :threads_per_cq,
    :resource_quota_size,
    :channel_args
  ]

  field :server_type, 1, type: Grpc.Testing.ServerType, enum: true
  field :security_params, 2, type: Grpc.Testing.SecurityParams
  field :port, 4, type: :int32
  field :async_server_threads, 7, type: :int32
  field :core_limit, 8, type: :int32
  field :payload_config, 9, type: Grpc.Testing.PayloadConfig
  field :core_list, 10, repeated: true, type: :int32
  field :other_server_api, 11, type: :string
  field :threads_per_cq, 12, type: :int32
  field :resource_quota_size, 1001, type: :int32
  field :channel_args, 1002, repeated: true, type: Grpc.Testing.ChannelArg
end

defmodule Grpc.Testing.ServerArgs do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          argtype: {atom, any}
        }
  defstruct [:argtype]

  oneof :argtype, 0
  field :setup, 1, type: Grpc.Testing.ServerConfig, oneof: 0
  field :mark, 2, type: Grpc.Testing.Mark, oneof: 0
end

defmodule Grpc.Testing.ServerStatus do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          stats: Grpc.Testing.ServerStats.t() | nil,
          port: integer,
          cores: integer
        }
  defstruct [:stats, :port, :cores]

  field :stats, 1, type: Grpc.Testing.ServerStats
  field :port, 2, type: :int32
  field :cores, 3, type: :int32
end

defmodule Grpc.Testing.CoreRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  defstruct []
end

defmodule Grpc.Testing.CoreResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          cores: integer
        }
  defstruct [:cores]

  field :cores, 1, type: :int32
end

defmodule Grpc.Testing.Void do
  @moduledoc false
  use Protobuf, syntax: :proto3

  defstruct []
end

defmodule Grpc.Testing.Scenario do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          name: String.t(),
          client_config: Grpc.Testing.ClientConfig.t() | nil,
          num_clients: integer,
          server_config: Grpc.Testing.ServerConfig.t() | nil,
          num_servers: integer,
          warmup_seconds: integer,
          benchmark_seconds: integer,
          spawn_local_worker_count: integer
        }
  defstruct [
    :name,
    :client_config,
    :num_clients,
    :server_config,
    :num_servers,
    :warmup_seconds,
    :benchmark_seconds,
    :spawn_local_worker_count
  ]

  field :name, 1, type: :string
  field :client_config, 2, type: Grpc.Testing.ClientConfig
  field :num_clients, 3, type: :int32
  field :server_config, 4, type: Grpc.Testing.ServerConfig
  field :num_servers, 5, type: :int32
  field :warmup_seconds, 6, type: :int32
  field :benchmark_seconds, 7, type: :int32
  field :spawn_local_worker_count, 8, type: :int32
end

defmodule Grpc.Testing.Scenarios do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          scenarios: [Grpc.Testing.Scenario.t()]
        }
  defstruct [:scenarios]

  field :scenarios, 1, repeated: true, type: Grpc.Testing.Scenario
end

defmodule Grpc.Testing.ScenarioResultSummary do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          qps: float,
          qps_per_server_core: float,
          server_system_time: float,
          server_user_time: float,
          client_system_time: float,
          client_user_time: float,
          latency_50: float,
          latency_90: float,
          latency_95: float,
          latency_99: float,
          latency_999: float,
          server_cpu_usage: float,
          successful_requests_per_second: float,
          failed_requests_per_second: float,
          client_polls_per_request: float,
          server_polls_per_request: float,
          server_queries_per_cpu_sec: float,
          client_queries_per_cpu_sec: float
        }
  defstruct [
    :qps,
    :qps_per_server_core,
    :server_system_time,
    :server_user_time,
    :client_system_time,
    :client_user_time,
    :latency_50,
    :latency_90,
    :latency_95,
    :latency_99,
    :latency_999,
    :server_cpu_usage,
    :successful_requests_per_second,
    :failed_requests_per_second,
    :client_polls_per_request,
    :server_polls_per_request,
    :server_queries_per_cpu_sec,
    :client_queries_per_cpu_sec
  ]

  field :qps, 1, type: :double
  field :qps_per_server_core, 2, type: :double
  field :server_system_time, 3, type: :double
  field :server_user_time, 4, type: :double
  field :client_system_time, 5, type: :double
  field :client_user_time, 6, type: :double
  field :latency_50, 7, type: :double
  field :latency_90, 8, type: :double
  field :latency_95, 9, type: :double
  field :latency_99, 10, type: :double
  field :latency_999, 11, type: :double
  field :server_cpu_usage, 12, type: :double
  field :successful_requests_per_second, 13, type: :double
  field :failed_requests_per_second, 14, type: :double
  field :client_polls_per_request, 15, type: :double
  field :server_polls_per_request, 16, type: :double
  field :server_queries_per_cpu_sec, 17, type: :double
  field :client_queries_per_cpu_sec, 18, type: :double
end

defmodule Grpc.Testing.ScenarioResult do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          scenario: Grpc.Testing.Scenario.t() | nil,
          latencies: Grpc.Testing.HistogramData.t() | nil,
          client_stats: [Grpc.Testing.ClientStats.t()],
          server_stats: [Grpc.Testing.ServerStats.t()],
          server_cores: [integer],
          summary: Grpc.Testing.ScenarioResultSummary.t() | nil,
          client_success: [boolean],
          server_success: [boolean],
          request_results: [Grpc.Testing.RequestResultCount.t()]
        }
  defstruct [
    :scenario,
    :latencies,
    :client_stats,
    :server_stats,
    :server_cores,
    :summary,
    :client_success,
    :server_success,
    :request_results
  ]

  field :scenario, 1, type: Grpc.Testing.Scenario
  field :latencies, 2, type: Grpc.Testing.HistogramData
  field :client_stats, 3, repeated: true, type: Grpc.Testing.ClientStats
  field :server_stats, 4, repeated: true, type: Grpc.Testing.ServerStats
  field :server_cores, 5, repeated: true, type: :int32
  field :summary, 6, type: Grpc.Testing.ScenarioResultSummary
  field :client_success, 7, repeated: true, type: :bool
  field :server_success, 8, repeated: true, type: :bool
  field :request_results, 9, repeated: true, type: Grpc.Testing.RequestResultCount
end

defmodule Grpc.Testing.ClientType do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3

  field :SYNC_CLIENT, 0
  field :ASYNC_CLIENT, 1
  field :OTHER_CLIENT, 2
end

defmodule Grpc.Testing.ServerType do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3

  field :SYNC_SERVER, 0
  field :ASYNC_SERVER, 1
  field :ASYNC_GENERIC_SERVER, 2
  field :OTHER_SERVER, 3
end

defmodule Grpc.Testing.RpcType do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3

  field :UNARY, 0
  field :STREAMING, 1
  field :STREAMING_FROM_CLIENT, 2
  field :STREAMING_FROM_SERVER, 3
  field :STREAMING_BOTH_WAYS, 4
end
