defmodule Grpc.Testing.ClientType do
  @moduledoc false
  use Protobuf, enum: true, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :SYNC_CLIENT, 0
  field :ASYNC_CLIENT, 1
  field :OTHER_CLIENT, 2
end

defmodule Grpc.Testing.ServerType do
  @moduledoc false
  use Protobuf, enum: true, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :SYNC_SERVER, 0
  field :ASYNC_SERVER, 1
  field :ASYNC_GENERIC_SERVER, 2
  field :OTHER_SERVER, 3
end

defmodule Grpc.Testing.RpcType do
  @moduledoc false
  use Protobuf, enum: true, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :UNARY, 0
  field :STREAMING, 1
  field :STREAMING_FROM_CLIENT, 2
  field :STREAMING_FROM_SERVER, 3
  field :STREAMING_BOTH_WAYS, 4
end

defmodule Grpc.Testing.PoissonParams do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :offered_load, 1, type: :double, json_name: "offeredLoad"
end

defmodule Grpc.Testing.ClosedLoopParams do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3
end

defmodule Grpc.Testing.LoadParams do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  oneof :load, 0

  field :closed_loop, 1, type: Grpc.Testing.ClosedLoopParams, json_name: "closedLoop", oneof: 0
  field :poisson, 2, type: Grpc.Testing.PoissonParams, oneof: 0
end

defmodule Grpc.Testing.SecurityParams do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :use_test_ca, 1, type: :bool, json_name: "useTestCa"
  field :server_host_override, 2, type: :string, json_name: "serverHostOverride"
  field :cred_type, 3, type: :string, json_name: "credType"
end

defmodule Grpc.Testing.ChannelArg do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  oneof :value, 0

  field :name, 1, type: :string
  field :str_value, 2, type: :string, json_name: "strValue", oneof: 0
  field :int_value, 3, type: :int32, json_name: "intValue", oneof: 0
end

defmodule Grpc.Testing.ClientConfig do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :server_targets, 1, repeated: true, type: :string, json_name: "serverTargets"
  field :client_type, 2, type: Grpc.Testing.ClientType, json_name: "clientType", enum: true
  field :security_params, 3, type: Grpc.Testing.SecurityParams, json_name: "securityParams"
  field :outstanding_rpcs_per_channel, 4, type: :int32, json_name: "outstandingRpcsPerChannel"
  field :client_channels, 5, type: :int32, json_name: "clientChannels"
  field :async_client_threads, 7, type: :int32, json_name: "asyncClientThreads"
  field :rpc_type, 8, type: Grpc.Testing.RpcType, json_name: "rpcType", enum: true
  field :load_params, 10, type: Grpc.Testing.LoadParams, json_name: "loadParams"
  field :payload_config, 11, type: Grpc.Testing.PayloadConfig, json_name: "payloadConfig"
  field :histogram_params, 12, type: Grpc.Testing.HistogramParams, json_name: "histogramParams"
  field :core_list, 13, repeated: true, type: :int32, json_name: "coreList"
  field :core_limit, 14, type: :int32, json_name: "coreLimit"
  field :other_client_api, 15, type: :string, json_name: "otherClientApi"
  field :channel_args, 16, repeated: true, type: Grpc.Testing.ChannelArg, json_name: "channelArgs"
  field :threads_per_cq, 17, type: :int32, json_name: "threadsPerCq"
  field :messages_per_stream, 18, type: :int32, json_name: "messagesPerStream"
  field :use_coalesce_api, 19, type: :bool, json_name: "useCoalesceApi"
end

defmodule Grpc.Testing.ClientStatus do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :stats, 1, type: Grpc.Testing.ClientStats
end

defmodule Grpc.Testing.Mark do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :reset, 1, type: :bool
end

defmodule Grpc.Testing.ClientArgs do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  oneof :argtype, 0

  field :setup, 1, type: Grpc.Testing.ClientConfig, oneof: 0
  field :mark, 2, type: Grpc.Testing.Mark, oneof: 0
end

defmodule Grpc.Testing.ServerConfig do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :server_type, 1, type: Grpc.Testing.ServerType, json_name: "serverType", enum: true
  field :security_params, 2, type: Grpc.Testing.SecurityParams, json_name: "securityParams"
  field :port, 4, type: :int32
  field :async_server_threads, 7, type: :int32, json_name: "asyncServerThreads"
  field :core_limit, 8, type: :int32, json_name: "coreLimit"
  field :payload_config, 9, type: Grpc.Testing.PayloadConfig, json_name: "payloadConfig"
  field :core_list, 10, repeated: true, type: :int32, json_name: "coreList"
  field :other_server_api, 11, type: :string, json_name: "otherServerApi"
  field :threads_per_cq, 12, type: :int32, json_name: "threadsPerCq"
  field :resource_quota_size, 1001, type: :int32, json_name: "resourceQuotaSize"

  field :channel_args, 1002,
    repeated: true,
    type: Grpc.Testing.ChannelArg,
    json_name: "channelArgs"
end

defmodule Grpc.Testing.ServerArgs do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  oneof :argtype, 0

  field :setup, 1, type: Grpc.Testing.ServerConfig, oneof: 0
  field :mark, 2, type: Grpc.Testing.Mark, oneof: 0
end

defmodule Grpc.Testing.ServerStatus do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :stats, 1, type: Grpc.Testing.ServerStats
  field :port, 2, type: :int32
  field :cores, 3, type: :int32
end

defmodule Grpc.Testing.CoreRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3
end

defmodule Grpc.Testing.CoreResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :cores, 1, type: :int32
end

defmodule Grpc.Testing.Void do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3
end

defmodule Grpc.Testing.Scenario do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :name, 1, type: :string
  field :client_config, 2, type: Grpc.Testing.ClientConfig, json_name: "clientConfig"
  field :num_clients, 3, type: :int32, json_name: "numClients"
  field :server_config, 4, type: Grpc.Testing.ServerConfig, json_name: "serverConfig"
  field :num_servers, 5, type: :int32, json_name: "numServers"
  field :warmup_seconds, 6, type: :int32, json_name: "warmupSeconds"
  field :benchmark_seconds, 7, type: :int32, json_name: "benchmarkSeconds"
  field :spawn_local_worker_count, 8, type: :int32, json_name: "spawnLocalWorkerCount"
end

defmodule Grpc.Testing.Scenarios do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :scenarios, 1, repeated: true, type: Grpc.Testing.Scenario
end

defmodule Grpc.Testing.ScenarioResultSummary do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :qps, 1, type: :double
  field :qps_per_server_core, 2, type: :double, json_name: "qpsPerServerCore"
  field :server_system_time, 3, type: :double, json_name: "serverSystemTime"
  field :server_user_time, 4, type: :double, json_name: "serverUserTime"
  field :client_system_time, 5, type: :double, json_name: "clientSystemTime"
  field :client_user_time, 6, type: :double, json_name: "clientUserTime"
  field :latency_50, 7, type: :double, json_name: "latency50"
  field :latency_90, 8, type: :double, json_name: "latency90"
  field :latency_95, 9, type: :double, json_name: "latency95"
  field :latency_99, 10, type: :double, json_name: "latency99"
  field :latency_999, 11, type: :double, json_name: "latency999"
  field :server_cpu_usage, 12, type: :double, json_name: "serverCpuUsage"

  field :successful_requests_per_second, 13,
    type: :double,
    json_name: "successfulRequestsPerSecond"

  field :failed_requests_per_second, 14, type: :double, json_name: "failedRequestsPerSecond"
  field :client_polls_per_request, 15, type: :double, json_name: "clientPollsPerRequest"
  field :server_polls_per_request, 16, type: :double, json_name: "serverPollsPerRequest"
  field :server_queries_per_cpu_sec, 17, type: :double, json_name: "serverQueriesPerCpuSec"
  field :client_queries_per_cpu_sec, 18, type: :double, json_name: "clientQueriesPerCpuSec"
end

defmodule Grpc.Testing.ScenarioResult do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :scenario, 1, type: Grpc.Testing.Scenario
  field :latencies, 2, type: Grpc.Testing.HistogramData
  field :client_stats, 3, repeated: true, type: Grpc.Testing.ClientStats, json_name: "clientStats"
  field :server_stats, 4, repeated: true, type: Grpc.Testing.ServerStats, json_name: "serverStats"
  field :server_cores, 5, repeated: true, type: :int32, json_name: "serverCores"
  field :summary, 6, type: Grpc.Testing.ScenarioResultSummary
  field :client_success, 7, repeated: true, type: :bool, json_name: "clientSuccess"
  field :server_success, 8, repeated: true, type: :bool, json_name: "serverSuccess"

  field :request_results, 9,
    repeated: true,
    type: Grpc.Testing.RequestResultCount,
    json_name: "requestResults"
end