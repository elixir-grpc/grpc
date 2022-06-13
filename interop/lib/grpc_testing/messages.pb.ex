defmodule Grpc.Testing.PayloadType do
  @moduledoc false
  use Protobuf, enum: true, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :COMPRESSABLE, 0
end

defmodule Grpc.Testing.GrpclbRouteType do
  @moduledoc false
  use Protobuf, enum: true, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :GRPCLB_ROUTE_TYPE_UNKNOWN, 0
  field :GRPCLB_ROUTE_TYPE_FALLBACK, 1
  field :GRPCLB_ROUTE_TYPE_BACKEND, 2
end

defmodule Grpc.Testing.ClientConfigureRequest.RpcType do
  @moduledoc false
  use Protobuf, enum: true, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :EMPTY_CALL, 0
  field :UNARY_CALL, 1
end

defmodule Grpc.Testing.BoolValue do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :value, 1, type: :bool
end

defmodule Grpc.Testing.Payload do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :type, 1, type: Grpc.Testing.PayloadType, enum: true
  field :body, 2, type: :bytes
end

defmodule Grpc.Testing.EchoStatus do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :code, 1, type: :int32
  field :message, 2, type: :string
end

defmodule Grpc.Testing.SimpleRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :response_type, 1, type: Grpc.Testing.PayloadType, json_name: "responseType", enum: true
  field :response_size, 2, type: :int32, json_name: "responseSize"
  field :payload, 3, type: Grpc.Testing.Payload
  field :fill_username, 4, type: :bool, json_name: "fillUsername"
  field :fill_oauth_scope, 5, type: :bool, json_name: "fillOauthScope"
  field :response_compressed, 6, type: Grpc.Testing.BoolValue, json_name: "responseCompressed"
  field :response_status, 7, type: Grpc.Testing.EchoStatus, json_name: "responseStatus"
  field :expect_compressed, 8, type: Grpc.Testing.BoolValue, json_name: "expectCompressed"
  field :fill_server_id, 9, type: :bool, json_name: "fillServerId"
  field :fill_grpclb_route_type, 10, type: :bool, json_name: "fillGrpclbRouteType"
end

defmodule Grpc.Testing.SimpleResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :payload, 1, type: Grpc.Testing.Payload
  field :username, 2, type: :string
  field :oauth_scope, 3, type: :string, json_name: "oauthScope"
  field :server_id, 4, type: :string, json_name: "serverId"

  field :grpclb_route_type, 5,
    type: Grpc.Testing.GrpclbRouteType,
    json_name: "grpclbRouteType",
    enum: true

  field :hostname, 6, type: :string
end

defmodule Grpc.Testing.StreamingInputCallRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :payload, 1, type: Grpc.Testing.Payload
  field :expect_compressed, 2, type: Grpc.Testing.BoolValue, json_name: "expectCompressed"
end

defmodule Grpc.Testing.StreamingInputCallResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :aggregated_payload_size, 1, type: :int32, json_name: "aggregatedPayloadSize"
end

defmodule Grpc.Testing.ResponseParameters do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :size, 1, type: :int32
  field :interval_us, 2, type: :int32, json_name: "intervalUs"
  field :compressed, 3, type: Grpc.Testing.BoolValue
end

defmodule Grpc.Testing.StreamingOutputCallRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :response_type, 1, type: Grpc.Testing.PayloadType, json_name: "responseType", enum: true

  field :response_parameters, 2,
    repeated: true,
    type: Grpc.Testing.ResponseParameters,
    json_name: "responseParameters"

  field :payload, 3, type: Grpc.Testing.Payload
  field :response_status, 7, type: Grpc.Testing.EchoStatus, json_name: "responseStatus"
end

defmodule Grpc.Testing.StreamingOutputCallResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :payload, 1, type: Grpc.Testing.Payload
end

defmodule Grpc.Testing.ReconnectParams do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :max_reconnect_backoff_ms, 1, type: :int32, json_name: "maxReconnectBackoffMs"
end

defmodule Grpc.Testing.ReconnectInfo do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :passed, 1, type: :bool
  field :backoff_ms, 2, repeated: true, type: :int32, json_name: "backoffMs"
end

defmodule Grpc.Testing.LoadBalancerStatsRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :num_rpcs, 1, type: :int32, json_name: "numRpcs"
  field :timeout_sec, 2, type: :int32, json_name: "timeoutSec"
end

defmodule Grpc.Testing.LoadBalancerStatsResponse.RpcsByPeer.RpcsByPeerEntry do
  @moduledoc false
  use Protobuf, map: true, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :int32
end

defmodule Grpc.Testing.LoadBalancerStatsResponse.RpcsByPeer do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :rpcs_by_peer, 1,
    repeated: true,
    type: Grpc.Testing.LoadBalancerStatsResponse.RpcsByPeer.RpcsByPeerEntry,
    json_name: "rpcsByPeer",
    map: true
end

defmodule Grpc.Testing.LoadBalancerStatsResponse.RpcsByPeerEntry do
  @moduledoc false
  use Protobuf, map: true, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :int32
end

defmodule Grpc.Testing.LoadBalancerStatsResponse.RpcsByMethodEntry do
  @moduledoc false
  use Protobuf, map: true, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: Grpc.Testing.LoadBalancerStatsResponse.RpcsByPeer
end

defmodule Grpc.Testing.LoadBalancerStatsResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :rpcs_by_peer, 1,
    repeated: true,
    type: Grpc.Testing.LoadBalancerStatsResponse.RpcsByPeerEntry,
    json_name: "rpcsByPeer",
    map: true

  field :num_failures, 2, type: :int32, json_name: "numFailures"

  field :rpcs_by_method, 3,
    repeated: true,
    type: Grpc.Testing.LoadBalancerStatsResponse.RpcsByMethodEntry,
    json_name: "rpcsByMethod",
    map: true
end

defmodule Grpc.Testing.LoadBalancerAccumulatedStatsRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3
end

defmodule Grpc.Testing.LoadBalancerAccumulatedStatsResponse.NumRpcsStartedByMethodEntry do
  @moduledoc false
  use Protobuf, map: true, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :int32
end

defmodule Grpc.Testing.LoadBalancerAccumulatedStatsResponse.NumRpcsSucceededByMethodEntry do
  @moduledoc false
  use Protobuf, map: true, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :int32
end

defmodule Grpc.Testing.LoadBalancerAccumulatedStatsResponse.NumRpcsFailedByMethodEntry do
  @moduledoc false
  use Protobuf, map: true, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :int32
end

defmodule Grpc.Testing.LoadBalancerAccumulatedStatsResponse.MethodStats.ResultEntry do
  @moduledoc false
  use Protobuf, map: true, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :key, 1, type: :int32
  field :value, 2, type: :int32
end

defmodule Grpc.Testing.LoadBalancerAccumulatedStatsResponse.MethodStats do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :rpcs_started, 1, type: :int32, json_name: "rpcsStarted"

  field :result, 2,
    repeated: true,
    type: Grpc.Testing.LoadBalancerAccumulatedStatsResponse.MethodStats.ResultEntry,
    map: true
end

defmodule Grpc.Testing.LoadBalancerAccumulatedStatsResponse.StatsPerMethodEntry do
  @moduledoc false
  use Protobuf, map: true, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: Grpc.Testing.LoadBalancerAccumulatedStatsResponse.MethodStats
end

defmodule Grpc.Testing.LoadBalancerAccumulatedStatsResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :num_rpcs_started_by_method, 1,
    repeated: true,
    type: Grpc.Testing.LoadBalancerAccumulatedStatsResponse.NumRpcsStartedByMethodEntry,
    json_name: "numRpcsStartedByMethod",
    map: true,
    deprecated: true

  field :num_rpcs_succeeded_by_method, 2,
    repeated: true,
    type: Grpc.Testing.LoadBalancerAccumulatedStatsResponse.NumRpcsSucceededByMethodEntry,
    json_name: "numRpcsSucceededByMethod",
    map: true,
    deprecated: true

  field :num_rpcs_failed_by_method, 3,
    repeated: true,
    type: Grpc.Testing.LoadBalancerAccumulatedStatsResponse.NumRpcsFailedByMethodEntry,
    json_name: "numRpcsFailedByMethod",
    map: true,
    deprecated: true

  field :stats_per_method, 4,
    repeated: true,
    type: Grpc.Testing.LoadBalancerAccumulatedStatsResponse.StatsPerMethodEntry,
    json_name: "statsPerMethod",
    map: true
end

defmodule Grpc.Testing.ClientConfigureRequest.Metadata do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :type, 1, type: Grpc.Testing.ClientConfigureRequest.RpcType, enum: true
  field :key, 2, type: :string
  field :value, 3, type: :string
end

defmodule Grpc.Testing.ClientConfigureRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  field :types, 1, repeated: true, type: Grpc.Testing.ClientConfigureRequest.RpcType, enum: true
  field :metadata, 2, repeated: true, type: Grpc.Testing.ClientConfigureRequest.Metadata
  field :timeout_sec, 3, type: :int32, json_name: "timeoutSec"
end

defmodule Grpc.Testing.ClientConfigureResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3
end
