defmodule Grpc.Testing.TestService.Service do
  @moduledoc false
  use GRPC.Service, name: "grpc.testing.TestService", protoc_gen_elixir_version: "0.10.0"

  rpc(:EmptyCall, Grpc.Testing.Empty, Grpc.Testing.Empty)

  rpc(:UnaryCall, Grpc.Testing.SimpleRequest, Grpc.Testing.SimpleResponse)

  rpc(:CacheableUnaryCall, Grpc.Testing.SimpleRequest, Grpc.Testing.SimpleResponse)

  rpc(
    :StreamingOutputCall,
    Grpc.Testing.StreamingOutputCallRequest,
    stream(Grpc.Testing.StreamingOutputCallResponse)
  )

  rpc(
    :StreamingInputCall,
    stream(Grpc.Testing.StreamingInputCallRequest),
    Grpc.Testing.StreamingInputCallResponse
  )

  rpc(
    :FullDuplexCall,
    stream(Grpc.Testing.StreamingOutputCallRequest),
    stream(Grpc.Testing.StreamingOutputCallResponse)
  )

  rpc(
    :HalfDuplexCall,
    stream(Grpc.Testing.StreamingOutputCallRequest),
    stream(Grpc.Testing.StreamingOutputCallResponse)
  )

  rpc(:UnimplementedCall, Grpc.Testing.Empty, Grpc.Testing.Empty)
end

defmodule Grpc.Testing.TestService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Grpc.Testing.TestService.Service
end

defmodule Grpc.Testing.UnimplementedService.Service do
  @moduledoc false
  use GRPC.Service, name: "grpc.testing.UnimplementedService", protoc_gen_elixir_version: "0.10.0"

  rpc(:UnimplementedCall, Grpc.Testing.Empty, Grpc.Testing.Empty)
end

defmodule Grpc.Testing.UnimplementedService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Grpc.Testing.UnimplementedService.Service
end

defmodule Grpc.Testing.ReconnectService.Service do
  @moduledoc false
  use GRPC.Service, name: "grpc.testing.ReconnectService", protoc_gen_elixir_version: "0.10.0"

  rpc(:Start, Grpc.Testing.ReconnectParams, Grpc.Testing.Empty)

  rpc(:Stop, Grpc.Testing.Empty, Grpc.Testing.ReconnectInfo)
end

defmodule Grpc.Testing.ReconnectService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Grpc.Testing.ReconnectService.Service
end

defmodule Grpc.Testing.LoadBalancerStatsService.Service do
  @moduledoc false
  use GRPC.Service,
    name: "grpc.testing.LoadBalancerStatsService",
    protoc_gen_elixir_version: "0.10.0"

  rpc(
    :GetClientStats,
    Grpc.Testing.LoadBalancerStatsRequest,
    Grpc.Testing.LoadBalancerStatsResponse
  )

  rpc(
    :GetClientAccumulatedStats,
    Grpc.Testing.LoadBalancerAccumulatedStatsRequest,
    Grpc.Testing.LoadBalancerAccumulatedStatsResponse
  )
end

defmodule Grpc.Testing.LoadBalancerStatsService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Grpc.Testing.LoadBalancerStatsService.Service
end

defmodule Grpc.Testing.XdsUpdateHealthService.Service do
  @moduledoc false
  use GRPC.Service,
    name: "grpc.testing.XdsUpdateHealthService",
    protoc_gen_elixir_version: "0.10.0"

  rpc(:SetServing, Grpc.Testing.Empty, Grpc.Testing.Empty)

  rpc(:SetNotServing, Grpc.Testing.Empty, Grpc.Testing.Empty)
end

defmodule Grpc.Testing.XdsUpdateHealthService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Grpc.Testing.XdsUpdateHealthService.Service
end

defmodule Grpc.Testing.XdsUpdateClientConfigureService.Service do
  @moduledoc false
  use GRPC.Service,
    name: "grpc.testing.XdsUpdateClientConfigureService",
    protoc_gen_elixir_version: "0.10.0"

  rpc(:Configure, Grpc.Testing.ClientConfigureRequest, Grpc.Testing.ClientConfigureResponse)
end

defmodule Grpc.Testing.XdsUpdateClientConfigureService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Grpc.Testing.XdsUpdateClientConfigureService.Service
end
