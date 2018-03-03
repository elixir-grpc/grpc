defmodule Grpc.Testing.TestService.Service do
  @moduledoc false
  use GRPC.Service, name: "grpc.testing.TestService"

  rpc :EmptyCall, Grpc.Testing.Empty, Grpc.Testing.Empty
  rpc :UnaryCall, Grpc.Testing.SimpleRequest, Grpc.Testing.SimpleResponse
  rpc :CacheableUnaryCall, Grpc.Testing.SimpleRequest, Grpc.Testing.SimpleResponse

  rpc :StreamingOutputCall,
      Grpc.Testing.StreamingOutputCallRequest,
      stream(Grpc.Testing.StreamingOutputCallResponse)

  rpc :StreamingInputCall,
      stream(Grpc.Testing.StreamingInputCallRequest),
      Grpc.Testing.StreamingInputCallResponse

  rpc :FullDuplexCall,
      stream(Grpc.Testing.StreamingOutputCallRequest),
      stream(Grpc.Testing.StreamingOutputCallResponse)

  rpc :HalfDuplexCall,
      stream(Grpc.Testing.StreamingOutputCallRequest),
      stream(Grpc.Testing.StreamingOutputCallResponse)

  rpc :UnimplementedCall, Grpc.Testing.Empty, Grpc.Testing.Empty
end

defmodule Grpc.Testing.TestService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Grpc.Testing.TestService.Service
end

defmodule Grpc.Testing.UnimplementedService.Service do
  @moduledoc false
  use GRPC.Service, name: "grpc.testing.UnimplementedService"

  rpc :UnimplementedCall, Grpc.Testing.Empty, Grpc.Testing.Empty
end

defmodule Grpc.Testing.UnimplementedService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Grpc.Testing.UnimplementedService.Service
end

defmodule Grpc.Testing.ReconnectService.Service do
  @moduledoc false
  use GRPC.Service, name: "grpc.testing.ReconnectService"

  rpc :Start, Grpc.Testing.ReconnectParams, Grpc.Testing.Empty
  rpc :Stop, Grpc.Testing.Empty, Grpc.Testing.ReconnectInfo
end

defmodule Grpc.Testing.ReconnectService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Grpc.Testing.ReconnectService.Service
end
