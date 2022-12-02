defmodule Grpc.Testing.WorkerService.Service do
  @moduledoc false
  use GRPC.Service, name: "grpc.testing.WorkerService", protoc_gen_elixir_version: "0.11.0"

  rpc :RunServer, stream(Grpc.Testing.ServerArgs), stream(Grpc.Testing.ServerStatus)

  rpc :RunClient, stream(Grpc.Testing.ClientArgs), stream(Grpc.Testing.ClientStatus)

  rpc :CoreCount, Grpc.Testing.CoreRequest, Grpc.Testing.CoreResponse

  rpc :QuitWorker, Grpc.Testing.Void, Grpc.Testing.Void
end

defmodule Grpc.Testing.WorkerService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Grpc.Testing.WorkerService.Service
end