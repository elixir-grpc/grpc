defmodule HelloworldStreams.Endpoint do
  @moduledoc false
  use GRPC.Endpoint

  intercept(GRPC.Server.Interceptors.Logger)
  run(HelloworldStreams.Server)
end
