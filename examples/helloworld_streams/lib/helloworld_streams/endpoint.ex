defmodule HelloworldStreams.Endpoint do
  @moduledoc false
  use GRPC.Endpoint

  intercept(GRPC.Server.Interceptors.Logger)
  run(HelloworldStreams.Utils.Reflection)
  run(HelloworldStreams.Server)
end
