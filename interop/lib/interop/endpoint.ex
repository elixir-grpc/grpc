defmodule Interop.Endpoint do
  use GRPC.Endpoint

  intercept Interop.ServerInterceptor

  run Interop.Server
end
