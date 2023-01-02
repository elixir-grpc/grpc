defmodule Interop.Endpoint do
  use GRPC.Endpoint

  intercept GRPC.Server.Interceptors.Logger
  intercept GRPCPrometheus.ServerInterceptor
  intercept Interop.ServerInterceptor

  run Interop.Server
end
