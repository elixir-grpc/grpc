defmodule Interop.Endpoint do
  use GRPC.Endpoint

  intercept GRPC.Logger.Server
  intercept GRPCPrometheus.ServerInterceptor
  intercept Interop.ServerInterceptor

  run Interop.Server
end
