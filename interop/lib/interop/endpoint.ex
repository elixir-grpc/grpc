defmodule Interop.Endpoint do
  use GRPC.Endpoint

  intercept GRPC.Logger.Server
  intercept GRPCPrometheus.ServerInterceptor

  run Interop.Server
end
