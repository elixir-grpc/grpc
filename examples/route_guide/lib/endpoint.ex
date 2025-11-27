defmodule Routeguide.Endpoint do
  use GRPC.Endpoint

  intercept GRPC.Server.Interceptors.Logger
  run Routeguide.RouteGuide.Server
end
