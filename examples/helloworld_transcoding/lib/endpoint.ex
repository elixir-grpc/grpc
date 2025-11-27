defmodule Helloworld.Endpoint do
  use GRPC.Endpoint

  intercept GRPC.Server.Interceptors.Logger
  run Helloworld.Greeter.Server
end
