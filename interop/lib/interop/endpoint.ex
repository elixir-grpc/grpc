defmodule Interop.Endpoint do
  use GRPC.Endpoint

  run Interop.Server
end
