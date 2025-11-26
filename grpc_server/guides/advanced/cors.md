# CORS

When accessing gRPC from a browser via HTTP transcoding or gRPC-Web, CORS headers may be required for the browser to allow access to the gRPC endpoint. Adding CORS headers can be done by using `GRPC.Server.Interceptors.CORS` as an interceptor in your `GRPC.Endpoint` module, configuring it as described in the module documentation:

Example:

```elixir
# Define your endpoint
defmodule Helloworld.Endpoint do
  use GRPC.Endpoint

  intercept GRPC.Server.Interceptors.Logger
  intercept GRPC.Server.Interceptors.CORS, allow_origin: "mydomain.io"
  run Helloworld.Greeter.Server
end
```