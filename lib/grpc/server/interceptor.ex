defmodule GRPC.ServerInterceptor do
  @moduledoc """
  Interceptor on server side. See `GRPC.Endpoint`.
  """

  @moduledoc deprecated: "Use `GRPC.Server.Interceptor` instead"

  alias GRPC.Server.Materializer

  @type options :: any()
  @type rpc_return ::
          {:ok, Materializer.t(), struct()}
          | {:ok, Materializer.t()}
          | {:error, GRPC.RPCError.t()}
  @type next :: (GRPC.Server.rpc_req(), Materializer.t() -> rpc_return())

  @callback init(options) :: options
  @callback call(GRPC.Server.rpc_req(), stream :: Materializer.t(), next, options) :: rpc_return
end

defmodule GRPC.Server.Interceptor do
  @moduledoc """
  Interceptor on server side. See `GRPC.Endpoint`.
  """
  alias GRPC.Server.Materializer

  @type options :: any()
  @type rpc_return ::
          {:ok, Materializer.t(), struct()}
          | {:ok, Materializer.t()}
          | {:error, GRPC.RPCError.t()}
  @type next :: (GRPC.Server.rpc_req(), Materializer.t() -> rpc_return())

  @callback init(options) :: options
  @callback call(GRPC.Server.rpc_req(), stream :: Materializer.t(), next, options) :: rpc_return
end
