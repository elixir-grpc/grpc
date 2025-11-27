defmodule GRPC.ServerInterceptor do
  @moduledoc """
  Interceptor on server side. See `GRPC.Endpoint`.
  """

  @moduledoc deprecated: "Use `GRPC.Server.Interceptor` instead"

  alias GRPC.Server.Stream

  @type options :: any()
  @type rpc_return ::
          {:ok, Stream.t(), struct()} | {:ok, Stream.t()} | {:error, GRPC.RPCError.t()}
  @type next :: (GRPC.Server.rpc_req(), Stream.t() -> rpc_return())

  @callback init(options) :: options
  @callback call(GRPC.Server.rpc_req(), stream :: Stream.t(), next, options) :: rpc_return
end

defmodule GRPC.Server.Interceptor do
  @moduledoc """
  Interceptor on server side. See `GRPC.Endpoint`.
  """
  alias GRPC.Server.Stream

  @type options :: any()
  @type rpc_return ::
          {:ok, Stream.t(), struct()} | {:ok, Stream.t()} | {:error, GRPC.RPCError.t()}
  @type next :: (GRPC.Server.rpc_req(), Stream.t() -> rpc_return())

  @callback init(options) :: options
  @callback call(GRPC.Server.rpc_req(), stream :: Stream.t(), next, options) :: rpc_return
end
