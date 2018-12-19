defmodule GRPC.ServerInterceptor do
  alias GRPC.Server.Stream

  @type options :: any
  @type rpc_return :: {:ok, Stream.t(), struct} | {:ok, Stream.t()} | {:error, GRPC.RPCError.t()}
  @type next :: (GRPC.Server.rpc_req(), Stream.t() -> rpc_return)

  @callback init(options) :: options
  @callback call(GRPC.Server.rpc_req(), stream :: Stream.t(), next, options) :: rpc_return
end

defmodule GRPC.ClientInterceptor do
  alias GRPC.Client.Stream

  @type options :: any
  @type req :: struct | nil
  @type next :: (Stream.t(), req -> GRPC.Stub.rpc_return())

  @callback init(options) :: options
  @callback call(stream :: Stream.t(), req, next, options) :: GRPC.Stub.rpc_return()
end
