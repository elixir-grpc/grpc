defmodule GRPC.Interceptor do
  alias GRPC.Server.Stream

  @type req :: struct | Enumerable.t
  @type return :: {:ok, Stream.t} | {:ok, Stream.t, struct} | {:error, GRPC.RPCError.t}
  @type next :: (req, Stream.t -> return)

  @callback init(any) :: any
  @callback call(req, stream :: Stream.t, next, any) :: return
end
