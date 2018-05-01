defmodule GRPC.Interceptor do
  alias GRPC.Server.Stream

  @type return :: {:ok, Stream.t} | {:ok, Stream.t, struct} | {:error, GRPC.RPCError.t}
  @type next :: (Stream.t -> return)

  @callback init(any) :: any
  @callback call(req :: struct, stream :: Stream.t, next, any) :: return
  @callback call(stream :: Stream.t, next, any) :: return
end
