defmodule GRPC.ClientInterceptor do
  @moduledoc """
  Interceptor on client side. See `GRPC.Stub.connect/2`.
  """

  @moduledoc deprecated: "Use `GRPC.Client.Interceptor` instead"

  alias GRPC.Client.Stream

  @type options :: any()
  @type req :: struct() | nil
  @type next :: (Stream.t(), req -> GRPC.Stub.rpc_return())

  @callback init(options) :: options
  @callback call(stream :: Stream.t(), req, next, options) :: GRPC.Stub.rpc_return()
end

defmodule GRPC.Client.Interceptor do
  @moduledoc """
  Interceptor on client side. See `GRPC.Stub.connect/2`.
  """
  alias GRPC.Client.Stream

  @type options :: any()
  @type req :: struct() | nil
  @type next :: (Stream.t(), req -> GRPC.Stub.rpc_return())

  @callback init(options) :: options
  @callback call(stream :: Stream.t(), req, next, options) :: GRPC.Stub.rpc_return()
end
