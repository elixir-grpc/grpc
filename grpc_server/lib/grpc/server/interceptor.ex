defmodule GRPC.ServerInterceptor do
  @moduledoc """
  Interceptor on server side. See `GRPC.Endpoint`.
  """

  @moduledoc deprecated: "Use `GRPC.Server.Interceptor` instead"

  alias GRPC.Server.Stream

  @callback init(options :: any()) :: options :: any()
  @callback call(
              req :: struct() | nil,
              stream :: Stream.t(),
              next :: (req :: struct() | nil, Stream.t() -> GRPC.Server.rpc_return()),
              options :: any()
            ) :: GRPC.Server.rpc_return()
end

defmodule GRPC.Server.Interceptor do
  @moduledoc """
  Interceptor on server side. See `GRPC.Endpoint`.
  """
  alias GRPC.Server.Stream

  @callback init(options :: any()) :: options :: any()
  @callback call(
              req :: struct() | nil,
              stream :: Stream.t(),
              next :: (req :: struct() | nil, Stream.t() -> GRPC.Server.rpc_return()),
              options :: any()
            ) :: GRPC.Server.rpc_return()
end
