defmodule GRPC.ClientInterceptor do
  @moduledoc """
  Interceptor on client side. See `GRPC.Stub.connect/2`.
  """

  @moduledoc deprecated: "Use `GRPC.Client.Interceptor` instead"

  alias GRPC.Client.Stream

  @callback init(options :: any()) :: options :: any()
  @callback call(
              stream :: Stream.t(),
              req :: struct() | nil,
              next :: (Stream.t(), req :: struct() | nil -> GRPC.Stub.rpc_return()),
              options :: any()
            ) :: GRPC.Stub.rpc_return()
end

defmodule GRPC.Client.Interceptor do
  @moduledoc """
  Interceptor on client side. See `GRPC.Stub.connect/2`.
  """
  alias GRPC.Client.Stream

  @callback init(options :: any()) :: options :: any()
  @callback call(
              stream :: Stream.t(),
              req :: struct() | nil,
              next :: (Stream.t(), req :: struct() | nil -> GRPC.Stub.rpc_return()),
              options :: any()
            ) :: GRPC.Stub.rpc_return()
end
