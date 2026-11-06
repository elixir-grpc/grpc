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

  ## Reading response headers and trailers

  An interceptor always sees the response headers and trailers, whether or not
  the caller passed `return_headers: true`. After the stub's reply is
  received, the result of `next` has these shapes:

    * unary and client streaming: `{:ok, reply, %{headers: headers, trailers: trailers}}`
    * server streaming and bidirectional streaming:
      `{:ok, replies, %{headers: headers}}`. The last item of `replies` is
      `{:trailers, trailers}`.
    * on failure: `{:error, %GRPC.RPCError{}}`

  The stub removes the headers and trailers again after the last interceptor
  returns, unless the caller asked for them. An interceptor that returns its
  own result in place of the one from `next` can return either shape. Pass the
  result through as it is to keep the caller's return value unchanged.

  For unary and server-streaming RPCs, `c:call/4` sees the reply through `next`:

      def call(stream, req, next, _opts) do
        case next.(stream, req) do
          {:ok, _reply, %{headers: headers}} = result ->
            MyApp.Session.store(headers["x-session"])
            result

          other ->
            other
        end
      end

  ## Client-streaming and bidirectional RPCs

  For these RPCs, `c:call/4` runs when the stream opens. The reply comes later,
  from `GRPC.Stub.recv/2`, so `next` cannot return it. An interceptor that needs
  the reply implements the optional `c:handle_recv/3`.
  """
  alias GRPC.Client.Stream

  @callback init(options :: any()) :: options :: any()
  @callback call(
              stream :: Stream.t(),
              req :: struct() | nil,
              next :: (Stream.t(), req :: struct() | nil -> GRPC.Stub.rpc_return()),
              options :: any()
            ) :: GRPC.Stub.rpc_return()

  @doc """
  Called from `GRPC.Stub.recv/2` for client-streaming and bidirectional RPCs.

  `result` has the shapes listed in the module documentation. Return it, or a
  changed copy, as the new result. Several interceptors run in the order their
  `c:call/4` results return, so the first interceptor in the `:interceptors`
  list runs first.

  It is not called for unary and server-streaming RPCs. Use `c:call/4` there.
  """
  @callback handle_recv(
              stream :: Stream.t(),
              result :: GRPC.Stub.rpc_return(),
              options :: any()
            ) :: GRPC.Stub.rpc_return()

  @optional_callbacks handle_recv: 3
end
