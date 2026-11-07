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

  By default, `next` returns `{:ok, reply}` for a unary RPC and `{:ok, replies}`
  for a server-streaming RPC. The response headers and trailers are not in it.

  To get them, define `c:return_headers?/1` and return `true`. Then `next`
  returns:

    * unary: `{:ok, reply, %{headers: headers, trailers: trailers}}`
    * server streaming: `{:ok, replies, %{headers: headers}}`. The last item of
      `replies` is `{:trailers, trailers}`.
    * on failure: `{:error, %GRPC.RPCError{}}`

      def return_headers?(_opts), do: true

      def call(stream, req, next, _opts) do
        case next.(stream, req) do
          {:ok, _reply, %{headers: headers}} = result ->
            MyApp.Session.store(headers["x-session"])
            result

          other ->
            other
        end
      end

  An interceptor that does not define `c:return_headers?/1` gets the same
  result as before. If the caller passes `return_headers: true`, every
  interceptor gets the long shape.

  Return the result of `next` unchanged to keep the caller's return value. The
  stub removes the headers and trailers after the last interceptor, unless the
  caller asked for them.

  ## Client-streaming and bidirectional RPCs

  For these RPCs, `c:call/4` runs when the stream opens. The reply comes later,
  from `GRPC.Stub.recv/2`, so `next` cannot return it. An interceptor that needs
  the reply implements the optional `c:handle_recv/3`. It always gets the
  headers and trailers.
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
  Tells the stub if `c:call/4` wants the headers and trailers in the result of
  `next`. `options` is the value `c:init/1` returned.

  If the interceptor does not define this callback, the answer is `false`.
  `c:handle_recv/3` is not affected. It always gets them.
  """
  @callback return_headers?(options :: any()) :: boolean()

  @doc """
  Called from `GRPC.Stub.recv/2` for client-streaming and bidirectional RPCs.

  `result` has the long shapes listed in the module documentation. Return it, or a
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

  @optional_callbacks handle_recv: 3, return_headers?: 1
end
