# Testing interceptors

You can test a server interceptor without an endpoint, a server process or a socket. `GRPC.Server.Test` builds a stream, runs your interceptors around a handler function, and returns what the chain returned.

## An interceptor that rejects a request

```elixir
defmodule MyApp.AuthInterceptor do
  @behaviour GRPC.Server.Interceptor

  @impl true
  def init(opts), do: opts

  @impl true
  def call(req, stream, next, _opts) do
    if Map.has_key?(stream.http_request_headers, "authorization") do
      next.(req, stream)
    else
      raise GRPC.RPCError, status: :unauthenticated, message: "missing authorization"
    end
  end
end
```

```elixir
defmodule MyApp.AuthInterceptorTest do
  use ExUnit.Case, async: true

  alias GRPC.Server.Test, as: ServerTest

  test "rejects a request without a token" do
    stream = ServerTest.build_stream(MyApp.Greeter.Server, :SayHello)
    request = %Helloworld.HelloRequest{name: "world"}

    assert {:error, %GRPC.RPCError{status: 16}} =
             ServerTest.run_interceptors([MyApp.AuthInterceptor], request, stream)
  end

  test "lets a request with a token through" do
    stream =
      ServerTest.build_stream(MyApp.Greeter.Server, :SayHello,
        headers: %{"authorization" => "Bearer abc"}
      )

    request = %Helloworld.HelloRequest{name: "world"}
    handler = fn req, _stream -> %Helloworld.HelloReply{message: "Hello, #{req.name}"} end

    assert {:ok, _stream, %Helloworld.HelloReply{message: "Hello, world"}} =
             ServerTest.run_interceptors([MyApp.AuthInterceptor], request, stream, handler)
  end
end
```

## What the functions do

`build_stream/3` takes a server module (one that uses `GRPC.Server`) and an RPC name. It fills the stream as the server does for a real call: `:rpc`, `:service_name`, `:grpc_type`, request and response modules, and a request id. Pass request headers with `:headers`, a deadline with `:timeout` and your `:local` data with `:local`.

`run_interceptors/4` takes a list of `module` or `{module, opts}`, and calls `init/1` on each, as `GRPC.Endpoint` does. The first in the list runs first. The handler function stands in for your RPC function: it takes the request and the stream, and returns the reply. The chain is built by `GRPC.Server`, the same function the server calls on a real request.

The result is what the chain returns, for example `{:ok, stream, reply}`. If an interceptor or the handler raises a `GRPC.RPCError`, you get `{:error, error}`. Any other exception propagates to your test.

## Reading what an interceptor sent

The stream records headers, trailers and replies. Read them after the run:

```elixir
stream = ServerTest.build_stream(MyApp.Greeter.Server, :SayHello)

{:ok, _, _} = ServerTest.run_interceptors([MyApp.RequestIdInterceptor], request, stream)

assert %{"x-request-id" => _} = ServerTest.response_headers(stream)
assert ServerTest.response_trailers(stream) == %{}
```

## Limits

The helper skips Cowboy, HTTP/2 framing and body decoding. It does not emit the `[:grpc, :server, :rpc]` telemetry events. If an interceptor depends on any of these, test it against an endpoint.
