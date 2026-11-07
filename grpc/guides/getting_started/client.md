# Client

This section demonstrates how to establish client connections and perform RPC calls using the Elixir gRPC client.

---

## Basic Connection and RPC

Connect with gRPC server:

```elixir
iex> {:ok, channel} = GRPC.Stub.connect("localhost:50051")
iex> request = Helloworld.HelloRequest.new(name: "grpc-elixir")
iex> {:ok, reply} = channel |> Helloworld.GreetingServer.Stub.say_unary_hello(request)
```

`use GRPC.Stub` generates one function per RPC, such as `say_unary_hello/3` above. For the arity, return value and options of each RPC kind, see [Generated functions](GRPC.Stub.html#module-generated-functions) in `GRPC.Stub`.

---

## Connecting to IPv6 Addresses

Put an IPv6 address in square brackets and give the port. Both adapters connect without extra options:

```elixir
iex> {:ok, channel} = GRPC.Stub.connect("[::1]:50051")
iex> {:ok, channel} = GRPC.Stub.connect("ipv6:[::1]:50051")
```

A host name that has only AAAA records works the same way as an IPv4 name:

```elixir
iex> {:ok, channel} = GRPC.Stub.connect("ipv6-only.example.com:50051")
iex> {:ok, channel} = GRPC.Stub.connect("dns://ipv6-only.example.com:50051")
```

A name with both A and AAAA records connects over IPv4. To reach its IPv6 address, use the address itself.

---

## Using Interceptors

Client interceptors allow you to add logic to the request/response lifecycle, such as logging, tracing, or authentication.

```elixir
iex> {:ok, channel} =
...>   GRPC.Stub.connect("localhost:50051",
...>     interceptors: [GRPC.Client.Interceptors.Logger]
...>   )
iex> request = Helloworld.HelloRequest.new(name: "Alice")
iex> {:ok, reply} = channel |> Helloworld.GreetingServer.Stub.say_unary_hello(request)
```

### Reading response headers

By default, `next` in an interceptor returns `{:ok, reply}`, without the response headers and
trailers. To get them, define `return_headers?/1` and return `true`. This interceptor saves a
session token that the server sends in a header:

```elixir
defmodule MyApp.SessionInterceptor do
  @behaviour GRPC.Client.Interceptor

  def init(opts), do: opts

  def return_headers?(_opts), do: true

  def call(stream, req, next, _opts) do
    case next.(stream, req) do
      {:ok, _reply, %{headers: headers}} = result ->
        if token = headers["x-session-token"], do: MyApp.Session.put(token)
        result

      other ->
        other
    end
  end
end
```

An interceptor without `return_headers?/1` gets `{:ok, reply}`, as before. The caller's return
value does not change. Pass `result` on as it is. The stub removes the headers and trailers after
the last interceptor, if the caller did not ask for them. If the caller passes
`return_headers: true`, every interceptor gets the long result.

For server-streaming calls the result is `{:ok, replies, %{headers: headers}}`, and the last
item of `replies` is `{:trailers, trailers}`. For client-streaming and bidirectional calls,
`call/4` runs when the stream opens, before any reply exists. Implement the optional
`handle_recv/3`, which `GRPC.Stub.recv/2` calls with the same result shapes. It always gets the
headers, so it needs no `return_headers?/1`:

```elixir
def handle_recv(_stream, {:ok, _reply, %{headers: headers}} = result, _opts) do
  MyApp.Session.put(headers["x-session-token"])
  result
end

def handle_recv(_stream, other, _opts), do: other
```

---

## Compression and Metadata

You can specify message compression and attach default headers to all requests.

```elixir
iex> {:ok, channel} =
...>   GRPC.Stub.connect("localhost:50051",
...>     compressor: GRPC.Compressor.Gzip,
...>     headers: [{"authorization", "Bearer my-token"}]
...>   )
```

---

## Client Adapters

By default, `GRPC.Stub.connect/2` uses the **Gun** adapter.  
You can switch to **Mint** (pure Elixir HTTP/2) or other adapters as needed.

### Slow readers with the Gun adapter

The Gun adapter holds back the server when you read a response slower than it
arrives. It reads ahead of you by at most `:response_window` bytes per stream
(default 1 MiB), then stops reopening the HTTP/2 window until you take more
messages. Gun counts the window in DATA frames of up to 16 KiB, so the default is
64 frames.

```elixir
iex> GRPC.Stub.connect("localhost:50051",
...>   adapter_opts: [response_window: 4 * 1024 * 1024]
...> )
```

The least window is 16,384 bytes, one frame. `GRPC.Stub.connect/2` raises
`ArgumentError` for a smaller value, because a peer built on cowlib (such as
Cowboy) holds a message back while the stream window is smaller than 16,384
bytes and the message does not fit. `response_window: :infinity` turns the limit
off. A message larger than the window still arrives in full.

The queue holds more than the window. After the frames are used up, the server
can still fill what is left of the HTTP/2 stream window. That window is set to
the response window, and Gun adds 65,535 bytes to it each time it reopens it. At
most the queue holds the window in frames, plus the window and 65,535 bytes: about
2.1 MiB by default.

The window limits bytes, not messages. Each queued entry is one DATA frame, so a
server that sends one small message per frame fills the same bytes with many
more entries. With a 64 KiB window and messages of 8 bytes, the queue can hold
about 16,000 entries. A larger window gives faster streams on high-latency links,
and costs memory per stream.

### Using Mint Adapter

```elixir
iex> GRPC.Stub.connect("localhost:50051",
...>   adapter: GRPC.Client.Adapters.Mint
...> )
```

You can configure adapter options globally via your application’s config:

```elixir
# File: config/config.exs
config :grpc, GRPC.Client.Adapters.Mint,
  timeout: 10_000,
  transport_opts: [cacertfile: "/etc/ssl/certs/ca-certificates.crt"]
```

The accepted options are the same as [`Mint.HTTP.connect/4`](https://hexdocs.pm/mint/Mint.HTTP.html#connect/4-options).