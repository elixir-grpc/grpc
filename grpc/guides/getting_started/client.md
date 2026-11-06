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