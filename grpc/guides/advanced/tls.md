# TLS and Mutual TLS

This guide shows how to connect to a gRPC server over TLS, and over mutual TLS (mTLS), where the server also checks the client's certificate. It covers both client adapters, Gun and Mint, and the server side.

The short version: **all TLS options go in a `GRPC.Credential`, passed as `:cred` to `GRPC.Stub.connect/2`.** The same credential works for both adapters.

```elixir
cred =
  GRPC.Credential.new(
    ssl: [
      certfile: "/etc/my_app/client.pem",
      keyfile: "/etc/my_app/client.key",
      cacertfile: "/etc/my_app/server_ca.pem",
      verify: :verify_peer
    ]
  )

{:ok, channel} = GRPC.Stub.connect("https://example.com:50051", cred: cred)
```

The `:ssl` list takes the client options of Erlang's [`:ssl`](https://www.erlang.org/doc/apps/ssl/ssl.html). The ones you need most:

| Option                    | Purpose                                                                                  |
|:--------------------------|:-----------------------------------------------------------------------------------------|
| `cacertfile`              | PEM file with the CA that signed the **server's** certificate.                           |
| `verify`                  | Use `:verify_peer` so the client checks the server's certificate.                        |
| `certfile`                | PEM file with the **client's** certificate. Needed for mTLS.                             |
| `keyfile`                 | PEM file with the client's private key. Needed for mTLS.                                 |
| `server_name_indication`  | The name to check the server's certificate against. A charlist, such as `~c"example.com"`. |

Spell it `cacertfile`, not `cacert_file`.

## Server-only TLS

The client checks the server's certificate and sends none of its own.

With a public CA, add the `:castore` dependency and connect with `https://`. The client fills in a credential that trusts the CAs in `CAStore`:

```elixir
{:ok, channel} = GRPC.Stub.connect("https://example.com:50051")
```

With a private CA, pass your own credential:

```elixir
cred =
  GRPC.Credential.new(
    ssl: [
      cacertfile: "/etc/my_app/server_ca.pem",
      verify: :verify_peer
    ]
  )

{:ok, channel} = GRPC.Stub.connect("https://example.com:50051", cred: cred)
```

Your credential replaces the default one. It does not add to it. Always give it a `cacertfile` and `verify: :verify_peer`.

## Mutual TLS

For mTLS the client also sends a certificate. Add `certfile` and `keyfile` to the credential:

```elixir
cred =
  GRPC.Credential.new(
    ssl: [
      certfile: "/etc/my_app/client.pem",
      keyfile: "/etc/my_app/client.key",
      cacertfile: "/etc/my_app/server_ca.pem",
      verify: :verify_peer,
      server_name_indication: ~c"example.com"
    ]
  )
```

Connect with it using either adapter:

```elixir
# Gun, the default
{:ok, channel} = GRPC.Stub.connect("https://example.com:50051", cred: cred)

# Mint
{:ok, channel} =
  GRPC.Stub.connect("https://example.com:50051",
    adapter: GRPC.Client.Adapters.Mint,
    cred: cred
  )
```

`server_name_indication` is optional when you connect by host name: the client uses that name. Set it when you connect by IP address, or by a name that is not in the server's certificate. Without it, an IP address fails the host name check.

### What the adapters do with the credential

On an `https` channel, each adapter takes `cred.ssl` and uses it as the TLS options for the socket.

* **Gun** merges `cred.ssl` with `adapter_opts: [transport_opts: ...]`. If both set the same key, `transport_opts` wins.
* **Mint** merges them the other way around. If both set the same key, `cred.ssl` wins.

Keep TLS options in `cred` and use `transport_opts` only for other socket options, such as `:ip` or `:nodelay`. Do not put certificates in the `config :grpc, GRPC.Client.Adapters.Mint` application config: its `:transport_opts` replaces the whole list, including the options from `cred`.

### Options that do not exist

These do not carry certificates, though they look like they might:

* `custom_opts:` is not a connect option. `GRPC.Stub.connect/2` raises on it. Inside `adapter_opts`, the Mint adapter rejects it.
* `adapter_opts: [certfile: ...]` is not a TLS option. The Mint adapter rejects TLS keys at the top level of `adapter_opts`. Put them in `cred`.
* `:cred` must be a `GRPC.Credential`, not a plain keyword list. Use `GRPC.Credential.new(ssl: [...])`.

## How the client picks TLS

The client decides from the target and from `:cred`:

| Target                    | `:cred`   | Result                                                       |
|:--------------------------|:----------|:-------------------------------------------------------------|
| `"https://host:port"`     | not given | TLS, with a default credential built from `CAStore`.         |
| `"https://host:port"`     | given     | TLS, with your credential.                                   |
| `"host:port"`             | given     | TLS, with your credential.                                   |
| `"host:port"`             | not given | **Plaintext.**                                               |
| `"http://host:port"`      | not given | Plaintext.                                                   |
| `"http://host:port"`      | given     | Raises `ArgumentError`.                                      |

The default credential needs `:castore`. Without it, `connect/2` raises and asks for a `:cred`.

We suggest `https://` for TLS targets, so the scheme says what the connection does.

## The server side

Give the server a credential in `adapter_opts`. It needs the server's certificate and key:

```elixir
server_cred =
  GRPC.Credential.new(
    ssl: [
      certfile: "/etc/my_app/server.pem",
      keyfile: "/etc/my_app/server.key"
    ]
  )

children = [
  {GRPC.Server.Supervisor,
   endpoint: MyApp.Endpoint,
   port: 50051,
   start_server: true,
   adapter_opts: [cred: server_cred]}
]
```

`GRPC.Server.start_endpoint/3` takes the same option:

```elixir
GRPC.Server.start_endpoint(MyApp.Endpoint, 50051, adapter_opts: [cred: server_cred])
```

To require client certificates, add three options. The server only asks for a client certificate when `verify` is `:verify_peer`:

```elixir
server_cred =
  GRPC.Credential.new(
    ssl: [
      certfile: "/etc/my_app/server.pem",
      keyfile: "/etc/my_app/server.key",
      cacertfile: "/etc/my_app/client_ca.pem",
      verify: :verify_peer,
      fail_if_no_peer_cert: true
    ]
  )
```

Here `cacertfile` holds the CA that signed the **client** certificates. Without `fail_if_no_peer_cert: true`, a client that sends no certificate still connects.

## Troubleshooting

### A plaintext client and a TLS server: `WRONG_VERSION_NUMBER`

If you connect to a TLS port with `"host:port"` and no `:cred`, the client sends plaintext HTTP/2 to a server that expects a TLS handshake. A gRPC server built on OpenSSL logs:

```text
Handshake failed with fatal error SSL_ERROR_SSL: error:100000f7:SSL routines:OPENSSL_internal:WRONG_VERSION_NUMBER.
```

An Elixir server on this library logs `unsupported_record_type` instead.

`GRPC.Stub.connect/2` still returns `{:ok, channel}`, because the connection is set up in a separate process. The failure shows up on the first call, as a `GRPC.RPCError` with status `UNAVAILABLE` (14). It does not tell you about TLS. Neither does the same error when you give no certificates at all.

The fix is on the client: use `https://`, or pass `:cred`.

### The server rejects the client

If the server requires a client certificate and the client sends none, or one the server does not trust, the call fails. With TLS 1.3 the client finishes its half of the handshake first, so `connect/2` returns `{:ok, channel}` and the first call fails. The server logs `certificate_required` or `Bad Certificate`.

Check three things: the client sends `certfile` and `keyfile`; the server's `cacertfile` holds the CA that signed the client certificate; the client certificate has not expired.

### The client rejects the server

The client logs `hostname_check_failed`, `unknown_ca` or a similar `bad_cert` reason. The call may then wait for a timeout, so look in the log for the reason.

* `hostname_check_failed`: the name the client dials is not in the server certificate's subject alternative names. Connect by a name that is, or set `server_name_indication`.
* `unknown_ca`: the client's `cacertfile` does not hold the CA that signed the server's certificate.

### `:badarg` or `{:options, ...}` when connecting

TLS options reached a plaintext socket, or the reverse. Check that the target is `https://` or that `:cred` is set, and that you spelled `cacertfile` and `certfile` as above.
