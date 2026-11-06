# Message size limits

A client limits the size of the messages it sends and receives. A message over a
limit fails the call with status `RESOURCE_EXHAUSTED` (`8`), and the error message
says the limit.

| Option | Where to set it | Default | Applies to |
|--------|-----------------|---------|------------|
| `:max_message_length` | `config :grpc, max_message_length: bytes` | 2 GiB (`2_147_483_648`) | One message, sent or received |
| `:max_decompressed_message_length` | `config :grpc, max_decompressed_message_length: bytes` | 4 MiB (`4_194_304`) | One message after gzip decompression |

```elixir
# config/config.exs
config :grpc, max_message_length: 16 * 1024 * 1024
```

The limit is read on every message, so it also works from `Application.put_env/3`
at run time. It is global to the node: a node that runs a client and a server
applies the same limit to both.

## Sending

A request over the limit is not sent.

* A unary call returns `{:error, %GRPC.RPCError{status: 8}}`.
* `GRPC.Stub.send_request/3` on a request stream raises `GRPC.RPCError`, because it
  returns the stream and has no error to return.

The message reads `Message to send is too large (2048 bytes, the limit is 1000 bytes)`.
The size is that of the encoded message, before compression.

## Receiving

The client checks the length in a message's prefix, so it stops before it has read the
message. The message reads `Received message is too large (2048 bytes, the limit is 1000 bytes)`.

* A unary call returns `{:error, %GRPC.RPCError{status: 8}}`.
* On a response stream, both adapters return `{:error, %GRPC.RPCError{status: 8}}`
  as an item, and end the stream.

Both adapters check the prefix in the first chunk that holds it, then cancel the
request, which resets the HTTP/2 stream. The server stops sending, and the client
does not read the rest of the message.

## Receiving very large replies

A unary reply of 100 MB, 300 MB and 600 MB arrives without error from a Cowboy server
on a loopback connection, with the Gun and Mint adapters. The default limit does not
stop it. The default timeout can: a unary call gives up after 10 seconds, and
a 600 MB reply took 9 seconds with Gun and 15 seconds with Mint on the test machine.
Pass a larger `:timeout` for such calls:

```elixir
MyApp.Greeter.Stub.say_hello(channel, request, timeout: 120_000)
```

Mint and Gun report this as `DEADLINE_EXCEEDED` (`4`). A server stops a call that
passes its deadline too, and the client gets the same status.
