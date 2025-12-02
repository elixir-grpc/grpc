# Custom Codecs

This guide explains how to implement and use custom codecs with `elixir-grpc`. Custom codecs allow you to serialize and deserialize gRPC messages using formats other than the default Protocol Buffers.

## Overview

`elixir-grpc` supports custom codecs through the `GRPC.Codec` behaviour. The library comes with built-in codecs:

- `GRPC.Codec.Proto` - Protocol Buffers (default)
- `GRPC.Codec.JSON` - JSON encoding (primarily for HTTP transcoding)
- `GRPC.Codec.WebText` - Base64-encoded Protocol Buffers for gRPC-Web

You can implement your own codec to support custom serialization formats like MessagePack, BSON, XML, or any other format.

## Implementing a Custom Codec

To create a custom codec, implement the `GRPC.Codec` behaviour:

```elixir
defmodule MyApp.CustomCodec do
  @behaviour GRPC.Codec

  @impl true
  def name(), do: "custom-format"

  @impl true
  def encode(struct, _opts \\ []) do
    # Convert the protobuf struct to your custom format
    # Return iodata
  end

  @impl true
  def decode(binary, module) do
    # Convert binary data back to the protobuf struct
    # Return the decoded struct
  end
end
```

### Required Callbacks

#### `name/0`

Returns a string identifier for your codec. This name becomes part of the `content-type` header:

```
application/grpc+custom-format
```

**Important:** The name should be unique and follow gRPC content-type conventions.

#### `encode/2`

Encodes a struct into binary data or iodata.

- First argument: The struct to encode
- Second argument: A keyword list of options (optional)
- Returns:** `iodata()`

#### `decode/2`

Decodes binary data back into a struct.

- First argument: Binary data to decode
- Second argument: The module of the target struct
- Returns: The decoded struct

### Optional Callbacks

#### `unpack_from_channel/1`

Called before decoding the gRPC payload. Useful for applying transformations like Base64 decoding (used by `GRPC.Codec.WebText`).

```elixir
@impl true
def unpack_from_channel(binary) do
  Base.decode64!(binary)
end
```

#### `pack_for_channel/1`

Called after encoding the protobuf message. Useful for applying transformations like Base64 encoding before sending.

```elixir
@impl true
def pack_for_channel(iodata) do
  Base.encode64(IO.iodata_to_binary(iodata))
end
```

## Example

Here's a complete example of a custom JSON codec:

```elixir
defmodule MyApp.CustomJSONCodec do
  @behaviour GRPC.Codec

  @impl true
  def name(), do: "custom-json"

  @impl true
  def encode(struct, _opts \\ []) do
    json = Protobuf.JSON.encode!(struct)
    "CUSTOM:" <> json
  end

  @impl true
  def decode(binary, module) do
    "CUSTOM:" <> json = binary
    Protobuf.JSON.decode!(json, module)
  end
end
```

## Using Custom Codecs

### Server-Side Configuration

Register your custom codec when defining your server:

```elixir
defmodule MyApp.Greeter.Server do
  use GRPC.Server,
    service: MyApp.Greeter.Service,
    codecs: [MyApp.CustomJSONCodec, GRPC.Codec.Proto]

  def say_hello(request, _stream) do
    %MyApp.HelloReply{message: "Hello, #{request.name}!"}
  end
end
```

>__Note__: Order matters. The first codec in the list is used as the default for the server.

### Client-Side Usage

#### Set Default Codec on Connection

```elixir
{:ok, channel} = GRPC.Stub.connect("localhost:50051", codec: MyApp.CustomJSONCodec)

request = %MyApp.HelloRequest{name: "World"}
{:ok, reply} = MyApp.Greeter.Stub.say_hello(channel, request)
```

#### Override Codec Per Request

```elixir
{:ok, channel} = GRPC.Stub.connect("localhost:50051")

request = %MyApp.HelloRequest{name: "World"}
{:ok, reply} = MyApp.Greeter.Stub.say_hello(channel, request, codec: MyApp.CustomJSONCodec)
```

### Multiple Codecs

Servers can support multiple codecs simultaneously. The codec is selected based on the `content-type` header sent by the client:

```elixir
defmodule MyApp.Greeter.Server do
  use GRPC.Server,
    service: MyApp.Greeter.Service,
    codecs: [
      GRPC.Codec.Proto,        # application/grpc+proto
      MyApp.CustomJSONCodec,   # application/grpc+custom-json
      MyApp.MsgPackCodec       # application/grpc+msgpack
    ]

  def say_hello(request, _stream) do
    %MyApp.HelloReply{message: "Hello, #{request.name}!"}
  end
end
```

Different clients can connect using different codecs:

```elixir
# Client 1 uses Proto
{:ok, channel1} = GRPC.Stub.connect("localhost:50051", codec: GRPC.Codec.Proto)

# Client 2 uses custom JSON
{:ok, channel2} = GRPC.Stub.connect("localhost:50051", codec: MyApp.CustomJSONCodec)

# Client 3 uses MessagePack
{:ok, channel3} = GRPC.Stub.connect("localhost:50051", codec: MyApp.MsgPackCodec)
```

## Content-Type Negotiation

When a client sends a request, it includes a `content-type` header:

```
content-type: application/grpc+custom-json
```

The server:

1. Extracts the codec name from the content-type (e.g., `custom-json`)
2. Searches for a registered codec with a matching `name/0` value
3. Uses that codec to decode the request and encode the response
4. Returns an error if no matching codec is found

## Error Handling

If a client uses a codec that is not registered on the server:

```elixir
defmodule UnregisteredCodec do
  @behaviour GRPC.Codec
  def name(), do: "unregistered"
  def encode(struct, _opts \\ []), do: inspect(struct)
  def decode(binary, _module), do: binary
end

{:ok, channel} = GRPC.Stub.connect("localhost:50051", codec: UnregisteredCodec)
request = %MyApp.HelloRequest{name: "Test"}

{:error, %GRPC.RPCError{status: 12}} = MyApp.Greeter.Stub.say_hello(channel, request)
# Status 12 = UNIMPLEMENTED
```

The server returns:

```
status: :unimplemented
message: "No codec registered for content-type application/grpc+unregistered"
```

## HTTP Transcoding and Custom Codecs

> **⚠️ IMPORTANT LIMITATION**
>
> **Custom codecs are NOT supported for HTTP transcoding.**
>
> When using `http_transcode: true`, the library **always** uses `GRPC.Codec.JSON` for encoding and decoding HTTP/JSON requests and responses, regardless of any custom codecs configured.

### Why This Limitation Exists

HTTP transcoding is designed to support the standard HTTP/JSON to gRPC mapping as defined by Google's [HTTP/JSON transcoding specification](https://cloud.google.com/endpoints/docs/grpc/transcoding). This specification mandates JSON as the serialization format.

The library automatically adds `GRPC.Codec.JSON` to the codec list when `http_transcode: true`

### Example: What Does NOT Work

```elixir
defmodule MyApp.Greeter.Server do
  use GRPC.Server,
    service: MyApp.Greeter.Service,
    http_transcode: true,
    codecs: [MyApp.CustomJSONCodec]  # Will be ignored for HTTP transcoding

  def say_hello(request, _stream) do
    %MyApp.HelloReply{message: "Hello!"}
  end
end
```

When making an HTTP/JSON request:

```bash
curl -X POST http://localhost:50051/myapp.Greeter/SayHello \
  -H "Content-Type: application/json" \
  -d '{"name": "World"}'
```

The response will **always** be encoded using `GRPC.Codec.JSON`, not `MyApp.CustomJSONCodec`.

### Workaround

If you need custom encoding for HTTP endpoints, consider:

1. **Disable HTTP transcoding** and use standard gRPC with your custom codec
2. **Create a separate HTTP endpoint** (e.g., using Phoenix) that proxies to your gRPC service

## Best Practices

1. **Choose descriptive names:** Use clear, unique names for your codecs (e.g., `"custom-msgpack"`, `"custom-json"`)

2. **Handle errors gracefully:** Implement proper error handling in `encode/2` and `decode/2`

3. **Test compatibility:** Ensure your codec works with both client and server

4. **Document content-type:** Make it clear to users what `content-type` header will be used

5. **Consider performance:** Custom codecs may have different performance characteristics than Protocol Buffers

6. **Avoid conflicts:** Don't use codec names that conflict with built-in codecs (`"proto"`, `"json"`, `"text"`)

7. **Remember HTTP transcoding limitation:** Custom codecs do NOT work with HTTP transcoding

## Summary

- Custom codecs allow you to use alternative serialization formats with gRPC
- Implement the `GRPC.Codec` behaviour with `name/0`, `encode/2`, and `decode/2`
- Register codecs in both server and client configurations
- Servers can support multiple codecs simultaneously
- Codec selection is based on the `content-type` header
- **Custom codecs are NOT supported for HTTP transcoding** - only `GRPC.Codec.JSON` is used

For more examples, see the test suite:
- `test/grpc/codec/proto_test.exs`
