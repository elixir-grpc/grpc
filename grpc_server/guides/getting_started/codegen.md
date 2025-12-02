# Codegen

Use `protoc` with [protobuf elixir plugin](https://github.com/elixir-protobuf/protobuf) or using [protobuf_generate](https://hexdocs.pm/protobuf_generate/readme.html) hex package to generate the necessary files.

---

## Write your protobuf file

```protobuf
syntax = "proto3";

package helloworld;

// The request message containing the user's name.
message HelloRequest {
  string name = 1;
}

// The response message containing the greeting
message HelloReply {
  string message = 1;
}

// The greeting service definition.
service GreetingServer {
  rpc SayUnaryHello (HelloRequest) returns (HelloReply) {}
  rpc SayServerHello (HelloRequest) returns (stream HelloReply) {}
  rpc SayBidStreamHello (stream HelloRequest) returns (stream HelloReply) {}
}
```

## Compile protos (protoc + elixir plugin)

The most basic way to compile protobuf files is by using the elixir plugin for the protoc compiler:

```bash
protoc --elixir_out=plugins=grpc:./lib -I./priv/protos helloworld.proto
```

See more detailed explanation [here](https://hexdocs.pm/protobuf/readme.html#generate-elixir-code).

But you can also benefit from more advanced options if you use the protobuf_generator. This is especially useful for use with HTTP Transcoding:

```shell
mix protobuf.generate  \ 
  --include-path=priv/proto \ 
  --include-path=deps/googleapis  \ 
  --generate-descriptors=true \ 
  --output-path=./lib \ 
  --plugins=ProtobufGenerate.Plugins.GRPCWithOptions \ 
  google/api/annotations.proto google/api/http.proto helloworld.proto
```

See more detailed explanation [here](https://hexdocs.pm/protobuf_generate/readme.html).