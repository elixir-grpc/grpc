# Helloworld in grpc-elixir

## Usage

1. Install deps and compile
```shell
$ mix do deps.get, compile
```

2. Run the server
```shell
$ mix run --no-halt
```

3. Run the client script
```shell
$ mix run priv/client.exs
```

## HTTP Transcoding

``` shell
# Say hello
curl -H 'Content-type: application/json' http://localhost:50051/v1/greeter/test

# Say hello from
curl -XPOST -H 'Content-type: application/json' -d '{"name": "test", "from": "anon"}' http://localhost:50051/v1/greeter
```

## Regenerate Elixir code from proto

1. Modify the proto `priv/protos/helloworld.proto`
2. Install `protoc` [here](https://developers.google.com/protocol-buffers/docs/downloads)
3. Install `protoc-gen-elixir`
```
mix escript.install hex protobuf
```
4. Generate the code:

```shell
$ (cd ../../; mix build_protobuf_escript && mix escript.build)
$ protoc -I priv/protos --elixir_out=:./lib/ --grpc_elixir_out=./lib --plugin="../../deps/protobuf/protoc-gen-elixir" --plugin="../../protoc-gen-grpc_elixir" priv/protos/helloworld.proto
```

Refer to [protobuf-elixir](https://github.com/tony612/protobuf-elixir#usage) for more information.

## How to start server when starting your application?

Pass `start_server: true` as an option for the `GRPC.Server.Supervisor` in your supervision tree.

## Benchmark

Using [ghz](https://ghz.sh/)

```
$ MIX_ENV=prod iex -S mix
# Now cowboy doesn't work well with concurrency in a connection, like --concurrency 6 --connections 1
$ ghz --insecure --proto priv/protos/helloworld.proto --call helloworld.Greeter.SayHello -d '{"name":"Joe"}' -z 10s  --concurrency 6 --connections 6 127.0.0.1:50051
# The result is for branch improve-perf
Summary:
  Count:	124239
  Total:	10.00 s
  Slowest:	18.85 ms
  Fastest:	0.18 ms
  Average:	0.44 ms
  Requests/sec:	12423.71

# Go
Summary:
  Count:	258727
  Total:	10.00 s
  Slowest:	5.39 ms
  Fastest:	0.09 ms
  Average:	0.19 ms
  Requests/sec:	25861.68
```
