# Helloworld in grpc-elixir

## Usage

1. Install deps and compile
```shell
$ mix do deps.get, compile
```

2. Run the server
```shell
$ mix grpc.server
```

3. Run the client script
```shell
$ mix run priv/client.exs
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
$ protoc -I priv/protos --elixir_out=plugins=grpc:./lib/ priv/protos/helloworld.proto
```

Refer to [protobuf-elixir](https://github.com/tony612/protobuf-elixir#usage) for more information.

## How to start server when starting your application?

Change the config to:

```elixir
config :grpc, start_server: true
```

## Benchmark

Using [ghz](https://ghz.sh/)

```
$ MIX_ENV=prod iex -S mix
# Now cowboy doesn't work well with concurrency
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
