# RouteGuide in grpc-elixir

## Usage

1. Install deps and compile
```
$ mix do deps.get, compile
```

2. Run the server
```
$ mix run --no-halt
```

2. Run the client
```
$ mix run priv/client.exs
```

## Regenerate Elixir code from proto

1. Modify the proto `priv/route_guide.proto`
2. Install `protoc` [here](https://developers.google.com/protocol-buffers/docs/downloads)
3. Install `protoc-gen-elixir`
```
mix escript.install hex protobuf
```
3. Generate the code:
```shell
$ protoc -I priv --elixir_out=plugins=grpc:./lib/ priv/route_guide.proto
```

Refer to [protobuf-elixir](https://github.com/tony612/protobuf-elixir#usage) for more information.

## Authentication

```
$ TLS=true mix run --no-halt
$ TLS=true mix run priv/client.exs
```

## FAQ

* How to change log level? Check out `config/config.exs`, default to warn
* Use local grpc-elixir? Uncomment `{:grpc, path: "../../"}` in `mix.exs`
* Why is output format of `Feature` & `Point` different from normal map? Check out `lib/inspect.ex`
