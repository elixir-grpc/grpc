# Helloworld in grpc-elixir

## Usage

Install deps and compile

```shell
$ mix do deps.get, compile
```

Run the script

```shell
$ mix run priv/main.exs
```

## Regenerate Elixir code from proto

Modify the proto `priv/protos/helloworld.proto`

Run mix task `grpc.gen`:

```shell
$ mix grpc.gen priv/protos/helloworld.proto --out lib/
```

View more options for `grpc.gen`:

```shell
$ mix help grpc.gen
```
