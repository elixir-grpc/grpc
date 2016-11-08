# Helloworld in grpc-elixir

## Usage

1. Install deps and compile

  ```shell
  $ mix do deps.get, compile
  ```

2. Run the script

  ```shell
  $ mix run priv/main.exs
  ```

Or you can run server and client separately in iex, check `priv/main.exs`

## Regenerate Elixir code from proto

1. Modify the proto `priv/protos/helloworld.proto`

2. Run mix task `grpc.gen`:

  ```shell
  $ mix grpc.gen priv/protos/helloworld.proto --out lib/
  ```

View more options for `grpc.gen`:

```shell
$ mix help grpc.gen
```
