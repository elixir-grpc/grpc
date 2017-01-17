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

2. Run mix task `grpc.gen`:

  ```shell
  $ mix grpc.gen priv/protos/helloworld.proto --out lib/
  ```

View more options for `grpc.gen`:

```shell
$ mix help grpc.gen
```

## How to start server when starting your application?

Change the config to:

```elixir
config :grpc, start_server: true
```
