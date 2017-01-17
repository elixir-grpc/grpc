# RouteGuide in grpc-elixir

## Usage

1. Install deps and compile

  ```
  $ mix do deps.get, compile
  ```

2. Run the server

  ```
  $ mix grpc.server
  ```

2. Run the client

  ```
  $ mix run priv/client.exs
  ```

## Regenerate Elixir code from proto

1. Modify the proto `priv/route_guide.proto`

2. Run mix task `grpc.gen`:

  ```
  $ mix grpc.gen priv/route_guide.proto --out lib/
  ```

View more options for `grpc.gen`:

```
$ mix help grpc.gen
```

## Authentication

```
$ TLS=true mix grpc.server
$ TLS=true mix run priv/client.exs
```

## FAQ

* Change log level? Check out `config/config.exs`, default to warn
* Use local grpc-elixir? Uncomment `{:grpc, path: "../../"}` in `mix.exs`
* Output format of `Feature` & `Point` is different from normal map? Check out `lib/inspect.ex`
* How to start server when starting your application?

  Change the config to:

  ```elixir
  config :grpc, start_server: true
  ```
