# RouteGuide in grpc-elixir

## Usage

1. Install deps and compile

  ```
  $ mix do deps.get, compile
  ```

2. Run the script

  ```
  $ mix run priv/main.exs
  ```

Or run server and client separately in iex, check `priv/main.exs`

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

## FAQ

* Change log level? Check `config/config.exs`, default to warn
* Use local grpc-elixir? Uncomment `{:grpc, path: "../../"}` in `mix.exs`
* Output format of `Feature` & `Point` is different from normal map? Check `lib/inspect.ex`
