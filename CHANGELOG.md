# Changelog

## 0.9.0 (Unreleased)

## 0.8.0 (2023-01-30)

- Updates to the recent upstream `elixir-grpc` that includes Mint support [`f5f5fab4`](https://github.com/elixir-grpc/grpc/commit/f5f5fab412dcf37826e583980a61190e865e96be)

### Added `mint` client adapter

Now, when spawning your connections it is possible for developers to choose 
which adapter to use.

```elixir
GRPC.Stub.connect("localhost:5001", adapter: GRPC.Client.Adapters.Mint)
# or (Gun is the current default adapter)
GRPC.Stub.connect("localhost:5001")
# the above is has the same effect as: 
GRPC.Stub.connect("localhost:5001", adapter: GRPC.Client.Adapters.Gun) 
```

### Why choose `mint` over `gun`?
Both clients will solve the same problems and recent load tests showed almost no
difference of performance between them. 

The reason why the additional `Mint` adapter is introduced, is because we are 
attempting to integrate with LaunchDarkly and their erlang client is 
incompatible with the current version of Gun which `elixir-grpc` is using. 
Using an adapter based on `Mint` resolves the `gun` version conflict. 

## 0.7.0 (2022-08-10)
- Updates to the recent upstream release of `elixir-grpc` [`v0.5.0`](https://github.com/elixir-grpc/grpc/releases/tag/v0.5.0)

### Breaking Changes

- Spawn supervisor now expects a Keyword list instead of a tuple. 
  - Before: 
   ``` elixir
      def children, do: [
      # ... other supervisors
        {GRPC.Server.Supervisor, {MyCoolEndPoint, @my_grpc_port}}
      ]
  ```
  - Now: 
  ```elixir
      def children, do: [
      # ... other supervisors
        {GRPC.Server.Supervisor, endpoint: MyEndpoint, port: @my_grpc_port, server_enabled: true}
      ]
   ```
- `server_enable` config is no longer defined in the application config, you should pass is as an option when spawning the supervisor, like in the example above
- `adapter_opts` now expects a keyword list instead of a map.
  - Before:
  ```elixir
      [adapter_opts: %{http2_opts: %{keepalive: @keep_alive}}]
  ``` 
  - After:
  ```elixir
      [adapter_opts: [http2_opts: %{keepalive: @keep_alive}]]
  ```

