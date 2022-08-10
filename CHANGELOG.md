# Changelog

## 0.8.0 (Unreleased)

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

