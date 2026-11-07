# Connection age

A gRPC client keeps one HTTP/2 connection for many calls. This is a problem when the client balances calls over several servers, for example with `round_robin` behind a Kubernetes headless service. The client opens its connections one time. When you add replicas later, the clients do not know about them. The new replicas get no calls until the clients restart.

The fix is to make the server close its connections after some time. The client then connects again, and its resolver and balancer pick up the new replicas. This follows [gRPC proposal A9](https://github.com/grpc/proposal/blob/master/A9-server-side-conn-mgt.md).

## Options

Set these in `adapter_opts` of your endpoint or supervisor:

```elixir
children = [
  {GRPC.Server.Supervisor,
   endpoint: MyApp.Endpoint,
   port: 50051,
   start_server: true,
   adapter_opts: [
     max_connection_age: :timer.minutes(5),
     max_connection_age_grace: :timer.seconds(30)
   ]}
]
```

  * `:max_connection_age` - the age, in milliseconds, at which the server starts to close a connection. The default is `:infinity`: the server never closes a connection because of its age.
  * `:max_connection_age_grace` - the time, in milliseconds, that open calls have to finish after the server starts to close the connection. The default is `:infinity`. This keeps the Cowboy default, `goaway_complete_timeout`, of 3000 ms. A number sets `goaway_complete_timeout`.

Each value is `:infinity` or a positive integer. Other values raise `ArgumentError` when the server starts.

With the defaults, nothing changes.

## What the server does

The server picks the age of each connection at random, from 90% to 110% of `:max_connection_age`. This stops all the connections of one client from closing at the same time.

When the age ends, the server:

  1. Sends GOAWAY with the last stream id `2^31 - 1`. This tells the client to open no more streams on this connection.
  2. Waits `goaway_initial_timeout` (Cowboy default: 1000 ms), so that streams that the client already sent can arrive.
  3. Sends GOAWAY with the id of the last stream it accepted. It refuses all later streams.
  4. Waits for the open calls to finish. When the last call finishes, the server closes the connection.
  5. After `:max_connection_age_grace`, closes the connection. Calls that are still open end. The server publishes `[:grpc, :server, :rpc, :abort]` for each of them.

The client sees GOAWAY, sends new calls on a new connection, and lets the open calls finish on the old one.

## Where the age counts from

The age counts from the first call on the connection, not from the moment the server accepts the connection. A connection that sends no call is not closed because of its age.

## Not in the options

  * A9 also defines `MAX_CONNECTION_IDLE`. It is not available. Cowboy's `idle_timeout` does not do the same: it ignores open streams and closes the connection at once.
  * The server does not send a PING after the first GOAWAY, as A9 suggests. The `goaway_initial_timeout` timer takes its place.
  * Keepalive PINGs from the server are not available.

## Notes

  * `:max_connection_age_grace` also limits the time that open calls have when the server stops.
  * The age applies to HTTP/2 connections only.
  * The Mint client adapter lets the open calls finish after GOAWAY. Earlier versions failed them with `UNAVAILABLE`.
