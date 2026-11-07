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
     max_connection_age_grace: :timer.seconds(30),
     max_connection_idle: :timer.minutes(1)
   ]}
]
```

  * `:max_connection_age` - the age, in milliseconds, at which the server starts to close a connection. The default is `:infinity`: the server never closes a connection because of its age.
  * `:max_connection_age_grace` - the time, in milliseconds, that open calls have to finish after the server starts to close the connection. The default is `:infinity`. This keeps the Cowboy default, `goaway_complete_timeout`, of 3000 ms. A number sets `goaway_complete_timeout`.
  * `:max_connection_idle` - the time, in milliseconds, that a connection has no open call before the server starts to close it. The default is `:infinity`.

Each value is `:infinity` or a positive integer. Other values raise `ArgumentError` when the server starts.

With the defaults, nothing changes.

## What the server does

The server picks the age of each connection at random, from 90% to 110% of `:max_connection_age`. This stops all the connections of one client from closing at the same time.

When the age ends, or the connection has been idle for `:max_connection_idle`, the server:

  1. Sends GOAWAY with the last stream id `2^31 - 1`. This tells the client to open no more streams on this connection.
  2. Waits `goaway_initial_timeout` (Cowboy default: 1000 ms), so that streams that the client already sent can arrive.
  3. Sends GOAWAY with the id of the last stream it accepted. It refuses all later streams.
  4. Waits for the open calls to finish. When the last call finishes, the server closes the connection.
  5. After `:max_connection_age_grace`, closes the connection. Calls that are still open end. The server publishes `[:grpc, :server, :rpc, :abort]` for each of them.

The client sees GOAWAY and lets the open calls finish on the old one. Gun and Mint with the pool send new calls on a new connection. See "What the client does".

## What the client does

The client lets the open calls finish on the old connection and sends new calls to a new one:

  * The Gun adapter does this by default.
  * The Mint adapter does this with the connection pool (`config :grpc, pool_enabled: true`). The pool opens a new connection when the old one gets GOAWAY, and removes the old one when its last call ends.
  * The Mint adapter without the pool fails a call that starts during the drain with `UNAVAILABLE`. The client connects again when the drain ends. The pool also fails a call that it gave to the old connection just before the GOAWAY, with the same status. The server did not process these calls, so the client can retry them.

Mint takes no request body after GOAWAY, not even the end of a stream. With the Mint adapter, a client-streaming or bidirectional call that is open at GOAWAY and has not ended its request stream fails with `UNAVAILABLE`. The server handler of that call waits for more messages until the connection closes or `:max_connection_age_grace` ends. Keep such calls short, set a `:max_connection_age_grace` that fits them, or use the Gun adapter.

## Where the age counts from

The age counts from the first call on the connection, not from the moment the server accepts the connection. A connection that sends no call is not closed because of its age.

## Idle time

A connection is idle when it has no open call. The server counts the open calls of each connection. The idle time starts when the connection gets its first call. It starts again each time the last open call ends. A new call ends the idle time.

The idle time is the same for all connections. It has no jitter. While a call is open, the server checks the connection one time for each `:max_connection_idle`. This is one wake of one process for each idle time.

When the idle time ends, the server closes the connection in the same way as for its age. A new call can arrive before it does. Cowboy does not lose that call. The first GOAWAY has the last stream id `2^31 - 1`, so Cowboy still accepts the call. Cowboy lets the call finish before it closes the connection. `:max_connection_age_grace` limits this time. A call that arrives after the second GOAWAY is refused. The client can send it again on a new connection.

You can set `:max_connection_age` and `:max_connection_idle` together. The first one to end closes the connection.

Cowboy's `idle_timeout` is not the same. It ignores open streams, and it closes the connection at once.

### A connection with no call

A9 measures the idle time from the creation of the connection. This adapter measures it from the first call. A connection that never sends a call is never closed because it is idle.

The reason is that Cowboy has no hook that runs in the connection process before the first stream. The adapter starts its helper process from the stream handler, so it starts with the first stream.

Only Cowboy's `idle_timeout` limits such a connection. This adapter sets `idle_timeout` to `:infinity` unless you set it in `adapter_opts`. Set `:idle_timeout` if you need to limit connections that send no call. Cowboy then also closes a connection that has open calls and no data for that time. Use a value that is longer than your longest call.

## Not in the options

  * The idle time does not start at the creation of the connection, as A9 says. See "A connection with no call".
  * The server does not send a PING after the first GOAWAY, as A9 suggests. The `goaway_initial_timeout` timer takes its place.
  * Keepalive PINGs from the server (`KEEPALIVE_TIME` and `KEEPALIVE_TIMEOUT`) are not available.
  * The server does not enforce the rate of PINGs from the client (`PERMIT_KEEPALIVE_TIME`).

Cowboy blocks the last two. The code that sends and reads PINGs must run in the connection process. Cowboy gives no way to do this from outside:

  * No process other than the connection process can send a PING. The connection process does not take a message for it.
  * Cowboy answers each PING from the client with an ack. It drops the PING acks that it gets, and it tells no one about PING frames.

We do not read the connection state with `:sys.get_state/1` or write to the socket to get around this.

The smallest change in Cowboy has two parts:

  1. A clause in the `cowboy_http2` loop for the message `{'$cowboy_send_ping', Opaque}`. It sends a PING frame with the 8 byte value `Opaque`.
  2. An option with a callback. Cowboy calls it when a PING or a PING ack arrives, with the value and the time. The adapter can then match each ack to its PING. It can close the connection when an ack is late. It can also count the PINGs from the client.

With this change, the helper in this adapter can send the PINGs and check the acks.

## Notes

  * `:max_connection_age_grace` also limits the time that open calls have when the server stops.
  * The age applies to HTTP/2 connections only.
  * The Mint client adapter lets the open calls finish after GOAWAY. Earlier versions failed them with `UNAVAILABLE`. See "What the client does" for new calls and for calls that stream a request.
