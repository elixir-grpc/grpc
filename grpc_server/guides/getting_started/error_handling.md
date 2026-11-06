# Error Handling

Effective error management is essential for maintaining reliability in gRPC streaming pipelines. In Elixir gRPC, all stream operators participate in a unified error propagation model, ensuring that failures — whether returned as {:error, reason} tuples or raised unexpectedly — are captured and translated consistently throughout the dataflow.

Developers can intercept, transform, and recover from errors using dedicated operators such as `map_error/2`, enabling graceful degradation, domain-specific responses, and seamless conversion into `GRPC.RPCError` formats that propagate correctly to clients.

This document explains how streaming error handling works, how exceptions interact with the pipeline, and how to design resilient services that continue processing even when individual elements fail.

---

## Recovery from errors

The `map_error/2` operator intercepts and transforms errors or exceptions emitted by previous stages in a stream pipeline.

It provides a unified mechanism for handling:

* Expected errors, such as validation or domain failures (`{:error, reason}`)
* Unexpected runtime errors, including raised or thrown exceptions inside other operators.

```elixir
iex> GRPC.Stream.from([1, 2])
...> |> GRPC.Stream.map(fn
...>   2 -> raise "boom"
...>   x -> x
...> end)
...> |> GRPC.Stream.map_error(fn
...>   {:error, {:exception, _reason}} ->
...>     {:error, GRPC.RPCError.exception(message: "Booomm")}
...> end)
``` 

In this example:

* The function inside `map/2` raises an exception for the value `2`.
* `map_error/2` captures and transforms that error into a structured `GRPC.RPCError` response.
* The stream continues processing without being interrupted.

This makes `map_error/2` suitable for input validation, runtime fault recovery, and user-facing error translation within gRPC pipelines.

---

## Unified Error Matching and Propagation

All stream operators share a unified error propagation model that guarantees consistent handling of exceptions and failures across the pipeline.

This ensures that user-defined functions within the stream — whether pure transformations, side effects, or external calls — always produce a predictable and recoverable result, maintaining the integrity of the dataflow even in the presence of unexpected errors.

```elixir
def say_unary_hello(request, _materializer) do
  GRPCStream.unary(request)
  |> GRPCStream.ask(Transformer)
  |> GRPCStream.map(fn
    %HelloReply{} = reply ->
      %HelloReply{message: "[Reply] #{reply.message}"}

    {:error, reason} ->
      {:error, GRPC.RPCError.exception(message: "error calling external process: #{inspect(reason)}")}
    
    error ->
      Logger.error("Unknown error")
      error
  end)
  |> GRPCStream.run()
end
```

By normalizing all possible outcomes, `GRPC.Stream` ensures fault-tolerant, exception-safe pipelines where operators can freely raise, throw, or return tuples without breaking the flow execution.

This unified model allows developers to build composable and reliable streaming pipelines that gracefully recover from both domain and runtime errors.

>_Note_: In the example above, we could use `map_error/2` instead of `map/2` to handle error cases explicitly. However, since the function also performs a transformation on successful values, `map/2` remains appropriate and useful in this context.

---

## Report server errors

When a handler fails, the client gets an error status and the server continues to run. To send the error to an error reporter, for example Sentry or Bugsnag, use one of two signals. Both signals are on by default.

* **Telemetry.** Two events have `:kind`, `:reason` and `:stacktrace`:
  * `[:grpc, :server, :rpc, :exception]`: the handler raises, throws or exits.
  * `[:grpc, :server, :rpc, :send_error]`: the handler returns, but the reply fails. The codec cannot encode it, a compressor raises, or the adapter does not send it.

  The two events are different because the telemetry span of the handler is closed when the reply fails. Refer to `GRPC.Telemetry` for the metadata of each event. `:send_error` has two measurements. `:duration` is the native time from the start of the pipeline to the failure. `:monotonic_time` is the time of the failure (`System.monotonic_time/0`). Its metadata is `:stream`, `:server`, `:endpoint`, `:path`, `:kind`, `:reason` and `:stacktrace`.
* **Logger.** The Cowboy adapter logs these failures with `:crash_reason` metadata: a handler failure, an encoding failure and a compressor that raises. Logger-based reporters, for example `Sentry.LoggerHandler`, read this metadata. The `exception_log_filter` option can remove these logs. The adapter does not log a reply that it does not send.

A client that disconnects during the reply does not cause an error on the server. To write a reply, the server sends a message to the connection process, so the call does not fail. The adapter stops the RPC and publishes `[:grpc, :server, :rpc, :abort]` with the exit reason (`:shutdown`). This event has no `:kind` and no `:stacktrace`, and the adapter logs nothing. To count these disconnects, attach to `:abort`.

To send reports from telemetry, attach a handler when the application starts:

```elixir
:telemetry.attach_many(
  "report-grpc-server-errors",
  [[:grpc, :server, :rpc, :exception], [:grpc, :server, :rpc, :send_error]],
  &MyApp.GRPCErrors.handle_event/4,
  nil
)

defmodule MyApp.GRPCErrors do
  # A handler returns these statuses on purpose. Do not report them.
  def handle_event(_event, _measurements, %{reason: %GRPC.RPCError{status: status}}, _config)
      when status in [1, 3, 5, 6, 7, 9, 16],
      do: :ok

  def handle_event(_event, _measurements, %{kind: :error} = meta, _config) do
    Sentry.capture_exception(Exception.normalize(:error, meta.reason, meta.stacktrace),
      stacktrace: meta.stacktrace
    )
  end

  def handle_event(_event, _measurements, %{kind: kind, reason: reason}, _config) do
    Sentry.capture_message("gRPC handler #{kind}: #{inspect(reason)}")
  end
end
```

The handler runs in the process that failed. Keep the handler short. Replace the `Sentry` calls with the calls of your reporter.
