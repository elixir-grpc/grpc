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

## Reporting server errors

When a handler fails, the client gets an error status and the server keeps running. To tell an error reporter such as Sentry or Bugsnag, use one of two signals. Both are on by default.

* **Telemetry.** Two events carry `:kind`, `:reason` and `:stacktrace`. `[:grpc, :server, :rpc, :exception]` is published when the handler raises, throws or exits. `[:grpc, :server, :rpc, :send_error]` is published when the handler returned but the reply failed: the codec rejects it, a compressor raises, or the adapter refuses to send it. They are separate because the handler's telemetry span has closed by the time the reply fails. See `GRPC.Telemetry` for the metadata of each.
* **Logger.** The Cowboy adapter logs a handler failure, an encoding failure or a raising compressor with `:crash_reason` metadata, which Logger-based reporters read, such as `Sentry.LoggerHandler`. The `exception_log_filter` option can drop these logs. A reply the adapter refuses to send is not logged.

A client that disconnects while the server is still replying is not an error the server can raise: writing a reply is an asynchronous message to the connection process, so the call never fails. The adapter stops the RPC and publishes `[:grpc, :server, :rpc, :abort]` with the exit reason (`:shutdown`). It has no `:kind` or `:stacktrace`, and nothing is logged. Attach to `:abort` if you want to count these.

To report from telemetry, attach a handler at application start:

```elixir
:telemetry.attach_many(
  "report-grpc-server-errors",
  [[:grpc, :server, :rpc, :exception], [:grpc, :server, :rpc, :send_error]],
  &MyApp.GRPCErrors.handle_event/4,
  nil
)

defmodule MyApp.GRPCErrors do
  # Statuses a handler returns on purpose are not worth a report.
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

The handler runs in the process that failed, so keep it short. Swap the `Sentry` calls for your reporter's.
