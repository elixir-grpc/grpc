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