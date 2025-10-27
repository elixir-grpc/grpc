defmodule GRPC.Stream do
  @moduledoc """
  Provides a `Flow`-based abstraction layer for building gRPC streaming pipelines in Elixir.

  This module allows you to consume gRPC request streams as `Flow` pipelines with support for
  backpressure via GenStage. You can also produce gRPC responses by materializing a `Flow`
  back into the gRPC stream.

  ## Capabilities

  - Transforms an incoming gRPC request stream into a `Flow` with backpressure.
  - Emits messages back into the gRPC response stream using `run_with/3`.
  - Supports joining with external producers (e.g., RabbitMQ, Kafka) for unbounded or fan-in stream sources.
  - Offers composable functional operators (`map/2`, `filter/2`, `flat_map/2`, etc.) on the stream.

  ## Example: Bidirectional Streaming

      defmodule MyGRPCService do
        use GRPC.Server, service: MyService.Service

        def route_chat(input, materializer) do
          GRPC.Stream.from(input, max_demand: 10)
          |> GRPC.Stream.map(fn note -> process_note(note) end)
          |> GRPC.Stream.run_with(materializer)
        end

        defp process_note(note), do: %Response{message: "Received"}
      end

  ## Example: Joining with an External Producer

  When integrating with external unbounded sources (e.g., message queues),
  you can pass a running `GenStage` producer using the `:join_with` option:

      defmodule MyGRPCService do
        use GRPC.Server, service: MyService.Service

        def stream_events(input, materializer) do
          {:ok, pid} = MyApp.RabbitMQ.Producer.start_link([])

          GRPC.Stream.from(input, join_with: pid, max_demand: 10)
          |> GRPC.Stream.map(&handle_event/1)
          |> GRPC.Stream.run_with(materializer)
        end

        defp handle_event({_, msg}), do: msg
        defp handle_event(event), do: %MyGRPC.Event{data: inspect(event)}
      end
  """
  alias GRPC.Stream.Operators
  alias GRPC.Server.Stream, as: Materializer

  defstruct flow: nil, options: [], metadata: %{}

  @type t :: %__MODULE__{flow: Flow.t(), options: Keyword.t(), metadata: map()}

  @type item :: any()

  @type reason :: any()

  @doc """
  Converts a gRPC input into a `Flow` pipeline with backpressure support.

  ## Parameters

    - `input`: A gRPC request stream (struct, enumerable, or Elixir `Stream`).

  ## Options

    - `:join_with` — An optional external `GenStage` producer to merge with the gRPC input.
    - `:dispatcher` — Specifies the `Flow` dispatcher (defaults to `GenStage.DemandDispatcher`).
    - `:propagate_context` - If `true`, the context from the `materializer` is propagated to the `Flow`.
    - `:materializer` - The `%GRPC.Server.Stream{}` struct representing the current gRPC stream context.

  And any other options supported by `Flow`.

  ## Returns

    A `GRPC.Stream` struct that represents the transformed stream.

  ## Example

      flow = GRPC.Stream.from(request, max_demand: 50)
  """
  @spec from(any(), Keyword.t()) :: t()
  def from(input, opts \\ [])

  def from(%Elixir.Stream{} = input, opts), do: build_grpc_stream(input, opts)

  def from(input, opts) when is_list(input), do: build_grpc_stream(input, opts)

  def from(input, opts) when not is_nil(input), do: from([input], opts)

  @doc """
  Converts a single gRPC request into a `Flow` pipeline with support for backpressure.
  This is useful for unary gRPC requests where you want to use the Flow API.

  ## Parameters

    - `input`: The single gRPC message to convert into a Flow.

  ## Options

    - `:join_with` - An optional additional producer stage PID to include in the Flow.
    - `:dispatcher` - An optional `GenStage` dispatcher to use in the underlying `Flow`. Defaults to `GenStage.DemandDispatcher`.
    - `:propagate_context` - If `true`, the context from the `materializer` is propagated to the `Flow`.
    - `:materializer` - The `%GRPC.Server.Stream{}` struct representing the current gRPC stream context.

  And any other options supported by `Flow`.

  ## Returns
    - A `GRPCStream` that emits the single gRPC message under demand.

  ## Example

      flow = GRPCStream.single(request, max_demand: 5)
  """
  @spec unary(any(), Keyword.t()) :: t()
  def unary(input, opts \\ []) when is_struct(input),
    do: build_grpc_stream([input], Keyword.merge(opts, unary: true))

  @doc """
  Extracts the underlying `Flow` pipeline from a `GRPC.Stream`.

  Raises an `ArgumentError` if the `Flow` has not been initialized.

  ## Returns

    A `Flow` pipeline.
  """
  @spec to_flow(t()) :: Flow.t()
  def to_flow(%__MODULE__{flow: flow}) when is_nil(flow), do: Flow.from_enumerable([])

  def to_flow(%__MODULE__{flow: flow}), do: flow

  @doc """
  Executes the underlying `Flow` for a unary stream.

  The response will be emitted automatically to the provided
  `:materializer` (set to a `GRPC.Server.Stream`) for the single resulting
  item in the materialized enumerable.

  The `stream` argument must be initialized as a `:unary` stream with
  a `:materializer` set.
  """
  @spec run(stream :: t()) :: :noreply
  def run(%__MODULE__{flow: flow, options: opts}) do
    opts = Keyword.take(opts, [:unary, :materializer])

    if opts[:unary] != true do
      raise ArgumentError, "GRPC.Stream.run/1 only supports unary streams"
    end

    materializer = opts[:materializer]

    if is_nil(materializer) do
      raise ArgumentError,
            "GRPC.Stream.run/1 requires a materializer to be set in the GRPC.Stream"
    end

    send_response(materializer, Enum.at(flow, 0), opts)

    :noreply
  end

  @doc """
  Executes the flow and emits responses into the provided gRPC server stream.

  ## Parameters

    - `flow`: A `GRPC.Stream` struct containing the flow to be executed.
    - `stream`: A `GRPC.Server.Stream` to which responses are sent.

  ## Options

    - `:dry_run` — If `true`, responses are not sent (used for testing or inspection).

  ## Returns

    - `:ok` if the stream was processed successfully.

  ## Example

      GRPC.Stream.run_with(request, mat)
  """
  @spec run_with(t(), Stream.t(), Keyword.t()) :: :ok
  def run_with(
        %__MODULE__{flow: flow, options: flow_opts} = _stream,
        %Materializer{} = from,
        opts \\ []
      ) do
    if not Keyword.get(flow_opts, :unary, true) do
      raise ArgumentError, "run_with/3 is not supported for unary streams"
    end

    flow
    |> Flow.map(fn
      {:ok, msg} ->
        send_response(from, msg, opts)
        flow

      {:error, %GRPC.RPCError{} = reason} ->
        send_response(from, reason, opts)
        flow

      {:error, reason} ->
        msg = GRPC.RPCError.exception(message: "#{inspect(reason)}")
        send_response(from, msg, opts)
        flow

      msg ->
        send_response(from, msg, opts)
        flow
    end)
    |> Flow.run()
  end

  @doc """
  Applies a side-effect function to each element of the stream without altering its values.

  The `effect/2` function is useful for performing **imperative or external actions** 
  (such as logging, sending messages, collecting metrics, or debugging) 
  while preserving the original stream data.

  It behaves like `Enum.each/2`, but returns the stream itself so it can continue in the pipeline.

  ## Examples

  ```elixir
  iex> parent = self()
  iex> stream =
  ...>   GRPC.Stream.from([1, 2, 3])
  ...>   |> GRPC.Stream.effect(fn x -> send(parent, {:seen, x*2}) end)
  ...>   |> GRPC.Stream.to_flow()
  ...>   |> Enum.to_list()
  iex> assert_receive {:seen, 2}
  iex> assert_receive {:seen, 4}
  iex> assert_receive {:seen, 6}
  iex> stream
  [1, 2, 3]
  ``` 
  In this example, the effect/2 function sends a message to the current process
  for each element in the stream, but the resulting stream values remain unchanged.

  ## Parameters

  - `stream` — The input `GRPC.Stream`.
  - `effect_fun` — A function that receives each item and performs a side effect
  (e.g. IO.inspect/1, Logger.info/1, send/2, etc.).

  ### Notes

  - This function is **lazy** — the `effect_fun` will only run once the stream is materialized
  (e.g. via `GRPC.Stream.run/1` or `GRPC.Stream.run_with/3`).
  - The use of `effect/2` ensures that the original item is returned unchanged,
  enabling seamless continuation of the pipeline.
  """
  @spec effect(t(), (term -> any)) :: t()
  defdelegate effect(stream, effect_fun), to: Operators

  @doc """
  Sends a request to an external process and awaits a response.

  If `target` is a PID, a message in the format `{:request, item, from}` is sent, and a reply
  in the format `{:response, msg}` is expected.

  If `target` is an `atom` we will try to locate the process through `Process.whereis/1`.

  ## Parameters

    - `stream`: The `GRPC.Stream` pipeline.
    - `target`: Target process PID or atom name.
    - `timeout`: Timeout in milliseconds (defaults to `5000`).

  """
  @spec ask(t(), pid | atom, non_neg_integer) :: t() | {:error, :timeout | :process_not_alive}
  defdelegate ask(stream, target, timeout \\ 5000), to: Operators

  @doc """
  Same as `ask/3`, but raises an exception on failure.

  ## Caution

  This version propagates errors via raised exceptions, which can crash the Flow worker and halt the pipeline.
  Prefer `ask/3` for production usage unless failure should abort the stream.
  """
  @spec ask!(t(), pid | atom, non_neg_integer) :: t()
  defdelegate ask!(stream, target, timeout \\ 5000), to: Operators

  @doc """
  Filters the stream using the given predicate function.

  The filter function is applied concurrently to the stream entries, so it shouldn't rely on execution order.
  """
  @spec filter(t(), (term -> term)) :: t
  defdelegate filter(stream, filter), to: Operators

  @doc """
  Applies a function to each entry and concatenates the resulting lists.

  Useful for emitting multiple messages for each input.
  """
  @spec flat_map(t, (term -> Enumerable.t())) :: t()
  defdelegate flat_map(stream, flat_mapper), to: Operators

  @doc """
  Applies a function to each stream item.
  """
  @spec map(t(), (term -> term)) :: t()
  defdelegate map(stream, mapper), to: Operators

  @doc """
  Intercepts and transforms error tuples or unexpected exceptions that occur
  within a gRPC stream pipeline.

  `map_error/3` allows graceful handling or recovery from errors produced by previous
  operators (e.g. `map/2`, `flat_map/2`) or from validation logic applied to incoming data.

  The provided `handler/1` function receives the error reason (or the exception struct) like:

      {:error, reason} -> failure
      {:error, {:exception, exception}} -> failure due to exception
      {:error, {kind, reason}} -> failure due to throw or exit

  And can either:

    * Return a new error tuple — e.g. `{:error, new_reason}` — to re-emit a modified error.
    * Return any other value to recover from the failure and continue the pipeline.

  This makes it suitable for both input validation and capturing unexpected runtime errors
  in stream transformations.

  ## Parameters

    - `stream` — The input stream or `Flow` pipeline.
    - `func` — A function that takes an error reason or exception and returns either a new value or an error tuple.

  ## Returns

    - A new stream where all error tuples and raised exceptions are processed by `func/1`.

  ## Examples

      iex> GRPC.Stream.from([1, 2])
      ...> |> GRPC.Stream.map(fn
      ...>   2 -> raise "boom"
      ...>   x -> x
      ...> end)
      ...> |> GRPC.Stream.map_error(fn
      ...>   {:error, {:exception, _reason}} ->
      ...>     {:error, GRPC.RPCError.exception(message: "Validation or runtime error")}
      ...> end)

  In this example:

  * The call to `GRPC.Stream.map/2` raises an exception for value `2`.
  * `map_error/3` catches the error and wraps it in a `GRPC.RPCError` struct with a custom message.
  * The pipeline continues execution, transforming errors into structured responses.

  ## Notes

  - `map_error/3` is **lazy** and only executes when the stream is materialized
    (via `GRPC.Stream.run/1` or `GRPC.Stream.run_with/3`).

  - Use this operator to implement **robust error recovery**, **input validation**, or
    to normalize exceptions from downstream Flow stages into well-defined gRPC errors.
  """
  defdelegate map_error(stream, func), to: Operators

  @doc """
  Applies a transformation function to each stream item, passing the context as an additional argument.
  This is useful for operations that require access to the stream's headers.
  """
  @spec map_with_context(t(), (map(), term -> term)) :: t()
  defdelegate map_with_context(stream, mapper), to: Operators

  @doc """
  Partitions the stream to allow grouping of items by key or condition.

  Use this before stateful operations such as `reduce/3`.

  ## Note

  Excessive use of partitioning can impact performance and memory usage.
  Only partition when required for correctness or performance.
  See https://hexdocs.pm/flow/Flow.html#module-partitioning for more details.

  """
  @spec partition(t(), keyword()) :: t()
  defdelegate partition(stream, options \\ []), to: Operators

  @doc """
  Reduces items in the stream using an accumulator.

  ## Parameters

    - `acc_fun` initializes the accumulator,
    - `reducer_fun` updates it for each input.

  ## Note
  See https://hexdocs.pm/flow/Flow.html#reduce/3 for more details.

  """
  @spec reduce(t, (-> acc), (term(), acc -> acc)) :: t when acc: term()
  defdelegate reduce(stream, acc_fun, reducer_fun), to: Operators

  @doc """
  Emits only distinct items from the stream. See `uniq_by/2` for more information.

  """
  @spec uniq(t) :: t
  defdelegate uniq(stream), to: Operators

  @doc """
  Emits only unique items as determined by the result of the given function.

  ## Note
  This function requires care when used for unbounded flows. For more information see https://hexdocs.pm/flow/Flow.html#uniq_by/2

  """
  @spec uniq_by(t, (term -> term)) :: t
  defdelegate uniq_by(stream, fun), to: Operators

  @doc """
  Retrieves HTTP/2 headers from a `GRPC.Server.Stream`.

  ## Client Note

  To receive headers on the client side, use the `:return_headers` option. See `GRPC.Stub`.
  """
  @spec get_headers(GRPC.Server.Stream.t()) :: map
  def get_headers(%GRPC.Server.Stream{adapter: adapter} = stream) do
    headers = adapter.get_headers(stream.payload)
    GRPC.Transport.HTTP2.decode_headers(headers)
  end

  defp build_grpc_stream(input, opts) do
    metadata =
      if Keyword.has_key?(opts, :propagate_context) do
        %GRPC.Server.Stream{} = mat = Keyword.fetch!(opts, :materializer)
        get_headers(mat) || %{}
      end

    opts = Keyword.merge(opts, metadata: metadata)
    dispatcher = Keyword.get(opts, :default_dispatcher, GenStage.DemandDispatcher)

    if opts[:unary] do
      case opts[:materializer] do
        %GRPC.Server.Stream{grpc_type: :unary} ->
          :ok

        %GRPC.Server.Stream{} ->
          raise ArgumentError,
                "materializer must be set to a unary GRPC.Server.Stream when unary: true is passed"

        _ ->
          raise ArgumentError, "materializer is required when unary: true is passed"
      end
    end

    flow =
      case Keyword.get(opts, :join_with) do
        pid when is_pid(pid) ->
          opts = Keyword.drop(opts, [:join_with, :default_dispatcher])

          input_flow = Flow.from_enumerable(input, opts)
          other_flow = Flow.from_stages([pid], opts)
          Flow.merge([input_flow, other_flow], dispatcher, opts)

        name when not is_nil(name) and is_atom(name) ->
          pid = Process.whereis(name)

          if not is_nil(pid) do
            opts = Keyword.drop(opts, [:join_with, :default_dispatcher])

            input_flow = Flow.from_enumerable(input, opts)
            other_flow = Flow.from_stages([pid], opts)
            Flow.merge([input_flow, other_flow], dispatcher, opts)
          else
            raise ArgumentError, "No process found for the given name: #{inspect(name)}"
          end

        # handle Elixir.Stream joining
        other when is_list(other) or is_function(other) ->
          Flow.from_enumerables([input, other], opts)

        _ ->
          opts = Keyword.drop(opts, [:join_with, :default_dispatcher])
          Flow.from_enumerable(input, opts)
      end

    %__MODULE__{flow: flow, options: opts}
  end

  defp send_response(from, msg, opts) do
    dry_run? = Keyword.get(opts, :dry_run, false)

    if not dry_run? do
      GRPC.Server.send_reply(from, msg)
    end
  end
end
