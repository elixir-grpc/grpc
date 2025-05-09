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
    - `opts`: Optional keyword list to configure the Flow.

  ## Options

    - `:join_with` — An optional external `GenStage` producer to merge with the gRPC input.
    - `:dispatcher` — Specifies the `Flow` dispatcher (defaults to `GenStage.DemandDispatcher`).

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
  Converts a gRPC request enumerable into a `Flow` pipeline while extracting and propagating context metadata from the `GRPC.Server.Stream` materializer.

  This is useful for unary gRPC calls that require metadata propagation within a Flow-based pipeline.

  ## Parameters

    - `input`: The enumerable stream of incoming gRPC messages.
    - `materializer`: A `%GRPC.Server.Stream{}` struct representing the current gRPC stream context.
    - `opts`: Optional keyword list for configuring the Flow or GenStage producer.

  ## Behavior

    - Automatically extracts gRPC metadata from the stream headers and injects it into the options as `:metadata`.
    - Sets `:unary` to `true` in the options to indicate unary processing.

  ## Returns

    - A `GRPC.Stream` that emits elements from the gRPC stream under demand.

  ## Example

      flow = GRPC.Stream.from_as_ctx(request, stream, max_demand: 10)

  """
  @spec from_as_ctx(any(), GRPC.Server.Stream.t(), Keyword.t()) :: t()
  def from_as_ctx(input, %GRPC.Server.Stream{} = materializer, opts \\ []) do
    meta = __MODULE__.get_headers(materializer) || %{}
    from(input, Keyword.merge(opts, metadata: meta))
  end

  @doc """
  Converts a single gRPC request into a `Flow` pipeline with support for backpressure.
  This is useful for unary gRPC requests where you want to use the Flow API.

  ## Parameters

    - `input`: The single gRPC message to convert into a Flow.
    - `opts`: Optional keyword list for configuring the Flow or GenStage producer.

  ## Options
    - `:join_with` - (optional) An additional producer stage PID to include in the Flow.
    - `:dispatcher` - (optional) The dispatcher to use for the Flow. Default is `GenStage.DemandDispatcher`.

  ## Returns
    - A `GRPCStream` that emits the single gRPC message under demand.

  ## Example

      flow = GRPCStream.single(request, max_demand: 5)
  """
  @spec single(any(), Keyword.t()) :: t()
  def single(input, opts \\ []) when is_struct(input),
    do: build_grpc_stream([input], Keyword.merge(opts, unary: true))

  @doc """
  Converts a single gRPC request into a `Flow` pipeline while extracting and propagating context metadata from the `GRPC.Server.Stream`.

  This is useful for unary gRPC calls that require metadata propagation within a Flow-based pipeline.

  ## Parameters

    - `input`: The enumerable stream of incoming gRPC messages.
    - `materializer`: A `%GRPC.Server.Stream{}` struct representing the current gRPC stream context.
    - `opts`: Optional keyword list for configuring the Flow or GenStage producer.

  ## Behavior

    - Automatically extracts gRPC metadata from the stream headers and injects it into the options as `:metadata`.

  ## Returns

    - A `GRPC.Stream` that emits the single gRPC message under demand.

  ## Example

      flow = GRPC.Stream.single_as_ctx(request, stream, max_demand: 10)

  """
  @spec single_as_ctx(any(), GRPC.Server.Stream.t(), Keyword.t()) :: t()
  def single_as_ctx(input, %GRPC.Server.Stream{} = materializer, opts \\ [])
      when is_struct(input) do
    meta = __MODULE__.get_headers(materializer) || %{}
    single(input, Keyword.merge(opts, metadata: meta))
  end

  @doc """
  Wraps an existing `Flow` into a `GRPC.Stream` struct.

  > Note: `:join_with` is not supported in this mode and will raise an error if passed.

  ## Parameters

    - `flow`: A valid `Flow` pipeline.
    - `opts`: Optional keyword options.

  ## Returns

    A `%GRPC.Stream{}` struct wrapping the flow.
  """
  @spec from_flow!(Flow.t(), Keyword.t()) :: t()
  def from_flow!(%Flow{} = flow, opts \\ []) do
    if Keyword.has_key?(opts, :join_with) do
      raise ArgumentError, "join_with option is not supported for Flow input"
    end

    %__MODULE__{flow: flow, options: opts}
  end

  @doc """
  Extracts the internal `Flow` pipeline from a `GRPC.Stream`.

  Raises an `ArgumentError` if the `flow` has not been initialized.

  ## Returns

    The internal `Flow` struct.
  """
  @spec to_flow!(t()) :: Flow.t()
  def to_flow!(%__MODULE__{flow: nil}) do
    raise ArgumentError, "GRPC.Stream has no flow, initialize it with from/2"
  end

  def to_flow!(%__MODULE__{flow: flow}), do: flow

  @doc """
  Executes the flow and emits responses into the provided gRPC server stream.

  ## Parameters

    - `flow`: A `GRPC.Stream` struct containing the flow to be executed.
    - `stream`: A `GRPC.Server.Stream` to which responses are sent.
    - `opts`: Optional keyword list. Currently supported:
    - `:dry_run` — If `true`, responses are not sent (used for testing or inspection).

  ## Returns

    - `:ok` if the stream was processed successfully.
    - For unary mode, returns the first item in the stream instead of sending responses.

  ## Example

      GRPC.Stream.run_with(request, mat)
  """
  @spec run_with(t(), Stream.t(), Keyword.t()) :: :ok | any()
  def run_with(
        %__MODULE__{flow: flow, options: flow_opts} = _stream,
        %Materializer{} = from,
        opts \\ []
      ) do
    dry_run? = Keyword.get(opts, :dry_run, false)
    unary? = Keyword.get(flow_opts, :unary, false)

    if unary? do
      flow
      |> Enum.to_list()
      |> List.flatten()
      |> List.first()
    else
      flow
      |> Flow.map(fn msg ->
        unless dry_run?, do: send_response(from, msg)
        flow
      end)
      |> Flow.run()
    end
  end

  @doc """
  Sends a request to an external process and awaits a response.

  If `target` is a PID, a message in the format `{:request, item, from}` is sent, and a reply
  in the format `{:response, msg}` is expected.

  If `target` is a GenServer module (atom), the message is `{:request, item}`, and a reply
  in the format `{:response, msg}` is expected.

  ## Parameters

    - `stream`: The `GRPC.Stream` pipeline.
    - `target`: Target process PID or GenServer module name.
    - `timeout`: Timeout in milliseconds (defaults to `5000`).

  ## Returns

    - Updated stream if successful.
    - `{:error, item, reason}` if the request fails or times out.
  """
  @spec ask(t(), pid | atom, non_neg_integer) :: t() | {:error, item(), reason()}
  defdelegate ask(stream, target, timeout \\ 5000), to: Operators

  @doc """
  Same as `ask/3`, but raises an exception on failure.

  ## Caution

  This version propagates errors via raised exceptions, which can crash the Flow worker and halt the pipeline.
  Prefer `ask/3` for production use unless failure should abort the stream.
  """
  @spec ask!(t(), pid | atom, non_neg_integer) :: t()
  defdelegate ask!(stream, target, timeout \\ 5000), to: Operators

  @doc """
  Filters the stream using the given predicate function in parallel.
  """
  @spec filter(t(), (term -> term)) :: t
  defdelegate filter(stream, filter), to: Operators

  @doc """
  Applies a mapping function and flattens the results (one level deep).

  Useful for emitting multiple messages for each input.
  """
  @spec flat_map(t, (term -> Enumerable.t())) :: t()
  defdelegate flat_map(stream, flat_mapper), to: Operators

  @doc """
  Applies a transformation function to each stream item.
  """
  @spec map(t(), (term -> term)) :: t()
  defdelegate map(stream, mapper), to: Operators

  defdelegate map_with_ctx(stream, mapper), to: Operators

  @doc """
  Partitions the stream to allow grouping of items by key or condition.

  Use this before stateful operations such as `reduce/3`.

  ## Note

  Excessive use of partitioning can impact performance and memory usage.
  Only partition when required for correctness or performance.
  """
  @spec partition(t(), keyword()) :: t()
  defdelegate partition(stream, options \\ []), to: Operators

  @doc """
  Reduces items in the stream using an accumulator.

  ## Parameters

    - `acc_fun` initializes the accumulator,
    - `reducer_fun` updates it for each input.
  """
  @spec reduce(t, (-> acc), (term, acc -> acc)) :: t when acc: term()
  defdelegate reduce(stream, acc_fun, reducer_fun), to: Operators

  @doc """
  Applies the given function rejecting each input in parallel.
  """
  @spec reject(t, (term -> term)) :: t
  defdelegate reject(stream, filter), to: Operators

  @doc """
  Emits only distinct items from the stream.
  """
  @spec uniq(t) :: t
  defdelegate uniq(stream), to: Operators

  @doc """
  Emits only unique items as determined by the result of the given function.
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
    dispatcher = Keyword.get(opts, :default_dispatcher, GenStage.DemandDispatcher)

    flow =
      case Keyword.get(opts, :join_with) do
        pid when is_pid(pid) ->
          opts = Keyword.drop(opts, [:join_with, :default_dispatcher])

          input_flow = Flow.from_enumerable(input, opts)
          other_flow = Flow.from_stages([pid], opts)
          Flow.merge([input_flow, other_flow], dispatcher, opts)

        # handle Elixir.Stream joining
        other when is_list(other) or is_function(other) ->
          Flow.from_enumerables([input, other], opts)

        _ ->
          opts = Keyword.drop(opts, [:join_with, :default_dispatcher])
          Flow.from_enumerable(input, opts)
      end

    %__MODULE__{flow: flow, options: opts}
  end

  defp send_response(from, msg) do
    GRPC.Server.send_reply(from, msg)
  end
end
