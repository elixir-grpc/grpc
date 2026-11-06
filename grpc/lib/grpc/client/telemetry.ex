defmodule GRPC.Client.Telemetry do
  @moduledoc """
  Transport-neutral telemetry emitted by gRPC clients.

  Transport events share the prefix `[:grpc, :client, :transport]`. References
  in metadata are opaque correlation values; they should not be used as metric
  labels.

  | Event | Measurements | Metadata |
  | --- | --- | --- |
  | `:started` | `generation: 0`, `active_streams: 0` | `logical_connection_ref`, `transport_ref`, `target`, `adapter` |
  | `:connected` | `generation` | common metadata plus `reconnect` |
  | `:connect_error` | none | common metadata plus bounded `stage` and diagnostic `reason` |
  | `:down` | `streams_terminated` | common metadata plus diagnostic `reason` |
  | `:stopped` | `streams_terminated` | common metadata plus diagnostic `reason` |
  | `:settings` | none | common metadata plus `max_concurrent_streams` |
  | `:streams` | `active` | common metadata |

  Generations start at one on the first successful establishment and increment
  after recovery. Stream occupancy is an authoritative snapshot after each
  stream-table mutation.

  `:connect_error` is emitted only when the adapter can assign `:resolve`,
  `:tcp`, `:tls`, or `:http2` from a public transport signal.

  Mint 1.9 does not expose whether the peer explicitly sent
  `SETTINGS_MAX_CONCURRENT_STREAMS`; its ambiguous default value is reported as
  `:unknown`. A small upstream Mint hook exposing setting presence would allow
  an explicit peer limit of 100 to be reported safely.

  Existing `[:grpc, :client, :rpc, ...]` events are unchanged. Stop and
  exception metadata may additionally contain `:failure_stage` and
  `:failure_reason` when the library knows them without parsing error text.
  """

  require Logger

  @transport_prefix [:grpc, :client, :transport]
  @rpc_prefix [:grpc, :client, :rpc]
  @failure_key {__MODULE__, :rpc_failure}

  @doc false
  def execute(event, measurements, metadata) do
    :telemetry.execute(@transport_prefix ++ [event], measurements, metadata)
  end

  @doc false
  def mark_rpc_failure(stage, reason) do
    Process.put(@failure_key, {stage, reason})
    :ok
  end

  @doc false
  def client_span(stream, request, span_fn) do
    Process.delete(@failure_key)
    start_metadata = %{stream: stream, request: request}
    start_time = System.monotonic_time()
    span_context = make_ref()

    :telemetry.execute(
      @rpc_prefix ++ [:start],
      %{monotonic_time: start_time, system_time: System.system_time()},
      Map.put(start_metadata, :telemetry_span_context, span_context)
    )

    try do
      result =
        try do
          span_fn.()
        rescue
          e -> :erlang.error(Exception.normalize(:error, e, __STACKTRACE__))
        end

      stop_time = System.monotonic_time()
      failure = Process.delete(@failure_key)

      :telemetry.execute(
        @rpc_prefix ++ [:stop],
        %{duration: stop_time - start_time, monotonic_time: stop_time},
        start_metadata
        |> Map.put(:result, result)
        |> put_failure(failure)
        |> Map.put(:telemetry_span_context, span_context)
      )

      result
    catch
      kind, reason ->
        stop_time = System.monotonic_time()
        stacktrace = __STACKTRACE__

        metadata =
          start_metadata
          |> Map.merge(%{kind: kind, reason: reason, stacktrace: stacktrace})
          |> put_failure(Process.delete(@failure_key))
          |> Map.put(:telemetry_span_context, span_context)

        :telemetry.execute(
          @rpc_prefix ++ [:exception],
          %{duration: stop_time - start_time, monotonic_time: stop_time},
          metadata
        )

        Logger.error(Exception.format(kind, reason, stacktrace))
        :erlang.raise(kind, reason, stacktrace)
    end
  end

  defp put_failure(metadata, {stage, reason}) do
    metadata
    |> Map.put(:failure_stage, stage)
    |> Map.put(:failure_reason, reason)
  end

  defp put_failure(metadata, nil), do: metadata
end
