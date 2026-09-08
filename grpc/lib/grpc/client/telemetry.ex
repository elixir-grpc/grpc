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
  | `:reset` | `streams_terminated` | common metadata plus diagnostic `reason` |
  | `:goaway` | `streams_terminated` | common metadata plus `error_code` and diagnostic `reason` |
  | `:settings` | none | common metadata plus `max_concurrent_streams` |
  | `:streams` | `active` | common metadata |
  | `:stream_rejected` | `count: 1` | common metadata plus `reason: :max_concurrent_streams` |

  Generations start at one on the first successful establishment and increment
  after recovery. Stream occupancy is an authoritative snapshot after each
  stream-table mutation. When both `:reset` and `:down` describe one loss,
  terminated streams are counted by `:down`; `:reset` reports zero to prevent
  double-counting.

  `:connect_error` is emitted only when the adapter can assign `:resolve`,
  `:tcp`, `:tls`, or `:http2` from a public transport signal. `:goaway` is
  emitted only when the adapter supplies a distinct GOAWAY signal.

  Gun 2.4 exposes connection up/down and settings changes through public owner
  messages, but does not expose a distinct connection-owner GOAWAY signal.
  Mint 1.9 exposes connection results, read availability, and settings
  snapshots. It does not expose GOAWAY details or whether the peer explicitly
  sent `SETTINGS_MAX_CONCURRENT_STREAMS`; its ambiguous default value is
  reported as `:unknown`. Consequently neither built-in adapter currently
  emits `:goaway`, and Mint cannot report an explicit peer limit of 100 without
  a small upstream Mint hook exposing setting presence.

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

    :telemetry.span(@rpc_prefix, start_metadata, fn ->
      result = span_fn.()
      failure = Process.delete(@failure_key) || classify_remote(result)
      metadata = Map.put(start_metadata, :result, result)
      {result, put_failure(metadata, failure)}
    end)
  catch
    kind, reason ->
      Process.delete(@failure_key)
      stacktrace = __STACKTRACE__
      Logger.error(Exception.format(kind, reason, stacktrace))
      :erlang.raise(kind, reason, stacktrace)
  end

  defp classify_remote({:error, %GRPC.RPCError{}}), do: {:remote, nil}
  defp classify_remote(_result), do: nil

  defp put_failure(metadata, {stage, reason}) do
    metadata
    |> Map.put(:failure_stage, stage)
    |> Map.put(:failure_reason, reason)
  end

  defp put_failure(metadata, nil), do: metadata
end
