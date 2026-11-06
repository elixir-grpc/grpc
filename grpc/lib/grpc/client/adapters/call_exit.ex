defmodule GRPC.Client.Adapters.CallExit do
  @moduledoc false
  # Calls an adapter's connection process without leaking the request in the exit.
  #
  # When `GenServer.call/3` exits (the connection process is busy past the call
  # timeout, or dead), the exit reason embeds the call message: for request calls
  # that is the request headers, including credentials such as `authorization`,
  # and the encoded body. That reason is logged by `GRPC.Telemetry.client_span/3`,
  # carried in `[:grpc, :client, :rpc, :exception]` metadata, and ends up in crash
  # reports, so headers and bodies are replaced with `:redacted` before the exit
  # propagates. The shape `{reason, {GenServer, :call, [server, message, timeout]}}`
  # and the path are kept, so callers matching on it are unaffected.

  @doc false
  @spec call(GenServer.server(), term(), timeout()) :: term()
  def call(server, message, timeout \\ 5_000) do
    GenServer.call(server, message, timeout)
  catch
    :exit, reason -> :erlang.raise(:exit, redact(reason), __STACKTRACE__)
  end

  @doc false
  @spec redact(term()) :: term()
  def redact({reason, {GenServer, :call, [server, message | rest]}}),
    do: {reason, {GenServer, :call, [server, redact_message(message) | rest]}}

  def redact(reason), do: reason

  # Gun: GRPC.Client.Adapters.Gun.ConnectionProcess
  defp redact_message({:request, path, _headers, _body, deadline}),
    do: {:request, path, :redacted, :redacted, deadline}

  defp redact_message({:open_stream, path, _headers, deadline}),
    do: {:open_stream, path, :redacted, deadline}

  defp redact_message({:send_data, stream_ref, fin, _data}),
    do: {:send_data, stream_ref, fin, :redacted}

  # Mint: GRPC.Client.Adapters.Mint.ConnectionProcess
  defp redact_message({:request, method, path, _headers, _body, opts}),
    do: {:request, method, path, :redacted, :redacted, opts}

  defp redact_message({:stream_body, request_ref, _body}),
    do: {:stream_body, request_ref, :redacted}

  defp redact_message(message), do: message
end
