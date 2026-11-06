defmodule GRPC.Client.Adapter do
  @moduledoc """
  HTTP client adapter for GRPC.
  """
  alias GRPC.Client.Stream

  @callback connect(channel :: struct(), opts :: keyword()) ::
              {:ok, struct()} | {:error, any()}

  @callback validate_opts(opts :: keyword()) :: :ok | {:error, String.t()}

  @callback disconnect(channel :: struct()) :: {:ok, struct()} | {:error, any()}

  @callback send_request(stream :: Stream.t(), contents :: iodata(), opts :: keyword()) ::
              Stream.t()

  @doc """
  Check `GRPC.Stub.recv/2` for more context about the return types
  """
  @callback receive_data(stream :: Stream.t(), opts :: keyword()) ::
              GRPC.Stub.receive_data_return() | {:error, any()}

  @doc """
  This callback is used to open a stream connection to the server.
  Mostly used when the payload for this request is streamed.
  To send data using the open stream request, you should use `send_data/3`
  """
  @callback send_headers(stream :: Stream.t(), opts :: keyword()) :: Stream.t()

  @doc """
  This callback will be responsible to send data to the server on a stream
  request is open using `send_headers/2`
   Opts:
      - :send_end_stream (optional) - ends the request stream
  """
  @callback send_data(stream :: Stream.t(), message :: iodata(), opts :: keyword()) :: Stream.t()

  @doc """
  Similarly to the option sent on `send_data/2` - :send_end_stream -
  this callback will end request stream
  """
  @callback end_stream(stream :: Stream.t()) :: Stream.t()

  @doc """
  Cancel a stream in a streaming client.
  """
  @callback cancel(stream :: Stream.t()) :: :ok | {:error, any()}

  @optional_callbacks validate_opts: 1

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
