defmodule GRPC.Telemetry do
  @moduledoc """
  Events published by GRPC

  These can be divided in client-side events and server-side events.

  ## Client-side Events

    * `[:grpc, :client, :rpc, :start]` - Published before all interceptors are executed.
    * `[:grpc, :client, :rpc, :stop]` - Published after all interceptors executed successfully.
      * `:duration` - the duration as measured through `System.monotonic_time()`
        for the whole interceptor pipeline.
    * `[:grpc, :client, :rpc, :exception]` - Published if any exception occurs while receiving a message.
      * `:duration` - the duration as measured through `System.monotonic_time()`
        for the execution since the start of the pipeline until the exception happened.

  | event        | measurements | metadata |
  |--------------|--------------|----------|
  | `[:rpc, :start]`  | `:count`     | `:stream` |
  | `[:rpc, :stop]`  | `:duration`  | `:stream` |
  | `[:rpc, :exception]` | `:duration`  | `:stream`, `:kind`, `:reason`, `:stacktrace` |

  ### Metadata

    * `:stream` - the `%GRPC.Server.Stream{}` for the request
    * `:function_name` - the name of the function called
    * `:server` - the server module name
    * `:endpoint` - the endpoint module name
    * `:request` - the client request
    * `:result` - the result returned from the interceptor pipeline.

  `:exception` events also include some error metadata:

    * `:reason` is the error value in case of `catch` or the actual exception in case of `rescue`.
    * `:kind` can be one of:
      * `:error` — from an `{:error, error}` return value. Some Erlang functions may also throw an
        `:error` tuple, which will be reported as `:error`.
      * `:exit` — from a caught process exit.
      * `:throw` — from a caught value, this doesn't necessarily mean that an error occurred.

  ## Server-side Events

    * `[:grpc, :server, :rpc, :start]` - Published before all interceptors are executed.
    * `[:grpc, :server, :rpc, :stop]` - Published after all interceptors executed successfully.
      * `:duration` - the duration as measured through `System.monotonic_time()`
        for the whole interceptor pipeline.
    * `[:grpc, :server, :rpc, :exception]` - Published if any exception occurs while receiving a message.
      * `:duration` - the duration as measured through `System.monotonic_time()`
        for the execution since the start of the pipeline until the exception happened.
    * `[:grpc, :server, :rpc, :send_error]` - Published by the Cowboy adapter when the
      handler returns but the adapter cannot encode, compress or send the reply. See
      "Failures while sending the reply" below.
      * `:duration` - the time from the start of the pipeline to the failure, as
        measured through `System.monotonic_time()`.
      * `:monotonic_time` - the time of the failure.
    * `[:grpc, :server, :rpc, :abort]` - Published when the adapter stops an in-flight
      RPC: expired deadline, client cancellation or a dropped connection. The exit
      signal doesn't unwind the RPC process, so `:stop` and `:exception` are not
      published for such a call; this event comes from the adapter's process.
      * `:duration` - the duration as measured through `System.monotonic_time()`
        from the arrival of the request until the abort.

  | event        | measurements | metadata |
  |--------------|--------------|----------|
  | `[:rpc, :start]`  | `:count`     | `:stream`, `:server`, `:endpoint`, `:function_name` |
  | `[:rpc, :stop]`  | `:duration`  | `:stream`, `:server`, `:endpoint`, `:function_name` , `:result` |
  | `[:rpc, :exception]` | `:duration`  | `:stream`, `:server`, `:endpoint`, `:function_name`, `:kind`, `:reason`, `:stacktrace` |
  | `[:rpc, :send_error]` | `:duration`, `:monotonic_time` | `:stream`, `:server`, `:endpoint`, `:path`, `:kind`, `:reason`, `:stacktrace` |
  | `[:rpc, :abort]` | `:duration`  | `:stream`, `:server`, `:endpoint`, `:path`, `:pid`, `:reason` |

  ### Metadata

    * `:stream` - the `%GRPC.Server.Stream{}` for the request.
    * `:function_name` - the name of the function called.
    * `:server` - the server module name.
    * `:endpoint` - the endpoint module name.
    * `:result` - the result returned from the interceptor pipeline.

  `:abort` events also include `:path` (the request path), `:pid` (the RPC
  process being stopped) and `:reason` (the exit reason, e.g. `:timeout`).
  They carry no `:function_name`, which is resolved in the RPC process; for
  the same reason `:stream` holds only what the adapter built on arrival, so
  read `:server` and `:endpoint` from the metadata rather than from it.

  `:exception` events also include some error metadata:

    * `:reason` is the error value in case of `catch` or the actual exception in case of `rescue`.
    * `:kind` can be one of:
      * `:error` — from an `{:error, error}` return value. Some Erlang functions may also throw an
        `:error` tuple, which will be reported as `:error`.
      * `:exit` — from a caught process exit.
      * `:throw` — from a caught value, this doesn't necessarily mean that an error occurred.

  ### Failures while sending the reply

  The handler can return, and the server can still fail to send the reply. The
  Cowboy adapter publishes `[:grpc, :server, :rpc, :send_error]` in these conditions:

    * The codec cannot encode the reply. `:reason` is the exception from the codec.
    * A compressor, or the codec's `pack_for_channel/1`, raises an exception.
      `:reason` is that exception. The client gets `INTERNAL`. Before, the client
      got no `grpc-status`.
    * The adapter does not send the reply. Examples are a compressor that the client
      does not accept, and a reply that is larger than the message limit. `:reason`
      is the `GRPC.RPCError` that the client gets. `:stacktrace` is the stack of the
      adapter process at the point where it found the failure.

  This event is not a second `:exception`. The `:telemetry.span/3` around the
  handler is closed at this time. For a call that returns, `:stop` comes first. Thus
  the event has no `:function_name` and no `:telemetry_span_context`. A tracer that
  pairs span events by that context does not see it. The metadata has `:path`
  instead.

  ### A client that disconnects

  The client can close the socket, close the connection or cancel the stream while
  the server sends the reply. In these conditions, nothing raises on the server.
  The adapter stops the RPC process and publishes `[:grpc, :server, :rpc, :abort]`.
  The event has the exit reason (`:shutdown`), but no `:kind` and no `:stacktrace`.
  The adapter does not publish `:stop`, `:exception` or `:send_error`, and it logs
  nothing. To write a reply, the server sends a message to the Cowboy process. Thus
  the send call does not fail.

  ### Send server errors to an error reporter

  An error reporter that reads `:exception` and `:send_error` events gets all the
  failures above. An error reporter that reads Logger reports does not need these
  events. The Cowboy adapter logs these failures with `:crash_reason` metadata: a
  handler failure, an encoding failure and a compressor that raises.
  `exception_log_filter` can remove these logs. The adapter does not log a reply
  that it does not send.
  """

  require Logger

  @server_rpc [:grpc, :server, :rpc]
  @client_rpc [:grpc, :client, :rpc]

  @doc "The server telemetry event prefix."
  def server_rpc_prefix, do: @server_rpc

  @doc "The client telemetry event prefix."
  def client_rpc_prefix, do: @client_rpc

  @doc false
  def server_span(server, endpoint, func_name, stream, span_fn) do
    start_metadata = %{
      server: server,
      endpoint: endpoint,
      function_name: func_name,
      stream: stream
    }

    :telemetry.span(@server_rpc, start_metadata, fn ->
      result = span_fn.()

      {result, Map.put(start_metadata, :result, result)}
    end)
  rescue
    e in GRPC.RPCError ->
      {:error, e}
  catch
    kind, reason ->
      stacktrace = __STACKTRACE__
      Logger.error(Exception.format(kind, reason, stacktrace))
      {:error, %{kind: kind, reason: reason, stack: stacktrace}}
  end

  @client_rpc_start_name @client_rpc ++ [:start]
  @doc false
  def client_rpc_start_name, do: @client_rpc_start_name

  @doc false
  def client_span(stream, request, span_fn) do
    start_metadata = %{stream: stream, request: request}

    :telemetry.span(@client_rpc, start_metadata, fn ->
      try do
        result = span_fn.()

        {result, Map.put(start_metadata, :result, result)}
      rescue
        e ->
          :erlang.error(Exception.normalize(:error, e, __STACKTRACE__))
      end
    end)
  catch
    kind, reason ->
      stacktrace = __STACKTRACE__
      Logger.error(Exception.format(kind, reason, stacktrace))
      :erlang.raise(kind, reason, stacktrace)
  end

  @client_rpc_stop_name @client_rpc ++ [:stop]
  @doc false
  def client_rpc_stop_name, do: @client_rpc_stop_name

  @doc false
  def client_rpc_stop(stream, duration) do
    :telemetry.execute(@client_rpc_stop_name, %{duration: duration}, %{stream: stream})
  end

  @client_rpc_exception_name @client_rpc ++ [:exception]
  @doc false
  def client_rpc_exception_name, do: @client_rpc_exception_name

  @doc false
  def client_rpc_exception(
        stream,
        kind,
        reason,
        stacktrace,
        duration
      ) do
    :telemetry.execute(@client_rpc_exception_name, %{duration: duration}, %{
      stream: stream,
      kind: kind,
      reason: reason,
      stacktrace: stacktrace
    })
  end
end
