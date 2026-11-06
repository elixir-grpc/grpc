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
      handler returned but encoding, compressing or sending the reply failed. See
      "Failures while sending the reply" below.
      * `:duration` - the duration as measured through `System.monotonic_time()`
        since the start of the pipeline until the failure.
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

  The handler can return and the server can still fail to answer. The Cowboy
  adapter publishes `[:grpc, :server, :rpc, :send_error]` when:

    * the codec rejects the reply: `:reason` is the exception the codec raised;
    * a compressor (or the codec's `pack_for_channel/1`) raises: `:reason` is the
      exception it raised, and the client gets `INTERNAL` where it used to get no
      `grpc-status`;
    * the adapter refuses to send the reply, for example a compressor the client did
      not accept, or a reply over the message limit: `:reason` is the `GRPC.RPCError`
      the client received, and `:stacktrace` is the adapter process's stack where it
      noticed the failure.

  This is a separate event, not a second `:exception`. The `:telemetry.span/3`
  around the handler has closed by then (`:stop` came first for a call that
  returned), so the event has no `:function_name` or `:telemetry_span_context`, and
  a tracer that pairs span events by that context never sees it. Its metadata has
  `:path` instead.

  ### A client that goes away

  When the client closes the socket, closes the connection or cancels the stream
  while the server is still replying, nothing raises on the server. The adapter
  stops the RPC process and publishes `[:grpc, :server, :rpc, :abort]`, with the
  exit reason (`:shutdown`) and no `:kind` or `:stacktrace`. `:stop`, `:exception`
  and `:send_error` are not published, and nothing is logged. Writing a reply is
  an asynchronous message to the Cowboy process, so the sending call never fails.

  ### Forwarding server errors to an error reporter

  An error reporter that reads `:exception` and `:send_error` events gets every
  failure above. One that reads Logger reports needs no event: the Cowboy adapter
  logs a handler failure, an encoding failure or a raising compressor with
  `:crash_reason` metadata, unless `exception_log_filter` drops it. A reply the
  adapter refuses to send is not logged.
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
