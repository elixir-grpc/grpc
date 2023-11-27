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

  | event        | measurements | metadata |
  |--------------|--------------|----------|
  | `[:rpc, :start]`  | `:count`     | `:stream`, `:server`, `:endpoint`, `:function_name` |
  | `[:rpc, :stop]`  | `:duration`  | `:stream`, `:server`, `:endpoint`, `:function_name` , `:result` |
  | `[:rpc, :exception]` | `:duration`  | `:stream`, `:server`, `:endpoint`, `:function_name`, `:kind`, `:reason`, `:stacktrace` |

  ### Metadata

    * `:stream` - the `%GRPC.Server.Stream{}` for the request.
    * `:function_name` - the name of the function called.
    * `:server` - the server module name.
    * `:endpoint` - the endpoint module name.
    * `:result` - the result returned from the interceptor pipeline.

  `:exception` events also include some error metadata:

    * `:reason` is the error value in case of `catch` or the actual exception in case of `rescue`.
    * `:kind` can be one of:
      * `:error` — from an `{:error, error}` return value. Some Erlang functions may also throw an
        `:error` tuple, which will be reported as `:error`.
      * `:exit` — from a caught process exit.
      * `:throw` — from a caught value, this doesn't necessarily mean that an error occurred.
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
