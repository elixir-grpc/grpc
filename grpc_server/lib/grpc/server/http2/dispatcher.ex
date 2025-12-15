defmodule GRPC.Server.HTTP2.Dispatcher do
  @moduledoc """
  Dispatches gRPC calls to registered services.

  This module:
  - Parses gRPC path ("/package.Service/Method")
  - Looks up service implementation from servers registry
  - Decodes protobuf request messages
  - Calls service handler functions
  - Encodes protobuf response messages
  - Handles streaming (client/server/bidirectional)
  """

  require Logger

  alias GRPC.Server.Cache
  alias GRPC.Server.HTTP2.StreamState
  alias GRPC.RPCError

  # Inline hot path functions
  @compile {:inline,
            parse_path: 1,
            lookup_server: 2,
            lookup_rpc: 2,
            get_codec: 2,
            get_compressor: 2,
            encode_response: 4,
            decode_messages: 4}

  @doc """
  Dispatches a gRPC call to the appropriate service.

  ## Parameters
  - `stream_state`: HTTP/2 stream with decoded headers
  - `servers`: Map of service name => server module
  - `endpoint`: The endpoint module (for telemetry, etc)

  ## Returns
  - `{:ok, response_headers, response_data, trailers}` - Success
  - `{:error, rpc_error}` - gRPC error to send to client
  """
  @spec dispatch(StreamState.t(), map(), atom(), GRPC.Server.HTTP2.Connection.t()) ::
          {:ok, list(), binary(), map()} | {:error, GRPC.RPCError.t()}
  def dispatch(%StreamState{} = stream_state, servers, endpoint, connection) do
    Logger.debug(
      "[dispatch] path=#{stream_state.path}, messages=#{length(stream_state.message_buffer)}"
    )

    # Check deadline BEFORE processing - if exceeded, return error immediately
    if StreamState.deadline_exceeded?(stream_state) do
      now = System.monotonic_time(:microsecond)

      Logger.debug(
        "[dispatch] Deadline exceeded for path=#{stream_state.path}, deadline=#{stream_state.deadline}, now=#{now}, diff=#{now - (stream_state.deadline || now)}us"
      )

      {:error, GRPC.RPCError.exception(status: :deadline_exceeded, message: "Deadline exceeded")}
    else
      with {:ok, service_name, method_name} <- parse_path(stream_state.path),
           {:ok, server} <- lookup_server(servers, service_name),
           {:ok, rpc} <- lookup_rpc(server, method_name),
           {:ok, codec} <- get_codec(server, stream_state.content_type),
           {:ok, compressor} <- get_compressor(server, stream_state.metadata),
           {:ok, requests} <-
             decode_messages(stream_state.message_buffer, rpc, codec, compressor),
           {:ok, response} <-
             call_service(
               server,
               rpc,
               method_name,
               requests,
               stream_state,
               endpoint,
               codec,
               compressor,
               connection
             ) do
        Logger.info("[dispatch] Encoding response")
        # Get custom headers/trailers from process dictionary
        # (set by handler during execution via set_headers/set_trailers)
        custom_headers = Process.get({:grpc_custom_headers, stream_state.stream_id}, %{})
        custom_trailers = Process.get({:grpc_custom_trailers, stream_state.stream_id}, %{})

        # Update stream_state with custom headers/trailers
        updated_stream_state = %{
          stream_state
          | custom_headers: custom_headers,
            custom_trailers: custom_trailers
        }

        # Cleanup process dictionary
        Process.delete({:grpc_custom_headers, stream_state.stream_id})
        Process.delete({:grpc_custom_trailers, stream_state.stream_id})

        # Encode response(s) - could be single response or list for streaming
        # Pass updated stream_state to include custom headers/trailers
        encode_responses(response, codec, compressor, rpc, updated_stream_state)
      else
        {:error, %GRPC.RPCError{} = error} ->
          error

        {:error, _reason} = err ->
          Logger.error("Dispatch error: #{inspect(err)}")
          {:error, GRPC.RPCError.exception(status: :internal, message: "Internal server error")}
      end
    end
  end

  @doc """
  Parses gRPC path into service and method names.

  Examples:
  - "/helloworld.Greeter/SayHello" → {"helloworld.Greeter", "SayHello"}
  - "/package.subpackage.Service/Method" → {"package.subpackage.Service", "Method"}
  """
  @spec parse_path(String.t()) :: {:ok, String.t(), String.t()} | {:error, RPCError.t()}
  def parse_path("/" <> rest) do
    case String.split(rest, "/", parts: 2) do
      [service_name, method_name] when service_name != "" and method_name != "" ->
        {:ok, service_name, method_name}

      _ ->
        {:error, RPCError.exception(status: :unimplemented, message: "Invalid path format")}
    end
  end

  def parse_path(_) do
    {:error, RPCError.exception(status: :unimplemented, message: "Path must start with /")}
  end

  @doc """
  Checks if a gRPC path corresponds to a bidirectional streaming RPC.

  Returns `true` if both request and response are streaming, `false` otherwise.
  """
  @spec is_bidi_streaming?(String.t(), map()) :: boolean()
  def is_bidi_streaming?(path, servers) do
    with {:ok, service_name, method_name} <- parse_path(path),
         {:ok, server} <- lookup_server(servers, service_name),
         {:ok, rpc} <- lookup_rpc(server, method_name) do
      {_name, {_req_mod, req_stream?}, {_res_mod, res_stream?}, _opts} = rpc
      req_stream? and res_stream?
    else
      _ -> false
    end
  end

  ## Private Functions
  defp lookup_server(servers, service_name) do
    case Map.get(servers, service_name) do
      nil ->
        {:error,
         RPCError.exception(status: :unimplemented, message: "Service not found: #{service_name}")}

      server ->
        {:ok, server}
    end
  end

  defp lookup_rpc(server, method_name) do
    # Use cache to find RPC definition
    case GRPC.Server.Cache.find_rpc(server, method_name) do
      nil ->
        {:error,
         RPCError.exception(
           status: :unimplemented,
           message: "Method not found: #{method_name}"
         )}

      rpc ->
        {:ok, rpc}
    end
  end

  defp get_codec(server, content_type) do
    # Extract codec subtype from content-type
    # "application/grpc+proto" → "proto"
    # "application/grpc+json" → "json"
    subtype =
      case String.split(content_type, "+", parts: 2) do
        ["application/grpc", subtype] -> subtype
        ["application/grpc"] -> "proto"
        _ -> "proto"
      end

    case Cache.find_codec(server, subtype) do
      nil ->
        {:error,
         RPCError.exception(status: :unimplemented, message: "Codec not found: #{subtype}")}

      codec ->
        {:ok, codec}
    end
  end

  defp get_compressor(server, metadata) do
    # Check grpc-encoding header
    encoding = Map.get(metadata, "grpc-encoding", "identity")

    if encoding == "identity" do
      {:ok, nil}
    else
      case Cache.find_compressor(server, encoding) do
        nil ->
          {:error,
           RPCError.exception(
             status: :unimplemented,
             message: "Compressor not found: #{encoding}"
           )}

        compressor ->
          {:ok, compressor}
      end
    end
  end

  defp decode_messages(message_buffer, rpc, codec, compressor) do
    # Extract request type from RPC definition
    # RPC format: {name, {request_module, is_stream?}, {reply_module, is_stream?}, options}
    {_name, {request_module, _is_stream?}, _reply, _options} = rpc

    try do
      messages =
        Enum.map(message_buffer, fn %{compressed: compressed?, data: data} ->
          # Decompress if needed
          data =
            if compressed? and compressor do
              compressor.decompress(data)
            else
              data
            end

          # Decode protobuf with the request module
          codec.decode(data, request_module)
        end)

      {:ok, messages}
    rescue
      e ->
        Logger.error("Failed to decode messages: #{inspect(e)}")

        {:error,
         RPCError.exception(status: :invalid_argument, message: "Invalid request message")}
    end
  end

  defp call_unary(server, func_name, request, stream) do
    Logger.info("[call_unary] Calling #{inspect(server)}.#{func_name}")

    # Check if function is implemented
    if function_exported?(server, func_name, 2) do
      # Accumulate base headers (don't send yet - handler may add custom headers)
      base_headers = %{
        ":status" => "200",
        "content-type" => "application/grpc+proto"
      }

      # Add grpc-encoding if there's a compressor
      base_headers =
        if stream.compressor do
          Map.put(base_headers, "grpc-encoding", stream.compressor.name())
        else
          base_headers
        end

      GRPC.Server.set_headers(stream, base_headers)

      try do
        # Call handler and get response
        response = apply(server, func_name, [request, stream])
        Logger.info("[call_unary] Response received, sending via send_reply")

        # Send response using async message (this will send accumulated headers first)
        # :noreply means GRPC.Stream.run() already sent the response
        if response != :noreply do
          GRPC.Server.send_reply(stream, response)
        end

        # Get custom trailers from process dictionary (set by handler via set_trailers)
        stream_id = stream.payload.stream_id
        custom_trailers = Process.get({:grpc_custom_trailers, stream_id}, %{})

        # Merge with mandatory grpc-status
        trailers = Map.merge(%{"grpc-status" => "0"}, custom_trailers)

        # Send trailers at the end with END_STREAM
        GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, trailers)

        # Return special marker for async handling (like server_streaming)
        {:ok, :streaming_done}
      rescue
        e in GRPC.RPCError ->
          # Send error as trailers (headers already accumulated, will be sent with trailers)
          stream_id = stream.payload.stream_id

          error_trailers = %{
            "grpc-status" => "#{e.status}",
            "grpc-message" => e.message || ""
          }

          # Get custom trailers from process dictionary
          custom_trailers = Process.get({:grpc_custom_trailers, stream_id}, %{})
          trailers = Map.merge(error_trailers, custom_trailers)

          # Send trailers with error (will send accumulated headers first if not sent yet)
          GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, trailers)

          # Return streaming_done (error already sent)
          {:ok, :streaming_done}

        e ->
          Logger.error("Handler error: #{Exception.message(e)}")

          error_trailers = %{
            # UNKNOWN
            "grpc-status" => "2",
            "grpc-message" => Exception.message(e)
          }

          # Send trailers with error
          GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, error_trailers)

          # Return streaming_done (error already sent)
          {:ok, :streaming_done}
      end
    else
      # Function not implemented
      Logger.error("Function #{inspect(server)}.#{func_name}/2 is not implemented")

      # Send required HTTP/2 headers first
      headers = %{
        ":status" => "200",
        "content-type" => "application/grpc+proto"
      }

      GRPC.Server.Adapters.ThousandIsland.send_headers(stream.payload, headers)

      # Then send error trailers
      error_trailers = %{
        # UNIMPLEMENTED
        "grpc-status" => "12",
        "grpc-message" => "Method not implemented"
      }

      GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, error_trailers)
      {:ok, :streaming_done}
    end
  end

  defp call_client_streaming(server, func_name, requests, stream) do
    # Check if function is implemented
    if function_exported?(server, func_name, 2) do
      try do
        # Accumulate base headers (don't send yet - handler may add custom headers)
        base_headers = %{
          ":status" => "200",
          "content-type" => "application/grpc+proto"
        }

        # Add grpc-encoding if there's a compressor
        base_headers =
          if stream.compressor do
            Map.put(base_headers, "grpc-encoding", stream.compressor.name())
          else
            base_headers
          end

        # Accumulate base headers without sending
        GRPC.Server.set_headers(stream, base_headers)

        # Convert list to stream
        request_enum = Enum.into(requests, [])
        response = apply(server, func_name, [request_enum, stream])

        # Send response using async message (this will send accumulated headers first)
        GRPC.Server.send_reply(stream, response)

        # Get custom trailers from process dictionary (set by handler via set_trailers)
        stream_id = stream.payload.stream_id
        custom_trailers = Process.get({:grpc_custom_trailers, stream_id}, %{})

        # Merge with mandatory grpc-status
        trailers = Map.merge(%{"grpc-status" => "0"}, custom_trailers)

        # Send trailers at the end with END_STREAM
        GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, trailers)

        # Return special marker for async handling (like server_streaming)
        {:ok, :streaming_done}
      rescue
        e in GRPC.RPCError ->
          # Send error as trailers
          stream_id = stream.payload.stream_id

          error_trailers = %{
            "grpc-status" => "#{e.status}",
            "grpc-message" => e.message || ""
          }

          custom_trailers = Process.get({:grpc_custom_trailers, stream_id}, %{})
          trailers = Map.merge(error_trailers, custom_trailers)
          GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, trailers)
          {:ok, :streaming_done}

        e ->
          Logger.error("Handler error: #{Exception.message(e)}")

          error_trailers = %{
            # UNKNOWN
            "grpc-status" => "2",
            "grpc-message" => Exception.message(e)
          }

          GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, error_trailers)
          {:ok, :streaming_done}
      end
    else
      # Function not implemented
      Logger.error("Function #{inspect(server)}.#{func_name}/2 is not implemented")

      # Send required HTTP/2 headers first
      headers = %{
        ":status" => "200",
        "content-type" => "application/grpc+proto"
      }

      GRPC.Server.Adapters.ThousandIsland.send_headers(stream.payload, headers)

      # Then send error trailers
      error_trailers = %{
        # UNIMPLEMENTED
        "grpc-status" => "12",
        "grpc-message" => "Method not implemented"
      }

      GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, error_trailers)
      {:ok, :streaming_done}
    end
  end

  defp call_server_streaming(server, func_name, request, stream) do
    # Check if function is implemented
    if function_exported?(server, func_name, 2) do
      try do
        # Accumulate base headers (don't send yet - handler may add custom headers)
        base_headers = %{
          ":status" => "200",
          "content-type" => "application/grpc+proto"
        }

        # Add grpc-encoding if there's a compressor
        base_headers =
          if stream.compressor do
            Map.put(base_headers, "grpc-encoding", stream.compressor.name())
          else
            base_headers
          end

        GRPC.Server.set_headers(stream, base_headers)

        # Handler calls GRPC.Server.send_reply for each response
        apply(server, func_name, [request, stream])

        # Get custom trailers from process dictionary (set by handler via set_trailers)
        stream_id = stream.payload.stream_id
        custom_trailers = Process.get({:grpc_custom_trailers, stream_id}, %{})

        # Merge with mandatory grpc-status
        trailers = Map.merge(%{"grpc-status" => "0"}, custom_trailers)

        # Send trailers at the end with END_STREAM
        GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, trailers)

        # Return special marker for streaming (not data to send)
        {:ok, :streaming_done}
      rescue
        e in GRPC.RPCError ->
          # Send error as trailers
          stream_id = stream.payload.stream_id

          error_trailers = %{
            "grpc-status" => "#{e.status}",
            "grpc-message" => e.message || ""
          }

          custom_trailers = Process.get({:grpc_custom_trailers, stream_id}, %{})
          trailers = Map.merge(error_trailers, custom_trailers)
          GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, trailers)
          {:ok, :streaming_done}

        e ->
          Logger.error("Handler error: #{Exception.message(e)}")

          error_trailers = %{
            # UNKNOWN
            "grpc-status" => "2",
            "grpc-message" => Exception.message(e)
          }

          GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, error_trailers)
          {:ok, :streaming_done}
      end
    else
      # Function not implemented
      Logger.error("Function #{inspect(server)}.#{func_name}/2 is not implemented")

      # Send required HTTP/2 headers first
      headers = %{
        ":status" => "200",
        "content-type" => "application/grpc+proto"
      }

      GRPC.Server.Adapters.ThousandIsland.send_headers(stream.payload, headers)

      # Then send error trailers
      error_trailers = %{
        # UNIMPLEMENTED
        "grpc-status" => "12",
        "grpc-message" => "Method not implemented"
      }

      GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, error_trailers)
      {:ok, :streaming_done}
    end
  end

  defp call_bidi_streaming(server, rpc, func_name, stream_state, stream, _connection) do
    # Check if function is implemented
    if function_exported?(server, func_name, 2) do
      stream_id = stream_state.stream_id
      message_buffer = stream_state.message_buffer

      Logger.info(
        "[call_bidi_streaming] Starting bidi stream #{stream_id} with #{length(message_buffer)} initial requests"
      )

      try do
        # Mark as streaming mode so send_headers will send immediately
        Process.put(:grpc_streaming_mode, true)

        # Convert initial messages to {flag, data} format
        initial_messages =
          Enum.map(message_buffer, fn %{compressed: compressed?, data: data} ->
            flag = if compressed?, do: 1, else: 0
            {flag, data}
          end)

        # Start BidiStream Task with initial messages in {flag, data} format
        {:ok, bidi_pid} = GRPC.Server.BidiStream.start_link(stream_id, initial_messages)

        Logger.info(
          "[call_bidi_streaming] BidiStream task started for stream #{stream_id}, pid=#{inspect(bidi_pid)}"
        )

        # Monitor the BidiStream task to detect early termination
        ref = Process.monitor(bidi_pid)
        Logger.debug("[call_bidi_streaming] Monitoring BidiStream task with ref #{inspect(ref)}")

        # Store the BidiStream PID in stream_state for later use
        # (when more DATA frames arrive)
        stream_state = stream.payload.stream_state
        updated_stream_state = %{stream_state | bidi_stream_pid: bidi_pid, handler_started: true}

        # CRITICAL: Store in process dictionary for immediate access
        # We can't use send() because handler is blocked in this dispatch call!
        _handler_pid = stream.payload.handler_pid

        Logger.info(
          "[Dispatcher] Storing bidi_pid #{inspect(bidi_pid)} in process dictionary for stream #{stream_id}"
        )

        # Store codec, compressor, and RPC in stream_state for decoding subsequent messages
        updated_stream_state = %{
          updated_stream_state
          | codec: stream.codec,
            compressor: stream.compressor,
            rpc: stream.rpc
        }

        # Store both PID and full state in process dictionary
        Process.put({:bidi_stream_pid, stream_id}, bidi_pid)
        Process.put({:bidi_stream_state, stream_id}, updated_stream_state)

        # Accumulate base headers (don't send yet - handler may add custom headers)
        base_headers = %{
          ":status" => "200",
          "content-type" => "application/grpc+proto"
        }

        # Add grpc-encoding if there's a compressor
        base_headers =
          if stream.compressor do
            Map.put(base_headers, "grpc-encoding", stream.compressor.name())
          else
            base_headers
          end

        # Accumulate base headers without sending
        GRPC.Server.set_headers(stream, base_headers)

        # Update the stream's payload to include bidi_stream_pid so adapter.reading_stream() can access it
        updated_stream = %{
          stream
          | payload: %{stream.payload | stream_state: updated_stream_state}
        }

        # CRITICAL: Run handler in a separate task to not block the connection handler
        # The connection handler MUST continue processing incoming DATA frames
        # and feed them to the BidiStream while the handler is consuming messages
        Task.start(fn ->
          try do
            # Use GRPC.Server.call to properly handle the request
            # This ensures the reading_stream is created correctly via adapter.reading_stream
            result = GRPC.Server.call(server, updated_stream, rpc, func_name)
            Logger.info("[call_bidi_streaming] Handler returned: #{inspect(result)}")

            case result do
              {:ok, stream} ->
                # Get custom trailers from process dictionary
                custom_trailers = Process.get({:grpc_custom_trailers, stream_id}, %{})
                trailers = Map.merge(%{"grpc-status" => "0"}, custom_trailers)
                GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, trailers)

              {:error, error} ->
                Logger.error("[call_bidi_streaming] Handler error result: #{inspect(error)}")

                trailers = %{
                  "grpc-status" => "#{error.status || 2}",
                  "grpc-message" => error.message || "Handler error"
                }

                GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, trailers)
            end
          rescue
            e in GRPC.RPCError ->
              Logger.error("[call_bidi_streaming] Handler RPC Error: #{inspect(e)}")

              trailers = %{
                "grpc-status" => "#{e.status}",
                "grpc-message" => e.message || ""
              }

              GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, trailers)

            e ->
              Logger.error(
                "[call_bidi_streaming] Handler error: #{Exception.message(e)}\n#{Exception.format_stacktrace(__STACKTRACE__)}"
              )

              trailers = %{
                "grpc-status" => "2",
                "grpc-message" => Exception.message(e)
              }

              GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, trailers)
          end
        end)

        # Return special marker for streaming (not data to send)
        {:ok, :streaming_done}
      rescue
        e in GRPC.RPCError ->
          Logger.error("[call_bidi_streaming] RPC Error: #{inspect(e)}")

          trailers = %{
            "grpc-status" => "#{e.status}",
            "grpc-message" => e.message || ""
          }

          GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, trailers)
          {:ok, :streaming_done}

        e ->
          Logger.error(
            "[call_bidi_streaming] Handler error: #{Exception.message(e)}\n#{Exception.format_stacktrace(__STACKTRACE__)}"
          )

          trailers = %{
            "grpc-status" => "2",
            "grpc-message" => Exception.message(e)
          }

          GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, trailers)
          {:ok, :streaming_done}
      end
    else
      # Function not implemented
      Logger.error("Function #{inspect(server)}.#{func_name}/2 is not implemented")

      # Send required HTTP/2 headers first
      headers = %{
        ":status" => "200",
        "content-type" => "application/grpc+proto"
      }

      GRPC.Server.Adapters.ThousandIsland.send_headers(stream.payload, headers)

      # Then send error trailers
      error_trailers = %{
        # UNIMPLEMENTED
        "grpc-status" => "12",
        "grpc-message" => "Method not implemented"
      }

      GRPC.Server.Adapters.ThousandIsland.send_trailers(stream.payload, error_trailers)
      {:ok, :streaming_done}
    end
  end

  # Encode responses - handles both unary (single response) and streaming (special marker)
  defp encode_responses(:streaming_done, _codec, _compressor, _rpc, _stream_state) do
    # Streaming was already handled incrementally, nothing to encode
    {:ok, :streaming_done}
  end

  defp encode_responses(response, codec, compressor, rpc, stream_state) do
    {_name, {_req_mod, _req_stream?}, {_res_mod, res_stream?}, _opts} = rpc

    if res_stream? do
      # This path shouldn't be reached anymore (streaming handled in call_server_streaming)
      Logger.debug("Unexpected: encode_responses called with streaming response")
      {:ok, :streaming_done}
    else
      # Unary - encode single response with custom headers/trailers from stream_state
      encode_response(response, codec, compressor, stream_state)
    end
  end

  # Encode a single message (without headers/trailers)
  defp encode_message(response, codec, compressor) do
    # Encode protobuf
    encoded = codec.encode(response)

    # Compress if needed
    {compressed_flag, data_binary} =
      if compressor do
        binary_data = IO.iodata_to_binary(encoded)
        {1, compressor.compress(binary_data)}
      else
        {0, IO.iodata_to_binary(encoded)}
      end

    # Calculate length
    length = byte_size(data_binary)

    # Build 5-byte length-prefixed message
    <<compressed_flag::8, length::32, data_binary::binary>>
  end

  defp encode_response(response, codec, compressor, stream_state) do
    try do
      # Encode the message using the helper
      message_data = encode_message(response, codec, compressor)

      # Build response headers
      base_headers = [
        {":status", "200"},
        {"content-type", "application/grpc+proto"}
      ]

      # Only add grpc-encoding if there's actual compression (not identity)
      headers =
        if compressor do
          base_headers ++ [{"grpc-encoding", compressor.name()}]
        else
          base_headers
        end

      # Merge custom headers from handler (for custom_metadata test)
      headers =
        if map_size(stream_state.custom_headers) > 0 do
          headers ++ Map.to_list(stream_state.custom_headers)
        else
          headers
        end

      # Build trailers (mandatory grpc-status)
      trailers = %{
        "grpc-status" => "0"
      }

      # Merge custom trailers from handler (for custom_metadata test)
      trailers = Map.merge(trailers, stream_state.custom_trailers)

      {:ok, headers, message_data, trailers}
    rescue
      e ->
        Logger.error("Failed to encode response: #{inspect(e)}")
        {:error, RPCError.exception(status: :internal, message: "Failed to encode response")}
    end
  end

  defp call_service(
         server,
         rpc,
         method_name,
         requests,
         stream_state,
         endpoint,
         codec,
         compressor,
         connection
       ) do
    {_name, {req_mod, req_stream?}, {res_mod, res_stream?}, _opts} = rpc
    func_name = Macro.underscore(method_name) |> String.to_atom()

    # Determine gRPC type based on streaming flags
    grpc_type = GRPC.Service.grpc_type(rpc)

    # Create a payload struct with metadata and handler info for streaming
    payload = %{
      headers: stream_state.metadata,
      stream_state: stream_state,
      handler_pid: stream_state.handler_pid,
      stream_id: stream_state.stream_id
    }

    grpc_stream = %GRPC.Server.Stream{
      server: server,
      endpoint: endpoint,
      grpc_type: grpc_type,
      request_mod: req_mod,
      response_mod: res_mod,
      rpc: rpc,
      codec: codec,
      compressor: compressor,
      adapter: GRPC.Server.Adapters.ThousandIsland,
      payload: payload
    }

    case {req_stream?, res_stream?} do
      {false, false} ->
        [request] = requests
        call_unary(server, func_name, request, grpc_stream)

      {true, false} ->
        call_client_streaming(server, func_name, requests, grpc_stream)

      {false, true} ->
        [request] = requests
        call_server_streaming(server, func_name, request, grpc_stream)

      {true, true} ->
        call_bidi_streaming(server, rpc, func_name, stream_state, grpc_stream, connection)
    end
  end
end
