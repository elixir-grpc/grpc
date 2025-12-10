defmodule GRPC.Server.HTTP2.Connection do
  @moduledoc """
  Represents the state of an HTTP/2 connection for gRPC.
  """
  require Logger

  alias GRPC.Transport.HTTP2.Frame
  alias GRPC.Transport.HTTP2.Settings
  alias GRPC.Transport.HTTP2.Errors
  alias GRPC.Server.HTTP2.StreamState

  # Inline hot path functions for performance
  @compile {:inline,
            extract_messages: 2, send_frame: 3, handle_headers_frame: 3, send_grpc_trailers: 4}

  defstruct local_settings: %Settings{},
            remote_settings: %Settings{},
            fragment_frame: nil,
            send_hpack_state: HPAX.new(4096),
            recv_hpack_state: HPAX.new(4096),
            send_window_size: 65_535,
            recv_window_size: 65_535,
            streams: %{},
            # Map of stream_id => stream_state
            next_stream_id: 2,
            # Client streams are odd, server push is even
            endpoint: nil,
            servers: %{},
            socket: nil,
            handler_pid: nil

  @typedoc "Encapsulates the state of an HTTP/2 connection"
  @type t :: %__MODULE__{
          local_settings: Settings.t(),
          remote_settings: Settings.t(),
          fragment_frame: Frame.Headers.t() | nil,
          send_hpack_state: term(),
          recv_hpack_state: term(),
          send_window_size: non_neg_integer(),
          recv_window_size: non_neg_integer(),
          streams: %{non_neg_integer() => map()},
          next_stream_id: non_neg_integer(),
          endpoint: atom(),
          servers: %{String.t() => [module()]},
          socket: ThousandIsland.Socket.t() | nil
        }

  @doc """
  Initializes a new HTTP/2 connection.
  """
  @spec init(ThousandIsland.Socket.t(), atom(), %{String.t() => [module()]}, keyword()) :: t()
  def init(socket, endpoint, servers, opts \\ []) do
    handler_pid = Keyword.get(opts, :handler_pid)
    Logger.debug("[Connection.init] handler_pid=#{inspect(handler_pid)}")

    connection = %__MODULE__{
      local_settings: struct!(Settings, Keyword.get(opts, :local_settings, [])),
      endpoint: endpoint,
      servers: servers,
      socket: socket,
      handler_pid: handler_pid
    }

    # Send initial SETTINGS frame per RFC9113§3.4
    settings_frame = %Frame.Settings{
      ack: false,
      settings: [
        header_table_size: connection.local_settings.header_table_size,
        max_concurrent_streams: connection.local_settings.max_concurrent_streams,
        initial_window_size: connection.local_settings.initial_window_size,
        max_frame_size: connection.local_settings.max_frame_size,
        max_header_list_size: connection.local_settings.max_header_list_size
      ]
    }

    send_frame(settings_frame, socket, connection)

    connection
  end

  @doc """
  Send headers for streaming response.
  """
  def send_headers(socket, stream_id, headers, connection) do
    # Check if stream still exists (may have been closed by RST_STREAM)
    unless Map.has_key?(connection.streams, stream_id) do
      Logger.warning(
        "[send_headers] SKIPPED - stream=#{stream_id} no longer exists (likely cancelled by client)"
      )

      connection
    else
      # Encode headers using HPAX - convert map to list of tuples
      Logger.debug("[send_headers] stream_id=#{stream_id}, headers=#{inspect(headers)}")
      headers_list = if is_map(headers), do: Map.to_list(headers), else: headers

      {header_block, _new_hpack} =
        HPAX.encode(:no_store, headers_list, connection.send_hpack_state)

      # Send HEADERS frame without END_STREAM
      frame = %Frame.Headers{
        stream_id: stream_id,
        fragment: header_block,
        end_stream: false,
        end_headers: true
      }

      send_frame(frame, socket, connection)
    end
  end

  @doc """
  Send data frame for streaming response.
  """
  def send_data(socket, stream_id, data, end_stream, connection) do
    # Check if stream still exists (may have been closed by RST_STREAM)
    unless Map.has_key?(connection.streams, stream_id) do
      Logger.warning(
        "[send_data] SKIPPED - stream=#{stream_id} no longer exists (likely cancelled by client)"
      )

      connection
    else
      # Send DATA frame
      frame = %Frame.Data{
        stream_id: stream_id,
        data: data,
        end_stream: end_stream
      }

      send_frame(frame, socket, connection)
    end
  end

  @doc """
  Send trailers (headers with END_STREAM) for streaming response.
  """
  def send_trailers(socket, stream_id, trailers, connection) do
    # Check if stream still exists (may have been closed by RST_STREAM)
    unless Map.has_key?(connection.streams, stream_id) do
      Logger.warning(
        "[send_trailers] SKIPPED - stream=#{stream_id} no longer exists (likely cancelled by client)"
      )

      connection
    else
      # Encode custom metadata (handles -bin suffix base64 encoding)
      # Note: encode_metadata filters out reserved headers like grpc-status
      encoded_custom = GRPC.Transport.HTTP2.encode_metadata(trailers)

      # Re-add reserved headers (grpc-status, etc) that were filtered out
      encoded_trailers =
        Map.merge(
          Map.take(trailers, ["grpc-status", "grpc-message"]),
          encoded_custom
        )

      # Convert map to list
      trailer_list = Map.to_list(encoded_trailers)

      {trailer_block, _new_hpack} =
        HPAX.encode(:no_store, trailer_list, connection.send_hpack_state)

      # Send HEADERS frame with END_STREAM
      frame = %Frame.Headers{
        stream_id: stream_id,
        fragment: trailer_block,
        end_stream: true,
        end_headers: true
      }

      connection = send_frame(frame, socket, connection)

      # Remove stream after sending END_STREAM (RFC 7540: stream transitions to closed)
      Logger.debug("[send_trailers] Removing stream #{stream_id} after sending END_STREAM")
      %{connection | streams: Map.delete(connection.streams, stream_id)}
    end
  end

  @doc """
  Set custom headers for a stream (used for custom_metadata test).
  """
  def set_stream_custom_headers(connection, stream_id, headers) do
    case Map.get(connection.streams, stream_id) do
      nil ->
        connection

      stream_state ->
        updated_stream = %{
          stream_state
          | custom_headers: Map.merge(stream_state.custom_headers, headers)
        }

        %{connection | streams: Map.put(connection.streams, stream_id, updated_stream)}
    end
  end

  @doc """
  Set custom trailers for a stream (used for custom_metadata test).
  """
  def set_stream_custom_trailers(connection, stream_id, trailers) do
    case Map.get(connection.streams, stream_id) do
      nil ->
        connection

      stream_state ->
        updated_stream = %{
          stream_state
          | custom_trailers: Map.merge(stream_state.custom_trailers, trailers)
        }

        %{connection | streams: Map.put(connection.streams, stream_id, updated_stream)}
    end
  end

  @doc """
  Get the stream state for a specific stream ID.
  Returns nil if stream doesn't exist.
  """
  def get_stream(connection, stream_id) do
    Map.get(connection.streams, stream_id)
  end

  @doc """
  Handles an incoming HTTP/2 frame.
  """
  @spec handle_frame(Frame.frame(), ThousandIsland.Socket.t(), t()) :: t()
  def handle_frame(frame, socket, connection) do
    do_handle_frame(frame, socket, connection)
  end

  defp do_handle_frame(
         %Frame.Continuation{end_headers: true, stream_id: stream_id} = frame,
         socket,
         %__MODULE__{fragment_frame: %Frame.Headers{stream_id: stream_id}} = connection
       ) do
    header_block = connection.fragment_frame.fragment <> frame.fragment
    header_frame = %{connection.fragment_frame | end_headers: true, fragment: header_block}
    do_handle_frame(header_frame, socket, %{connection | fragment_frame: nil})
  end

  defp do_handle_frame(
         %Frame.Continuation{end_headers: false, stream_id: stream_id} = frame,
         _socket,
         %__MODULE__{fragment_frame: %Frame.Headers{stream_id: stream_id}} = connection
       ) do
    fragment = connection.fragment_frame.fragment <> frame.fragment
    # TODO: Check max header list size
    fragment_frame = %{connection.fragment_frame | fragment: fragment}
    %{connection | fragment_frame: fragment_frame}
  end

  defp do_handle_frame(_frame, _socket, %__MODULE__{fragment_frame: %Frame.Headers{}}) do
    connection_error!("Expected CONTINUATION frame (RFC9113§6.10)")
  end

  defp do_handle_frame(%Frame.Settings{ack: true}, _socket, connection) do
    Logger.info("[Connection] Received SETTINGS ACK")
    connection
  end

  defp do_handle_frame(%Frame.Settings{ack: false} = frame, socket, connection) do
    Logger.info("[Connection] Received SETTINGS, sending ACK")
    %Frame.Settings{ack: true, settings: []} |> send_frame(socket, connection)

    remote_settings = apply_settings(connection.remote_settings, frame.settings)

    # Update HPACK table size if changed
    send_hpack_state =
      if remote_settings.header_table_size != connection.remote_settings.header_table_size do
        HPAX.resize(connection.send_hpack_state, remote_settings.header_table_size)
      else
        connection.send_hpack_state
      end

    %{connection | remote_settings: remote_settings, send_hpack_state: send_hpack_state}
  end

  defp do_handle_frame(%Frame.Ping{ack: false, payload: data}, socket, connection) do
    Logger.info("[Connection] Received PING, sending ACK")
    %Frame.Ping{ack: true, payload: data} |> send_frame(socket, connection)
    connection
  end

  defp do_handle_frame(%Frame.Ping{ack: true}, _socket, connection) do
    # Ignore PING ACKs for now (we don't track sent PINGs yet)
    connection
  end

  defp do_handle_frame(%Frame.Goaway{} = frame, _socket, connection) do
    Logger.info(
      "Received GOAWAY: last_stream_id=#{frame.last_stream_id}, error=#{frame.error_code}"
    )

    # TODO: Handle graceful shutdown
    connection
  end

  defp do_handle_frame(%Frame.WindowUpdate{stream_id: 0} = frame, _socket, connection) do
    Logger.info("[Connection] WINDOW_UPDATE connection-level: +#{frame.size_increment}")
    new_window = connection.send_window_size + frame.size_increment

    if new_window > 2_147_483_647 do
      connection_error!("Flow control window overflow (RFC9113§6.9.1)")
    end

    %{connection | send_window_size: new_window}
  end

  defp do_handle_frame(%Frame.Headers{} = frame, socket, connection) do
    Logger.info(
      "[RECV_HEADERS] stream=#{frame.stream_id}, end_headers=#{frame.end_headers}, end_stream=#{frame.end_stream}"
    )

    if frame.end_headers do
      handle_headers_frame(frame, socket, connection)
    else
      # Start accumulating CONTINUATION frames
      %{connection | fragment_frame: frame}
    end
  end

  defp do_handle_frame(%Frame.Data{stream_id: stream_id} = frame, socket, connection) do
    Logger.info(
      "[RECV_DATA] stream=#{stream_id}, size=#{byte_size(frame.data)}, end_stream=#{frame.end_stream}, stream_exists=#{Map.has_key?(connection.streams, stream_id)}"
    )

    case Map.get(connection.streams, stream_id) do
      nil ->
        Logger.warning(
          "[IGNORE_DATA] stream=#{stream_id} not found, size=#{byte_size(frame.data)} (stream already closed)"
        )

        connection

      stream_state ->
        # Send WINDOW_UPDATE to allow client to continue sending
        data_size = byte_size(frame.data)

        # Only send WINDOW_UPDATE if there's actual data (non-zero increment)
        if data_size > 0 do
          Logger.debug("[WINDOW_UPDATE] stream=#{stream_id}, size=#{data_size}")
          # Send connection-level WINDOW_UPDATE
          conn_window_update = %Frame.WindowUpdate{stream_id: 0, size_increment: data_size}
          send_frame(conn_window_update, socket, connection)

          # Send stream-level WINDOW_UPDATE  
          stream_window_update = %Frame.WindowUpdate{
            stream_id: stream_id,
            size_increment: data_size
          }

          send_frame(stream_window_update, socket, connection)
        end

        # Accumulate data in stream buffer
        updated_stream = %{stream_state | data_buffer: stream_state.data_buffer <> frame.data}

        # Mark if END_STREAM was received (stream half-closed remote)
        updated_stream =
          if frame.end_stream do
            %{updated_stream | end_stream_received: true}
          else
            updated_stream
          end

        updated_connection = %{
          connection
          | streams: Map.put(connection.streams, stream_id, updated_stream)
        }

        # For bidirectional streaming, process when we have complete messages
        # For other types, wait for END_STREAM
        should_process =
          if frame.end_stream do
            true
          else
            # For bidi, check if we have complete messages AND start processing
            updated_stream.is_bidi_streaming and has_complete_message?(updated_stream.data_buffer)
          end

        if should_process do
          # Process the request (for bidi, this starts the handler on first message)
          Logger.info(
            "[Connection] Stream #{stream_id} processing (end_stream=#{frame.end_stream}, bidi=#{updated_stream.is_bidi_streaming})"
          )

          process_grpc_request(socket, updated_stream, updated_connection, frame.end_stream)
        else
          # More data coming (non-bidi case)
          Logger.debug(
            "[Connection] Stream #{stream_id} waiting for more data (buffer=#{byte_size(updated_stream.data_buffer)} bytes)"
          )

          updated_connection
        end
    end
  end

  defp do_handle_frame(
         %Frame.RstStream{stream_id: stream_id, error_code: error},
         _socket,
         connection
       ) do
    stream_exists = Map.has_key?(connection.streams, stream_id)

    Logger.info(
      "[RECV_RST_STREAM] stream=#{stream_id}, error=#{error}, stream_exists=#{stream_exists}"
    )

    # Notify BidiStream (if exists) that stream was cancelled
    case Process.get({:bidi_stream_pid, stream_id}) do
      nil ->
        Logger.debug("[Connection] No BidiStream found for stream #{stream_id}")
        :ok

      pid ->
        Logger.info("[Connection] Cancelling BidiStream for stream #{stream_id}")
        GRPC.Server.BidiStream.cancel(pid)
    end

    # Clean up process dictionary
    Process.delete({:bidi_stream_pid, stream_id})
    Process.delete({:bidi_stream_state, stream_id})
    Process.delete({:grpc_custom_trailers, stream_id})

    # Remove stream from streams map
    streams = Map.delete(connection.streams, stream_id)
    Logger.debug("[REMOVE_STREAM] stream=#{stream_id}, remaining_streams=#{map_size(streams)}")
    %{connection | streams: streams}
  end

  defp do_handle_frame(%Frame.WindowUpdate{stream_id: stream_id} = frame, _socket, connection) do
    Logger.info("[Connection] WINDOW_UPDATE stream=#{stream_id}: +#{frame.size_increment}")
    # TODO: Update stream send window
    connection
  end

  defp do_handle_frame(%Frame.Priority{}, _socket, connection) do
    # gRPC doesn't use priority, ignore
    connection
  end

  defp do_handle_frame(%Frame.PushPromise{}, _socket, _connection) do
    # Server push not supported in gRPC
    connection_error!("PUSH_PROMISE not supported (RFC9113§8.4)")
  end

  defp do_handle_frame(%Frame.Unknown{}, _socket, connection) do
    # Ignore unknown frames per RFC9113§4.1
    connection
  end

  defp handle_headers_frame(frame, _socket, connection) do
    Logger.info("[handle_headers_frame] Decoding HPACK for stream #{frame.stream_id}")

    # Check if this is trailers for an existing stream
    case Map.get(connection.streams, frame.stream_id) do
      nil ->
        # New stream - decode headers and create stream state
        case HPAX.decode(frame.fragment, connection.recv_hpack_state) do
          {:ok, headers, new_hpack_state} ->
            Logger.info(
              "[handle_headers_frame] Decoded headers for stream #{frame.stream_id}: #{inspect(headers)}"
            )

            # Create stream state from headers
            stream_state =
              StreamState.from_headers(
                frame.stream_id,
                headers,
                connection.local_settings.initial_window_size
              )

            # Add handler_pid for streaming support
            stream_state = %{stream_state | handler_pid: connection.handler_pid}

            # Check if this is bidirectional streaming
            # For bidi, we need to process messages as they arrive (not wait for END_STREAM)
            is_bidi =
              GRPC.Server.HTTP2.Dispatcher.is_bidi_streaming?(stream_state.path, connection.servers)

            stream_state = %{stream_state | is_bidi_streaming: is_bidi}

            if is_bidi do
              Logger.info(
                "[handle_headers_frame] Stream #{frame.stream_id} is bidirectional streaming"
              )
            end

            # Store stream in connection
            streams = Map.put(connection.streams, frame.stream_id, stream_state)

            %{connection | recv_hpack_state: new_hpack_state, streams: streams}

          {:error, reason} ->
            connection_error!("HPACK decode error: #{inspect(reason)}")
        end

      _stream_state ->
        # Trailers for existing stream - just decode but don't create new stream
        # This can happen when client sends trailers after we've sent response/error
        Logger.info(
          "[handle_headers_frame] Ignoring trailers for stream #{frame.stream_id} (stream already processed)"
        )

        case HPAX.decode(frame.fragment, connection.recv_hpack_state) do
          {:ok, _headers, new_hpack_state} ->
            %{connection | recv_hpack_state: new_hpack_state}

          {:error, reason} ->
            Logger.warning(
              "[handle_headers_frame] Failed to decode trailers for stream #{frame.stream_id}: #{inspect(reason)}"
            )

            # Continue without updating HPACK state to avoid connection error
            connection
        end
    end
  end

  defp apply_settings(settings, []), do: settings
  # Convert map to keyword list if needed (for compatibility with grpc_core Frame.Settings)
  defp apply_settings(settings, settings_map) when is_map(settings_map) do
    apply_settings(settings, Map.to_list(settings_map))
  end

  defp apply_settings(settings, [{:header_table_size, value} | rest]) do
    apply_settings(%{settings | header_table_size: value}, rest)
  end

  defp apply_settings(settings, [{:enable_push, value} | rest]) do
    apply_settings(%{settings | enable_push: value}, rest)
  end

  defp apply_settings(settings, [{:max_concurrent_streams, value} | rest]) do
    apply_settings(%{settings | max_concurrent_streams: value}, rest)
  end

  defp apply_settings(settings, [{:initial_window_size, value} | rest]) do
    if value > 2_147_483_647 do
      connection_error!("Invalid initial window size (RFC9113§6.5.2)")
    end

    apply_settings(%{settings | initial_window_size: value}, rest)
  end

  defp apply_settings(settings, [{:max_frame_size, value} | rest]) do
    if value < 16_384 or value > 16_777_215 do
      connection_error!("Invalid max frame size (RFC9113§6.5.2)")
    end

    apply_settings(%{settings | max_frame_size: value}, rest)
  end

  defp apply_settings(settings, [{:max_header_list_size, value} | rest]) do
    apply_settings(%{settings | max_header_list_size: value}, rest)
  end

  defp apply_settings(settings, [_unknown | rest]) do
    # Ignore unknown settings per RFC9113§6.5.2
    apply_settings(settings, rest)
  end

  defp send_frame(frame, socket, connection) do
    Logger.debug(
      "[SEND_FRAME] type=#{inspect(frame.__struct__)}, stream=#{Map.get(frame, :stream_id, :none)}, flags=#{inspect(Map.get(frame, :flags, []))}}"
    )

    max_frame_size = connection.remote_settings.max_frame_size
    iodata = Frame.serialize(frame, max_frame_size)

    # Send all frame data at once (iodata is already properly formatted)
    # Skip sending if socket is nil (test mode)
    if socket != nil do
      ThousandIsland.Socket.send(socket, iodata)
    end

    connection
  end

  defp connection_error!(message) do
    raise Errors.ConnectionError, message
  end

  # Process a complete gRPC request (called when DATA arrives)
  # For bidi streaming, this may be called multiple times as messages arrive
  defp process_grpc_request(socket, stream_state, connection, end_stream) do
    Logger.debug(
      "[process_grpc_request] Processing gRPC call: #{stream_state.path} (end_stream=#{end_stream}, bidi=#{stream_state.is_bidi_streaming})"
    )

    # For bidi streaming that's already started, feed new messages to the BidiStream
    if stream_state.is_bidi_streaming and stream_state.handler_started do
      Logger.debug("[process_grpc_request] Bidi stream already started, feeding new messages")
      stream_state = extract_messages_from_buffer(stream_state)

      # Feed messages to the BidiStream Task after decoding them
      if length(stream_state.message_buffer) > 0 and stream_state.bidi_stream_pid do
        Logger.info(
          "[Connection] Decoding and feeding #{length(stream_state.message_buffer)} messages to BidiStream #{stream_state.stream_id}, pid=#{inspect(stream_state.bidi_stream_pid)}"
        )

        # Decode the messages using codec, compressor, and RPC from stream_state
        decoded_messages = decode_stream_messages(stream_state.message_buffer, stream_state)

        GRPC.Server.BidiStream.put_messages(stream_state.bidi_stream_pid, decoded_messages)
        # Clear both message_buffer and data_buffer after feeding
        _stream_state = %{stream_state | message_buffer: [], data_buffer: <<>>}
      end

      # If END_STREAM, mark the BidiStream as finished
      if end_stream and stream_state.bidi_stream_pid do
        Logger.info(
          "[Connection] Marking bidi stream #{stream_state.stream_id} as finished, pid=#{inspect(stream_state.bidi_stream_pid)}"
        )

        GRPC.Server.BidiStream.finish(stream_state.bidi_stream_pid)
      end

      # Update stream state
      streams = Map.put(connection.streams, stream_state.stream_id, stream_state)
      %{connection | streams: streams}
    else
      # First time processing (or non-bidi) - start the handler
      process_grpc_request_initial(socket, stream_state, connection, end_stream)
    end
  end

  defp process_grpc_request_initial(socket, stream_state, connection, end_stream) do
    Logger.debug("[process_grpc_request_initial] Starting gRPC handler")

    try do
      # Extract messages from data_buffer
      stream_state = extract_messages_from_buffer(stream_state)

      # Mark handler as started for bidi
      stream_state =
        if stream_state.is_bidi_streaming do
          %{stream_state | handler_started: true}
        else
          stream_state
        end

      # Update stream in connection before dispatching
      connection = %{
        connection
        | streams: Map.put(connection.streams, stream_state.stream_id, stream_state)
      }

      # Use dispatcher to handle the gRPC call
      Logger.debug("[process_grpc_request_initial] Dispatching gRPC call: #{stream_state.path}")

      Logger.debug(
        "[process_grpc_request] Message buffer: #{length(stream_state.message_buffer)} messages"
      )

      updated_connection =
        case GRPC.Server.HTTP2.Dispatcher.dispatch(
               stream_state,
               connection.servers,
               connection.endpoint,
               connection
             ) do
          {:ok, :streaming_done} ->
            # Streaming was handled incrementally via messages, nothing more to send
            Logger.debug("[process_grpc_request] Streaming completed")

            # For bidi streaming, update stream_state with full state from process dictionary
            # (includes bidi_stream_pid, codec, compressor, rpc)
            if stream_state.is_bidi_streaming do
              updated_stream_state = Process.get({:bidi_stream_state, stream_state.stream_id})

              Logger.info(
                "[Connection] Updated stream_state for stream #{stream_state.stream_id}: bidi_pid=#{inspect(updated_stream_state.bidi_stream_pid)}"
              )

              %{
                connection
                | streams:
                    Map.put(connection.streams, stream_state.stream_id, updated_stream_state)
              }
            else
              connection
            end

          {:ok, response_headers, response_data, trailers} ->
            Logger.debug("[process_grpc_request] RPC succeeded, sending response")
            # OPTIMIZATION: Send all frames in one syscall using iolist
            send_grpc_response_batch(
              socket,
              stream_state.stream_id,
              response_headers,
              response_data,
              trailers,
              connection
            )

          {:error, %GRPC.RPCError{} = error} ->
            Logger.error("[process_grpc_request] RPC error: #{inspect(error)}")
            # Check if stream still exists (might have been removed by concurrent error handling)
            if Map.has_key?(connection.streams, stream_state.stream_id) do
              updated_connection =
                send_grpc_error(socket, stream_state.stream_id, error, connection)

              # Remove stream immediately after error and return (don't continue processing)
              Logger.info(
                "[process_grpc_request] Removing stream #{stream_state.stream_id} after error"
              )

              %{
                updated_connection
                | streams: Map.delete(updated_connection.streams, stream_state.stream_id)
              }
            else
              Logger.warning(
                "[process_grpc_request] Stream #{stream_state.stream_id} already removed, skipping error send"
              )

              connection
            end
        end

      # Check if stream still exists (it may have been removed by error handling)
      stream_exists = Map.has_key?(updated_connection.streams, stream_state.stream_id)

      # For bidi streaming, keep the stream alive to receive more messages
      # But if it's the end of the stream (client sent END_STREAM), clean up
      if stream_state.is_bidi_streaming and not end_stream and stream_exists do
        Logger.debug("[process_grpc_request] Keeping bidi stream #{stream_state.stream_id} alive")
        updated_connection
      else
        # If stream doesn't exist, it was already removed (e.g., by send_grpc_error)
        if not stream_exists do
          updated_connection
        else
          # If bidi streaming and END_STREAM, mark as finished
          if stream_state.is_bidi_streaming and end_stream do
            # Get bidi_stream_pid from updated connection state
            updated_stream_state = updated_connection.streams[stream_state.stream_id]

            if updated_stream_state && updated_stream_state.bidi_stream_pid do
              Logger.info(
                "[Connection] Marking bidi stream #{stream_state.stream_id} as finished (initial END_STREAM), pid=#{inspect(updated_stream_state.bidi_stream_pid)}"
              )

              GRPC.Server.BidiStream.finish(updated_stream_state.bidi_stream_pid)
            end
          end

          # DON'T remove stream here for ThousandIsland adapter!
          # The adapter sends async messages ({:grpc_send_data}, {:grpc_send_trailers})
          # that will be processed later by handle_info in the Handler.
          # The stream will be removed when send_grpc_trailers is called (which sends END_STREAM).
          # 
          # For Cowboy adapter (synchronous), the response is sent immediately during dispatch,
          # so the stream can be removed here. But for ThousandIsland, we need to wait for
          # the async messages to be processed.
          #
          # TODO: Add a flag to StreamState to indicate if response was fully sent,
          # or let the trailers handler remove the stream after sending END_STREAM.
          Logger.debug(
            "[process_grpc_request] Keeping stream #{stream_state.stream_id} alive for async response (will be removed after trailers)"
          )

          updated_connection
        end
      end
    rescue
      e ->
        Logger.error("[process_grpc_request] Exception: #{inspect(e)}")

        Logger.error(
          "[process_grpc_request] Stacktrace:\n#{Exception.format_stacktrace(__STACKTRACE__)}"
        )

        # Check if stream still exists (might have been removed by concurrent error handling)
        if Map.has_key?(connection.streams, stream_state.stream_id) do
          updated_connection =
            send_grpc_error(
              socket,
              stream_state.stream_id,
              %{status: :internal, message: "Internal error"},
              connection
            )

          %{
            updated_connection
            | streams: Map.delete(updated_connection.streams, stream_state.stream_id)
          }
        else
          Logger.warning(
            "[process_grpc_request] Stream #{stream_state.stream_id} already removed in rescue, skipping error send"
          )

          connection
        end
    end
  end

  defp send_grpc_trailers(socket, stream_id, trailers, connection) do
    # Check if stream still exists (may have been closed by RST_STREAM)
    unless Map.has_key?(connection.streams, stream_id) do
      Logger.warning(
        "[send_grpc_trailers] SKIPPED - stream=#{stream_id} no longer exists (likely cancelled by client)"
      )

      connection
    else
      Logger.debug(
        "[send_grpc_trailers] Sending trailers for stream #{stream_id}: #{inspect(trailers)}"
      )

      # Convert map to list of tuples for HPAX
      trailer_list = Map.to_list(trailers)

      # Encode trailers using HPACK
      {trailer_block, new_hpack} =
        HPAX.encode(:no_store, trailer_list, connection.send_hpack_state)

      # Send HEADERS frame with END_STREAM flag
      headers_frame = %Frame.Headers{
        stream_id: stream_id,
        fragment: trailer_block,
        end_stream: true,
        end_headers: true
      }

      send_frame(headers_frame, socket, connection)

      # Return updated connection with new HPACK state
      %{connection | send_hpack_state: new_hpack}
    end
  end

  # OPTIMIZATION: Send headers + data + trailers in one syscall (Bandit-style batching)
  defp send_grpc_response_batch(
         socket,
         stream_id,
         response_headers,
         response_data,
         trailers,
         connection
       ) do
    max_frame_size = connection.remote_settings.max_frame_size

    # Encode headers frame if provided
    {headers_iodata, hpack_after_headers} =
      if response_headers && response_headers != [] do
        header_list =
          if is_map(response_headers), do: Map.to_list(response_headers), else: response_headers

        {encoded_headers, new_hpack} =
          HPAX.encode(:no_store, header_list, connection.send_hpack_state)

        headers_frame = %Frame.Headers{
          stream_id: stream_id,
          fragment: encoded_headers,
          end_stream: false,
          end_headers: true
        }

        {Frame.serialize(headers_frame, max_frame_size), new_hpack}
      else
        {[], connection.send_hpack_state}
      end

    # Encode data frame
    data_frame = %Frame.Data{
      stream_id: stream_id,
      data: response_data,
      end_stream: false
    }

    data_iodata = Frame.serialize(data_frame, max_frame_size)

    # Encode trailers frame
    trailer_list = Map.to_list(trailers)
    {trailer_block, final_hpack} = HPAX.encode(:no_store, trailer_list, hpack_after_headers)

    trailers_frame = %Frame.Headers{
      stream_id: stream_id,
      fragment: trailer_block,
      end_stream: true,
      end_headers: true
    }

    trailers_iodata = Frame.serialize(trailers_frame, max_frame_size)

    # Combine all frames into one iolist and send in single syscall
    combined_iodata = [headers_iodata, data_iodata, trailers_iodata]

    # Skip sending if socket is nil (test mode)
    if socket != nil do
      ThousandIsland.Socket.send(socket, combined_iodata)
    end

    # Return updated connection with new HPACK state
    %{connection | send_hpack_state: final_hpack}
  end

  defp extract_messages_from_buffer(stream_state) do
    # Extract 5-byte length-prefixed messages from data_buffer
    Logger.info(
      "[Connection] Extracting messages from data_buffer (#{byte_size(stream_state.data_buffer)} bytes)"
    )

    {messages, remaining} = extract_messages(stream_state.data_buffer, [])
    # Reverse since we build list backwards for performance
    extracted_count = length(messages)

    Logger.info(
      "[Connection] Extracted #{extracted_count} messages, #{byte_size(remaining)} bytes remaining"
    )

    %{stream_state | message_buffer: Enum.reverse(messages), data_buffer: remaining}
  end

  # Optimized: prepend instead of append for O(1) instead of O(n)
  defp extract_messages(
         <<compressed::8, length::32, payload::binary-size(length), rest::binary>>,
         acc
       ) do
    message = %{compressed: compressed == 1, data: payload}
    extract_messages(rest, [message | acc])
  end

  defp extract_messages(buffer, acc) do
    # Not enough data for a complete message
    {acc, buffer}
  end

  defp decode_stream_messages(message_buffer, stream_state) do
    # Extract request type from RPC definition
    # RPC format: {name, {request_module, is_stream?}, {reply_module, is_stream?}, options}
    {_name, {request_module, _is_stream?}, _reply, _options} = stream_state.rpc
    codec = stream_state.codec
    compressor = stream_state.compressor

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
  end

  # Made public so Handler can call it when deadline exceeded during send_reply
  def send_grpc_error(socket, stream_id, error, connection) do
    status = Map.get(error, :status, :unknown)
    message = Map.get(error, :message, "Unknown error")

    # Convert atom status to integer code
    status_code = if is_atom(status), do: apply(GRPC.Status, status, []), else: status

    stream_state = Map.get(connection.streams, stream_id)

    # Se o stream não existe OU já enviamos erro, não fazer nada
    if !stream_state do
      Logger.warning("[SEND_GRPC_ERROR] stream=#{stream_id} SKIPPED - stream not found")
      connection
    else
      if stream_state.error_sent do
        Logger.warning("[SEND_GRPC_ERROR] stream=#{stream_id} SKIPPED - error already sent")
        connection
      else
        headers_sent = stream_state.headers_sent
        end_stream_received = stream_state.end_stream_received

        Logger.warning(
          "[SEND_GRPC_ERROR] stream=#{stream_id}, status=#{status_code}, message=#{message}, headers_sent=#{headers_sent}, end_stream_received=#{end_stream_received}"
        )

        # RFC 7540 Section 5.1: Se já enviamos END_STREAM (via headers_sent=true em resposta anterior),
        # o stream está "closed" e NÃO podemos enviar mais frames (exceto PRIORITY)
        # Nesse caso, apenas remover o stream e não enviar nada
        if headers_sent && end_stream_received do
          Logger.warning(
            "[SEND_GRPC_ERROR] stream=#{stream_id} SKIPPED - stream is fully closed (both sides sent END_STREAM)"
          )

          %{connection | streams: Map.delete(connection.streams, stream_id)}
        else
          # Check if headers were already sent for this stream

          # If headers not sent yet AND stream hasn't received END_STREAM, send HTTP/2 headers first
          # If stream received END_STREAM, it's half-closed remote - we can only send TRAILERS
          updated_connection =
            if !headers_sent && !end_stream_received do
              Logger.warning(
                "[SEND_GRPC_ERROR] Sending HTTP/2 headers first for stream=#{stream_id}"
              )

              headers = %{":status" => "200", "content-type" => "application/grpc+proto"}
              send_headers(socket, stream_id, headers, connection)

              # Verify stream still exists after send_headers (may have been closed by RST_STREAM)
              if Map.has_key?(connection.streams, stream_id) do
                # Mark headers as sent in the stream state
                updated_stream = %{stream_state | headers_sent: true}
                updated_conn = put_in(connection.streams[stream_id], updated_stream)

                trailers = %{
                  "grpc-status" => to_string(status_code),
                  "grpc-message" => message
                }

                Logger.warning(
                  "[SEND_GRPC_ERROR] Sending TRAILERS with END_STREAM for stream=#{stream_id}"
                )

                send_grpc_trailers(socket, stream_id, trailers, updated_conn)
              else
                Logger.warning(
                  "[SEND_GRPC_ERROR] SKIPPED trailers - stream=#{stream_id} was closed after sending headers"
                )

                connection
              end
            else
              # If headers were sent OR stream received END_STREAM, just send trailers
              if end_stream_received && !headers_sent do
                Logger.warning(
                  "[SEND_GRPC_ERROR] Stream #{stream_id} received END_STREAM, skipping HEADERS, sending only TRAILERS"
                )
              end

              trailers = %{
                "grpc-status" => to_string(status_code),
                "grpc-message" => message
              }

              Logger.warning(
                "[SEND_GRPC_ERROR] Sending TRAILERS with END_STREAM for stream=#{stream_id}"
              )

              send_grpc_trailers(socket, stream_id, trailers, connection)
            end

          # Verify stream still exists before marking error_sent (may have been closed by RST_STREAM)
          updated_connection =
            if Map.has_key?(updated_connection.streams, stream_id) do
              # Marcar que já enviamos erro (RFC 7540: após END_STREAM, o stream está closed)
              update_in(updated_connection.streams[stream_id], fn s ->
                if s, do: %{s | error_sent: true}, else: nil
              end)
            else
              Logger.warning(
                "[SEND_GRPC_ERROR] SKIPPED marking error_sent - stream=#{stream_id} was already closed"
              )

              updated_connection
            end

          # RFC 7540: Após enviar END_STREAM, o stream transiciona para "closed"
          # Remover imediatamente para evitar processar mais mensagens neste stream
          Logger.warning("[REMOVE_STREAM] stream=#{stream_id} - removed after sending error")
          %{updated_connection | streams: Map.delete(updated_connection.streams, stream_id)}
        end
      end
    end
  end

  # Check if buffer has at least one complete gRPC message
  # gRPC message format: 1 byte compressed flag + 4 bytes length + N bytes data
  defp has_complete_message?(buffer) when byte_size(buffer) < 5, do: false

  defp has_complete_message?(<<_compressed::8, length::32, rest::binary>>) do
    byte_size(rest) >= length
  end
end
