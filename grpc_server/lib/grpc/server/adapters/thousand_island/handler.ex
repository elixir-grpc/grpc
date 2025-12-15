defmodule GRPC.Server.Adapters.ThousandIsland.Handler do
  @moduledoc """
  ThousandIsland handler for gRPC requests.

  Implementa ThousandIsland.Handler para lidar com gRPC sobre HTTP/2.
  """
  use ThousandIsland.Handler

  alias GRPC.Server.HTTP2.Connection
  alias GRPC.Transport.HTTP2.Frame
  alias GRPC.Transport.HTTP2.Errors
  require Logger

  # HTTP/2 connection preface per RFC9113§3.4
  @connection_preface "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

  # Inline hot path functions
  @compile {:inline, handle_data: 3, handle_preface: 3, handle_frames_loop: 5}

  @impl ThousandIsland.Handler
  def handle_connection(socket, handler_options) do
    Logger.debug("New HTTP/2 connection established - socket: #{inspect(socket)}")

    # Initialize ETS cache for codecs/compressors lookup  
    GRPC.Server.Cache.init()

    # Support both keyword list and map formats
    {servers_list, endpoint, opts} =
      if is_map(handler_options) do
        {Map.get(handler_options, :servers, []), Map.get(handler_options, :endpoint),
         Map.get(handler_options, :opts, [])}
      else
        {Keyword.get(handler_options, :servers, []), Keyword.get(handler_options, :endpoint),
         Keyword.get(handler_options, :opts, [])}
      end

    servers =
      cond do
        is_map(servers_list) and not is_struct(servers_list) -> servers_list
        is_list(servers_list) -> GRPC.Server.servers_to_map(servers_list)
        true -> %{}
      end

    Logger.debug("[handle_connection] servers: #{inspect(servers)}")
    Logger.debug("[handle_connection] endpoint: #{inspect(endpoint)}")

    new_state = %{
      endpoint: endpoint,
      servers: servers,
      opts: opts,
      connection: nil,
      buffer: <<>>,
      preface_received: false,
      accumulated_headers: %{}
    }

    {:continue, new_state}
  end

  @impl ThousandIsland.Handler
  def handle_data(data, socket, %{preface_received: false, buffer: buffer} = state) do
    new_buffer = buffer <> data
    handle_preface(new_buffer, socket, state)
  end

  def handle_data(data, socket, %{buffer: buffer} = state) do
    new_buffer = buffer <> data
    handle_frames(new_buffer, socket, state)
  end

  @impl ThousandIsland.Handler
  def handle_close(_socket, state) do
    Logger.debug("Connection closed")
    {:close, state}
  end

  @impl ThousandIsland.Handler
  def handle_error(reason, _socket, state) do
    Logger.error("Connection error: #{inspect(reason)}")
    {:close, state}
  end

  def handle_info({:grpc_accumulate_headers, stream_id, headers}, {socket, state}) do
    current_headers = Map.get(state.accumulated_headers, stream_id, %{})
    updated_headers = Map.merge(current_headers, headers)
    new_accumulated = Map.put(state.accumulated_headers, stream_id, updated_headers)
    {:noreply, {socket, %{state | accumulated_headers: new_accumulated}}}
  end

  def handle_info({:grpc_send_headers, stream_id, headers}, {socket, state}) do
    Logger.debug("[Streaming] Sending headers for stream #{stream_id}")
    Connection.send_headers(socket, stream_id, headers, state.connection)
    {:noreply, {socket, state}}
  end

  def handle_info({:grpc_send_data, stream_id, data}, {socket, state}) do
    accumulated = Map.get(state.accumulated_headers, stream_id, %{})

    new_state =
      if map_size(accumulated) > 0 do
        Connection.send_headers(socket, stream_id, accumulated, state.connection)
        %{state | accumulated_headers: Map.delete(state.accumulated_headers, stream_id)}
      else
        state
      end

    Connection.send_data(socket, stream_id, data, false, new_state.connection)
    {:noreply, {socket, new_state}}
  end

  def handle_info({:grpc_send_trailers, stream_id, trailers}, {socket, state}) do
    accumulated = Map.get(state.accumulated_headers, stream_id, %{})

    new_state =
      if map_size(accumulated) > 0 do
        updated_conn = Connection.send_headers(socket, stream_id, accumulated, state.connection)

        %{
          state
          | accumulated_headers: Map.delete(state.accumulated_headers, stream_id),
            connection: updated_conn
        }
      else
        state
      end

    # Send trailers (headers with END_STREAM) for streaming
    # This will also remove the stream from the connection
    updated_connection =
      Connection.send_trailers(socket, stream_id, trailers, new_state.connection)

    new_state = %{new_state | connection: updated_connection}
    {:noreply, {socket, new_state}}
  end

  def handle_info({:update_stream_state, stream_id, updated_stream_state}, {socket, state}) do
    Logger.debug(
      "[Handler] Updating stream_state for stream #{stream_id}, bidi_pid=#{inspect(updated_stream_state.bidi_stream_pid)}"
    )

    connection = state.connection

    updated_connection = %{
      connection
      | streams: Map.put(connection.streams, stream_id, updated_stream_state)
    }

    {:noreply, {socket, %{state | connection: updated_connection}}}
  end

  def handle_info(_msg, {socket, state}) do
    {:noreply, {socket, state}}
  end

  defp handle_preface(buffer, _socket, state) when byte_size(buffer) < 24 do
    # Wait for more data (preface is 24 bytes)
    {:continue, %{state | buffer: buffer}}
  end

  defp handle_preface(<<@connection_preface, remaining::binary>>, socket, state) do
    # Valid preface, initialize connection  
    try do
      opts = Keyword.put(state.opts, :handler_pid, self())
      connection = Connection.init(socket, state.endpoint, state.servers, opts)
      new_state = %{state | connection: connection, preface_received: true, buffer: <<>>}

      if byte_size(remaining) > 0 do
        handle_frames(remaining, socket, new_state)
      else
        {:continue, new_state}
      end
    rescue
      e ->
        Logger.error(
          "Error initializing connection: #{inspect(e)}\n#{Exception.format_stacktrace()}"
        )

        {:close, state}
    end
  end

  defp handle_preface(_buffer, _socket, state) do
    Logger.debug("Invalid HTTP/2 preface")
    {:close, state}
  end

  defp handle_frames(buffer, socket, state) do
    handle_frames_loop(
      buffer,
      socket,
      state.connection,
      state.connection.remote_settings.max_frame_size,
      state
    )
  end

  defp handle_frames_loop(buffer, socket, connection, max_frame_size, original_state) do
    case Frame.deserialize(buffer, max_frame_size) do
      {{:ok, frame}, rest} ->
        try do
          new_connection = Connection.handle_frame(frame, socket, connection)

          if byte_size(rest) > 0 do
            # Continue processing with updated connection
            handle_frames_loop(rest, socket, new_connection, max_frame_size, original_state)
          else
            # All frames processed, return updated state
            {:continue, %{original_state | connection: new_connection, buffer: <<>>}}
          end
        rescue
          e in Errors.ConnectionError ->
            Logger.debug("Connection error: #{e.message}")
            {:close, original_state}

          e in Errors.StreamError ->
            Logger.debug("Stream error: #{e.message}")
            {:continue, %{original_state | connection: connection, buffer: rest}}
        end

      {{:more, _partial}, <<>>} ->
        # Need more data to parse frame
        {:continue, %{original_state | connection: connection, buffer: buffer}}

      {{:error, error_code, reason}, _rest} ->
        Logger.debug("Frame deserialization error: #{reason} (code: #{error_code})")
        {:close, original_state}

      nil ->
        # No more frames to parse
        {:continue, %{original_state | connection: connection, buffer: <<>>}}
    end
  end

  def read_full_body(pid) do
    GenServer.call(pid, :read_full_body)
  end

  def read_body(pid) do
    GenServer.call(pid, :read_body)
  end

  def send_data(pid, data, opts) do
    GenServer.cast(pid, {:send_data, data, opts})
  end

  def send_headers(pid, headers) do
    GenServer.cast(pid, {:send_headers, headers})
  end

  def set_headers(pid, headers) do
    GenServer.cast(pid, {:set_headers, headers})
  end

  def set_trailers(pid, trailers) do
    GenServer.cast(pid, {:set_trailers, trailers})
  end

  def send_trailers(pid, trailers) do
    GenServer.cast(pid, {:send_trailers, trailers})
  end

  def get_headers(pid) do
    GenServer.call(pid, :get_headers)
  end

  def get_peer(pid) do
    GenServer.call(pid, :get_peer)
  end

  def get_cert(pid) do
    GenServer.call(pid, :get_cert)
  end

  def get_query_string(pid) do
    GenServer.call(pid, :get_query_string)
  end

  def get_bindings(pid) do
    GenServer.call(pid, :get_bindings)
  end

  def set_compressor(pid, compressor) do
    GenServer.cast(pid, {:set_compressor, compressor})
  end
end
