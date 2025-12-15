defmodule GRPC.Server.Adapters.ThousandIsland do
  @moduledoc """
  A server (`GRPC.Server.Adapter`) adapter using `:thousand_island`.

  ThousandIsland is a modern, pure Elixir socket server that provides:
  - Built-in connection pooling
  - Efficient resource management  
  - Better integration with Elixir/OTP ecosystem
  - Simpler architecture than Cowboy/Ranch

  ## Advantages over Cowboy

  - **Built-in pooling**: Native connection pool management
  - **Lower overhead**: Simpler architecture, fewer layers
  - **Modern design**: Built with current Elixir best practices
  - **Telemetry integration**: First-class observability support

  ## Architecture & Process Model

  ### Module Responsibilities

  1. **GRPC.Server.Adapters.ThousandIsland** (this module)
     - Adapter API implementation (`GRPC.Server.Adapter` behaviour)
     - Server lifecycle (start/stop)
     - Helper functions: `send_reply/3`, `send_headers/2`, `send_trailers/2`
     - These functions send async messages to the Handler process

  2. **GRPC.Server.Adapters.ThousandIsland.Handler**
     - ThousandIsland.Handler behaviour implementation
     - HTTP/2 connection lifecycle
     - Frame processing coordinator
     - Message handling (async operations from user handlers)
     - State management (accumulated headers, connection state)

  3. **GRPC.Server.HTTP2.Connection**
     - HTTP/2 protocol state machine
     - Frame encoding/decoding (HEADERS, DATA, SETTINGS, etc.)
     - HPACK compression/decompression
     - Stream state management (per-stream tracking)
     - Flow control

  4. **GRPC.Server.HTTP2.StreamState**
     - Per-stream state tracking
     - Message buffering and framing
     - gRPC message assembly (5-byte length-prefix framing)
     - Stream lifecycle (idle -> open -> half_closed -> closed)

  5. **GRPC.Server.HTTP2.Dispatcher**
     - RPC method routing and dispatch
     - Determines RPC type (unary, client_stream, server_stream, bidi_stream)
     - Spawns handler tasks for streaming RPCs
     - Manages BidiStream GenServer for bidirectional streaming

  6. **GRPC.Server.BidiStream**
     - GenServer for bidirectional streaming
     - Message queue for incoming requests
     - Lazy enumerable generation for handler consumption
     - Backpressure management

  ## Request Pipeline by RPC Type

  ### Process Hierarchy

  ```mermaid
  graph TD
    A[ThousandIsland Supervisor] --> B[Handler Process]
    B --> C[Connection State<br/>HTTP2.Connection]
    C --> D[Stream States<br/>HTTP2.StreamState per stream_id]
    B --> E[User Handler Tasks<br/>spawned per RPC]
    E --> F[BidiStream GenServer<br/>only for bidi streaming]
  ```

  ### 1. Unary RPC (request -> response)

  #### Request Path

  1. **Client sends HTTP/2 frames** → TCP socket
  2. **Handler.handle_data/3** receives raw bytes
     - Buffers until complete frames available
  3. **Connection.handle_frame/3** processes each frame
     - HEADERS frame → decode headers, create StreamState
     - DATA frame → accumulate in StreamState.data_buffer
     - When END_STREAM received → decode gRPC message
  4. **Connection.process_grpc_request/4** extracts complete request
     - Decodes 5-byte length-prefixed message
     - Looks up RPC method from path
  5. **Dispatcher.dispatch/4** routes to handler
     - Calls `Dispatcher.call_unary/5`
     - Directly invokes user handler function: `MyServer.my_method(request, stream)`
     - Handler runs **synchronously** in Handler process

  #### Response Path

  1. **Handler returns response** (or calls `GRPC.Server.send_reply/2`)
  2. **Dispatcher** sends response headers + data + trailers
     - Headers: `{":status" => "200", "content-type" => "application/grpc+proto"}`
     - Data: gRPC framed message (5-byte length + protobuf)
     - Trailers: `{"grpc-status" => "0"}`
  3. **Connection.send_headers/4** encodes HEADERS frame
     - HPACK compression
     - Sends via socket
  4. **Connection.send_data/5** encodes DATA frame
     - Sets END_STREAM flag
  5. **Connection.send_trailers/4** encodes final HEADERS frame
     - Sets END_HEADERS + END_STREAM flags

  **Process Model**: Single Handler process handles entire request synchronously

  ### 2. Client Streaming RPC (stream of requests -> response)

  #### Request Path

  1. **Client sends multiple DATA frames** (END_STREAM on last)
  2. **Handler.handle_data/3** → **Connection.handle_frame/3**
     - Each DATA frame appends to StreamState.data_buffer
     - Messages accumulated in StreamState.message_buffer
  3. **When END_STREAM received** → **process_grpc_request/4**
     - All messages decoded
  4. **Dispatcher.call_client_streaming/5**
     - Creates `Stream.unfold` from buffered messages
     - Calls handler: `MyServer.my_method(request_enum, stream)`
     - Handler **synchronously** consumes stream

  #### Response Path

  Same as Unary (single response at end)

  **Process Model**: Single Handler process, synchronous handler execution

  ### 3. Server Streaming RPC (request -> stream of responses)

  #### Request Path

  Same as Unary (single request)

  #### Response Path

  1. **Handler calls `GRPC.Server.send_reply/2` multiple times**
  2. **This adapter's `send_reply/3`** sends async message:
     - `send(handler_pid, {:grpc_send_data, stream_id, framed_data})`
  3. **Handler.handle_info/2** receives `:grpc_send_data`
     - Calls `Connection.send_data/5` to send DATA frame
     - Each call is a separate DATA frame (END_STREAM=false)
  4. **Final trailers** sent at end
     - `GRPC.Server.send_trailers/2` → `{:grpc_send_trailers, ...}`
     - Handler sends final HEADERS frame with END_STREAM

  **Process Model**: 

  - Handler spawns **Task** to run user handler asynchronously
  - Handler process receives messages from Task and sends frames
  - Task communicates via messages to Handler process

  ### 4. Bidirectional Streaming RPC (stream ↔ stream)

  This is the most complex case with multiple concurrent processes.

  #### Process Model

  ```mermaid
  graph LR
    subgraph HP["Handler Process (#PID<0.545.0>)"]
      HS["State: accumulated_headers<br/>stream_id => headers"]
    end
    
    subgraph UT["User Handler Task (#PID<0.XXX.0>)"]
      UTR["Runs: MyServer.full_duplex_call<br/>request_enum, stream"]
    end
    
    subgraph BS["BidiStream GenServer (#PID<0.YYY.0>)"]
      BSQ["Queue: Buffered incoming requests"]
    end
    
    Client -->|HTTP/2 frames| HP
    HP -->|HTTP/2 frames| Client
    UT -->|:grpc_send_data| HP
    HP -->|:add_message| BS
    BS -->|request_enum<br/>lazy pull| UT
  ```

  #### Request Path (Incoming)

  1. **Client sends DATA frames** (multiple, no END_STREAM until done)
  2. **Handler.handle_data/3** → **Connection.handle_frame/3**
     - Each DATA frame processed immediately
  3. **Connection.process_grpc_request/4** (on first HEADERS)
     - Creates StreamState with `is_bidi_streaming: true`
     - Calls **Dispatcher.call_bidi_streaming/5**
  4. **Dispatcher.call_bidi_streaming/5** (CRITICAL!)
     - **Starts BidiStream GenServer**: `{:ok, bidi_pid} = BidiStream.start_link(stream_id, [])`
     - **Accumulates base headers** (don't send yet!):
       ```elixir
       base_headers = %{":status" => "200", "content-type" => "application/grpc+proto"}
       GRPC.Server.set_headers(stream, base_headers)  # Sends {:grpc_accumulate_headers, ...}
       ```
     - **Spawns User Handler Task**:
       ```elixir
       request_enum = BidiStream.to_enum(bidi_pid)
       Task.start(fn -> 
         MyServer.full_duplex_call(request_enum, stream)
       end)
       ```
     - **Stores bidi_pid in StreamState** for later DATA frames
     - Returns `:streaming_done` (dispatcher exits, Handler continues)
  5. **Subsequent DATA frames** (while handler running)
     - **Connection.handle_frame/3** receives DATA frame
     - Decodes gRPC message
     - **Sends to BidiStream**: `GenServer.cast(bidi_pid, {:add_message, message})`
     - BidiStream queues message for handler consumption
  6. **User Handler consumes request_enum**
     - `Enum.each(request_enum, fn req -> ... end)`
     - Each iteration pulls from BidiStream (lazy, blocks if queue empty)
     - BidiStream dequeues message and returns to handler

  #### Response Path (Outgoing)

  1. **User Handler calls `GRPC.Server.send_reply/2`**
     - Runs in User Task process
  2. **This adapter's `send_reply/3`**:
     ```elixir
     send(handler_pid, {:grpc_send_data, stream_id, framed_data})
     ```
  3. **Handler.handle_info({:grpc_send_data, ...}, state)**
     - **CRITICAL: Header accumulation pattern**
     - Checks if accumulated headers exist for stream_id:
       ```elixir
       accumulated = Map.get(state.accumulated_headers, stream_id, %{})
       if map_size(accumulated) > 0 do
         # First DATA frame - send accumulated headers first!
         Connection.send_headers(socket, stream_id, accumulated, connection)
         # Clear accumulated headers
         state = %{state | accumulated_headers: Map.delete(..., stream_id)}
       end
       ```
     - Then sends DATA frame:
       ```elixir
       Connection.send_data(socket, stream_id, data, false, connection)
       ```
  4. **Custom metadata support**:
     - If handler calls `GRPC.Server.send_headers/2`:
       ```elixir
       send(handler_pid, {:grpc_accumulate_headers, stream_id, headers})
       ```
     - **Handler.handle_info({:grpc_accumulate_headers, ...})**:
       ```elixir
       current = Map.get(state.accumulated_headers, stream_id, %{})
       updated = Map.merge(current, headers)
       state = %{state | accumulated_headers: Map.put(..., stream_id, updated)}
       ```
     - These headers are sent with FIRST DATA frame (see step 3)
  5. **Final trailers** (when handler finishes)
     - `GRPC.Server.send_trailers/2` → `{:grpc_send_trailers, stream_id, trailers}`
     - **Handler.handle_info({:grpc_send_trailers, ...})**:
       - Checks for unsent accumulated headers (empty stream case):
         ```elixir
         if map_size(accumulated_headers) > 0 do
           Connection.send_headers(...)  # Send base headers first
         end
         Connection.send_trailers(...)  # Then trailers with END_STREAM
         ```

  #### Critical Timing & Synchronization

  **Problem**: HTTP/2 requires HEADERS before DATA, but we need to:
  1. Allow handler to add custom headers (via `send_headers/2`)
  2. Send base headers (`:status`, `content-type`)
  3. All in FIRST HEADERS frame (can't send HEADERS twice)

  **Solution** (inspired by Cowboy's `set_resp_headers` pattern):
  1. **Dispatcher accumulates base headers** without sending:
     - Sends `{:grpc_accumulate_headers, stream_id, base_headers}` message
     - Handler stores in `state.accumulated_headers`
  2. **User handler can add custom headers** (optional):
     - Calls `GRPC.Server.send_headers(stream, custom_headers)`
     - Merges into accumulated headers in Handler state
  3. **First `send_reply` sends ALL accumulated headers**:
     - Handler checks `accumulated_headers[stream_id]`
     - Sends merged (base + custom) headers in SINGLE HEADERS frame
     - Clears accumulated headers
     - Then sends DATA frame
  4. **For empty streams** (no `send_reply` calls):
     - `send_trailers` checks for unsent accumulated headers
     - Sends headers before trailers

  **Why this works**:
  - Handler process is single-threaded message loop
  - Messages processed in order: accumulate_headers → send_data → send_trailers
  - Accumulated headers guaranteed to be merged before first DATA
  - User Task sends messages asynchronously, Handler serializes them

  ## Message Flow Diagram (Bidi Streaming)

  ```mermaid
  sequenceDiagram
    participant Client
    participant Handler as Handler Process
    participant Task as User Task
    participant Bidi as BidiStream
    
    Client->>Handler: HEADERS
    Handler->>Handler: create StreamState
    Handler->>Bidi: start BidiStream
    Handler->>Handler: accumulate headers
    Handler->>Task: spawn Task
    Task->>Task: request_enum (lazy, blocks)
    
    Client->>Handler: DATA(req1)
    Handler->>Handler: decode message
    Handler->>Bidi: add_message
    Bidi->>Task: pull next (req1)
    Task->>Task: process req1
    Task->>Task: send_reply(resp1)
    Task->>Handler: :grpc_send_data
    Handler->>Handler: send headers (1st!)
    Handler->>Client: HEADERS
    Handler->>Client: DATA(resp1)
    
    Client->>Handler: DATA(req2)
    Handler->>Bidi: add_message
    Bidi->>Task: pull next (req2)
    Task->>Task: process req2
    Task->>Task: send_reply(resp2)
    Task->>Handler: :grpc_send_data
    Handler->>Client: DATA(resp2)
    
    Client->>Handler: DATA (END_STREAM)
    Handler->>Bidi: finish stream
    Bidi->>Task: nil (done)
    Task->>Task: handler finishes
    Task->>Task: send_trailers
    Task->>Handler: :grpc_send_trailers
    Handler->>Client: HEADERS(trailers)<br/>END_STREAM
  ```

  ## Key Design Patterns

  1. **Async Message Passing**: User handlers send messages to Handler process
     - Decouples user code from HTTP/2 frame management
     - Handler serializes all socket writes (thread-safe)

  2. **Lazy Enumerables**: Streaming requests use `Stream.unfold`
     - Backpressure: handler blocks if no messages available
     - Memory efficient: doesn't buffer entire stream

  3. **Header Accumulation**: Inspired by Cowboy's `set_resp_headers`
     - Accumulate headers in Handler state (not process dictionary!)
     - Send on first DATA or trailers (whichever comes first)
     - Allows custom headers while respecting HTTP/2 constraints

  4. **GenServer Message Queue**: BidiStream acts as message buffer
     - Decouples incoming frame rate from handler processing rate
     - Natural backpressure via GenServer mailbox

  5. **Process Dictionary for Stream Metadata**: 
     - Used in Dispatcher context: `Process.put({:bidi_stream_pid, stream_id}, pid)`
     - Allows Connection to find BidiStream when DATA arrives
     - Alternative to passing state through deep call stack
  """

  @behaviour GRPC.Server.Adapter

  require Logger
  alias GRPC.Server.Adapters.ThousandIsland.Handler

  @default_num_acceptors 100
  @default_max_connections 16384

  @doc """
  Starts a ThousandIsland server.

  ## Options
    * `:ip` - The IP to bind the server to (default: listen on all interfaces)
    * `:port` - The port to listen on (required)
    * `:num_acceptors` - Number of acceptor processes (default: 100)
    * `:num_connections` - Maximum concurrent connections (default: 16384)
    * `:transport_options` - Additional transport options to pass to ThousandIsland
  """
  @impl true
  def start(endpoint, servers, port, opts) do
    server_opts = build_server_opts(endpoint, servers, port, opts)

    case ThousandIsland.start_link(server_opts) do
      {:ok, pid} ->
        actual_port = get_actual_port(pid, port)
        {:ok, pid, actual_port}

      {:error, {:already_started, pid}} ->
        Logger.warning("Failed to start #{servers_name(endpoint, servers)}: already started")
        actual_port = get_actual_port(pid, port)
        {:ok, pid, actual_port}

      {:error, :eaddrinuse} = error ->
        Logger.error("Failed to start #{servers_name(endpoint, servers)}: port already in use")
        error

      {:error, _} = error ->
        error
    end
  end

  defp get_actual_port(pid, default_port) do
    case ThousandIsland.listener_info(pid) do
      {:ok, {_ip, actual_port}} -> actual_port
      _ -> default_port
    end
  end

  @doc """
  Return a child_spec to start server under a supervisor.
  """
  @spec child_spec(atom(), %{String.t() => [module()]}, non_neg_integer(), Keyword.t()) ::
          Supervisor.child_spec()
  def child_spec(endpoint, servers, port, opts) do
    server_opts = build_server_opts(endpoint, servers, port, opts)

    scheme = if cred_opts(opts), do: :https, else: :http

    Logger.info(
      "Starting #{servers_name(endpoint, servers)} with ThousandIsland using #{scheme}://0.0.0.0:#{port}"
    )

    server_name = servers_name(endpoint, servers)

    %{
      id: server_name,
      start: {ThousandIsland, :start_link, [server_opts]},
      type: :supervisor,
      restart: :permanent,
      shutdown: :infinity
    }
  end

  @impl true
  def stop(_endpoint, _servers) do
    # TODO: Implement proper shutdown of ThousandIsland server
    # ThousandIsland.stop(server_pid)
    :ok
  end

  @spec read_body(GRPC.Server.Adapter.state()) :: {:ok, binary()}
  def read_body(%{data: data}) do
    {:ok, data}
  end

  @spec reading_stream(GRPC.Server.Adapter.state()) :: Enumerable.t()
  def reading_stream(%{stream_state: %{bidi_stream_pid: bidi_pid}}) when not is_nil(bidi_pid) do
    # For bidi streaming, return the lazy stream from BidiStream
    GRPC.Server.BidiStream.to_enum(bidi_pid)
  end

  def reading_stream(%{data: data}) do
    # Create a stream that yields the data once
    Stream.unfold({data, false}, fn
      {_, true} ->
        nil

      {buffer, false} ->
        case GRPC.Message.get_message(buffer) do
          {message, rest} -> {message, {rest, false}}
          _ -> nil
        end
    end)
  end

  def set_headers(%{handler_pid: pid, stream_id: stream_id}, headers) do
    send(pid, {:grpc_accumulate_headers, stream_id, headers})
    :ok
  end

  def set_resp_trailers(%{handler_pid: pid, stream_id: stream_id}, trailers) do
    # Send message to accumulate trailers in handler state
    # They will be merged with final trailers when stream completes
    send(pid, {:grpc_accumulate_trailers, stream_id, trailers})
    :ok
  end

  def get_headers(%{headers: headers}) do
    headers
  end

  def get_headers(%{connection: connection}) do
    connection.metadata || %{}
  end

  def get_peer(%{socket: socket}) do
    case ThousandIsland.Socket.peername(socket) do
      {:ok, {address, port}} ->
        {:ok, {address, port}}

      error ->
        error
    end
  end

  def get_cert(%{socket: socket}) do
    case ThousandIsland.Socket.peercert(socket) do
      {:ok, cert} -> {:ok, cert}
      {:error, _} -> {:error, :no_peercert}
    end
  end

  def get_qs(_payload) do
    # Query string not applicable for gRPC
    ""
  end

  def get_bindings(_payload) do
    # Path bindings not applicable for gRPC
    %{}
  end

  def set_compressor(_payload, _compressor) do
    # Compressor will be stored in connection state
    :ok
  end

  @impl true
  def send_reply(%{handler_pid: pid, stream_id: stream_id}, data, opts) do
    # Encode message with gRPC framing (compressed flag + length + data)
    compressor = Keyword.get(opts, :compressor)
    codec = Keyword.get(opts, :codec)

    case GRPC.Message.to_data(data, compressor: compressor, codec: codec, iolist: true) do
      {:ok, framed_data, _size} ->
        # Send data frame - handler will send accumulated headers first if needed
        send(pid, {:grpc_send_data, stream_id, framed_data})
        :ok

      {:error, _msg} ->
        :ok
    end
  end

  # Fallback for non-streaming (shouldn't happen but keeps compatibility)
  def send_reply(_payload, _data, _opts), do: :ok

  @impl true
  def send_headers(%{handler_pid: pid, stream_id: stream_id}, headers) do
    # Send message to accumulate headers in handler state
    # They will be sent on first send_reply call
    send(pid, {:grpc_accumulate_headers, stream_id, headers})
    :ok
  end

  def send_headers(_payload, _headers), do: :ok

  def send_trailers(%{handler_pid: pid, stream_id: stream_id}, trailers) do
    send(pid, {:grpc_send_trailers, stream_id, trailers})
    :ok
  end

  defp build_server_opts(endpoint, servers, port, opts) do
    adapter_opts = Keyword.get(opts, :adapter_opts, opts)

    num_acceptors = Keyword.get(adapter_opts, :num_acceptors, @default_num_acceptors)
    num_connections = Keyword.get(adapter_opts, :num_connections, @default_max_connections)

    transport_opts =
      adapter_opts
      |> Keyword.get(:transport_options, [])
      |> Keyword.put(:port, port)
      |> maybe_add_ip(adapter_opts)
      |> maybe_add_ssl(cred_opts(opts))
      # Optimize TCP buffers for gRPC performance (support up to 1MB messages)
      # 1MB buffer for large messages
      |> Keyword.put_new(:buffer, 1_048_576)
      # 1MB receive buffer
      |> Keyword.put_new(:recbuf, 1_048_576)
      # 1MB send buffer
      |> Keyword.put_new(:sndbuf, 1_048_576)
      # Disable Nagle's algorithm for low latency
      |> Keyword.put_new(:nodelay, true)

    # Configure HTTP/2 settings for larger frames (needed for large gRPC messages)
    local_settings = [
      # 1MB window size for large payloads
      initial_window_size: 1_048_576,
      # Keep default max frame size
      max_frame_size: 16_384
    ]

    handler_options = %{
      endpoint: endpoint,
      servers: servers,
      opts: [local_settings: local_settings]
    }

    Logger.debug("[build_server_opts] Creating handler_options")

    [
      port: port,
      transport_module: transport_module(opts),
      transport_options: transport_opts,
      handler_module: Handler,
      handler_options: handler_options,
      num_acceptors: num_acceptors,
      num_connections: num_connections
    ]
  end

  defp maybe_add_ip(transport_opts, adapter_opts) do
    case Keyword.get(adapter_opts, :ip) do
      nil -> transport_opts
      ip -> Keyword.put(transport_opts, :ip, ip)
    end
  end

  defp maybe_add_ssl(transport_opts, nil), do: transport_opts

  defp maybe_add_ssl(transport_opts, cred_opts) do
    Keyword.merge(transport_opts, cred_opts.ssl)
  end

  defp transport_module(opts) do
    if cred_opts(opts) do
      ThousandIsland.Transports.SSL
    else
      ThousandIsland.Transports.TCP
    end
  end

  defp cred_opts(opts) do
    opts[:cred]
  end

  defp servers_name(nil, servers) do
    servers |> Map.values() |> Enum.map(fn s -> inspect(s) end) |> Enum.join(",")
  end

  defp servers_name(endpoint, _) do
    inspect(endpoint)
  end
end
