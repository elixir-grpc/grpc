defmodule GRPC.Server.HTTP2.StreamState do
  @moduledoc """
  Manages individual HTTP/2 stream state for gRPC requests.

  Each gRPC call is handled as a separate HTTP/2 stream. This module:
  - Decodes HTTP/2 headers into gRPC metadata
  - Accumulates DATA frames into gRPC messages
  - Handles 5-byte length-prefixed message framing
  - Manages stream lifecycle (HEADERS -> DATA -> trailers)
  """

  alias GRPC.HTTP2.Frame

  @type stream_id :: pos_integer()
  @type state :: :idle | :open | :half_closed_local | :half_closed_remote | :closed

  @type t :: %__MODULE__{
          stream_id: stream_id(),
          state: state(),
          # gRPC request info
          path: String.t() | nil,
          method: String.t() | nil,
          authority: String.t() | nil,
          content_type: String.t() | nil,
          metadata: map(),
          # Deadline (absolute time in microseconds when request expires)
          deadline: integer() | nil,
          # Message buffering
          data_buffer: binary(),
          message_buffer: [map()],
          # Flow control
          window_size: integer(),
          # Trailers
          trailers: map(),
          # Handler PID for streaming
          handler_pid: pid() | nil,
          # Custom headers/trailers set by handler
          custom_headers: map(),
          custom_trailers: map(),
          # Flag to track if response headers were sent
          headers_sent: boolean(),
          # Bidirectional streaming flag
          is_bidi_streaming: boolean(),
          # Flag to track if handler was already started (for bidi)
          handler_started: boolean(),
          # PID of the BidiStream task (for bidi streaming only)
          bidi_stream_pid: pid() | nil,
          # Codec, compressor, and RPC info for decoding subsequent messages
          codec: module() | nil,
          compressor: module() | nil,
          rpc: tuple() | nil,
          # Flag to track if client sent END_STREAM (stream half-closed remote)
          end_stream_received: boolean(),
          # Flag to track if we already sent an error response (prevent duplicates)
          error_sent: boolean()
        }

  defstruct stream_id: nil,
            state: :idle,
            path: nil,
            method: nil,
            authority: nil,
            content_type: nil,
            metadata: %{},
            data_buffer: <<>>,
            message_buffer: [],
            window_size: 65_535,
            trailers: %{},
            is_bidi_streaming: false,
            handler_started: false,
            bidi_stream_pid: nil,
            handler_pid: nil,
            custom_headers: %{},
            custom_trailers: %{},
            headers_sent: false,
            codec: nil,
            compressor: nil,
            rpc: nil,
            deadline: nil,
            end_stream_received: false,
            error_sent: false

  # Parses grpc-timeout header and converts to absolute deadline
  # Format: "1H" (hour), "1M" (minute), "1S" (second), "1m" (millisecond), "1u" (microsecond), "1n" (nanosecond)
  defp parse_timeout_to_deadline(nil), do: nil

  defp parse_timeout_to_deadline(timeout_str) do
    case Integer.parse(timeout_str) do
      {value, "H"} -> System.monotonic_time(:microsecond) + value * 3_600_000_000
      {value, "M"} -> System.monotonic_time(:microsecond) + value * 60_000_000
      {value, "S"} -> System.monotonic_time(:microsecond) + value * 1_000_000
      {value, "m"} -> System.monotonic_time(:microsecond) + value * 1_000
      {value, "u"} -> System.monotonic_time(:microsecond) + value
      {value, "n"} -> System.monotonic_time(:microsecond) + div(value, 1_000)
      _ -> nil
    end
  end

  @doc """
  Creates a new stream state.
  """
  @spec new(stream_id(), integer()) :: t()
  def new(stream_id, initial_window_size \\ 65_535) do
    %__MODULE__{
      stream_id: stream_id,
      state: :idle,
      window_size: initial_window_size
    }
  end

  @doc """
  Creates a stream state from decoded HTTP/2 headers.
  Extracts pseudo-headers and metadata from header list.
  """
  @spec from_headers(stream_id(), list({String.t(), String.t()}), integer()) :: t()
  def from_headers(stream_id, headers, initial_window_size \\ 65_535) do
    stream = new(stream_id, initial_window_size)

    {path, method, authority, content_type, timeout_str, metadata} =
      Enum.reduce(headers, {nil, nil, nil, nil, nil, %{}}, fn
        {":path", value}, {_, m, a, ct, t, meta} ->
          {value, m, a, ct, t, meta}

        {":method", value}, {p, _, a, ct, t, meta} ->
          {p, value, a, ct, t, meta}

        {":authority", value}, {p, m, _, ct, t, meta} ->
          {p, m, value, ct, t, meta}

        {"content-type", value}, {p, m, a, _, t, meta} ->
          {p, m, a, value, t, meta}

        {"grpc-timeout", value}, {p, m, a, ct, _, meta} ->
          {p, m, a, ct, value, meta}

        # Skip other pseudo-headers
        {":" <> _rest, _value}, acc ->
          acc

        {key, value}, {p, m, a, ct, t, meta} ->
          {p, m, a, ct, t, Map.put(meta, key, value)}
      end)

    # Parse timeout and calculate deadline
    deadline = parse_timeout_to_deadline(timeout_str)

    if timeout_str do
      require Logger

      Logger.info(
        "[StreamState] Parsed grpc-timeout: #{timeout_str} -> deadline: #{inspect(deadline)} (now: #{System.monotonic_time(:microsecond)})"
      )
    end

    %{
      stream
      | path: path,
        method: method,
        authority: authority,
        content_type: content_type || "application/grpc+proto",
        metadata: metadata,
        deadline: deadline,
        state: :open
    }
  end

  @doc """
  Checks if the stream's deadline has been exceeded.
  Returns true if the deadline exists and has passed, false otherwise.
  """
  @spec deadline_exceeded?(t()) :: boolean()
  def deadline_exceeded?(%__MODULE__{deadline: nil}), do: false

  def deadline_exceeded?(%__MODULE__{deadline: deadline}) do
    System.monotonic_time(:microsecond) > deadline
  end

  @doc """
  Processes HEADERS frame and extracts gRPC metadata.

  HTTP/2 pseudo-headers are decoded into gRPC request fields:
  - `:method` → "POST" (gRPC always uses POST)
  - `:path` → "/package.Service/Method"
  - `:authority` → "host:port"
  - `content-type` → "application/grpc+proto" etc

  Other headers become gRPC metadata.
  """
  @spec handle_headers(t(), Frame.Headers.t()) :: {:ok, t()} | {:error, term()}
  def handle_headers(stream, %Frame.Headers{} = headers) do
    case stream.state do
      :idle ->
        decode_headers(stream, headers)

      :half_closed_remote ->
        # Trailers received
        decode_trailers(stream, headers)

      _other ->
        {:error, :protocol_error}
    end
  end

  @doc """
  Processes DATA frame and accumulates gRPC messages.

  gRPC uses 5-byte length-prefixed framing:
  - Byte 0: Compression flag (0 = no compression, 1 = compressed)
  - Bytes 1-4: Message length (big-endian uint32)
  - Bytes 5+: Message payload

  Multiple messages can arrive in a single DATA frame or be split across frames.
  """
  @spec handle_data(t(), Frame.Data.t()) :: {:ok, t(), [map()]} | {:error, term()}
  def handle_data(stream, %Frame.Data{} = data) do
    case stream.state do
      :open ->
        process_data(stream, data)

      _other ->
        {:error, :stream_closed}
    end
  end

  @doc """
  Updates stream state based on end_stream flag.
  """
  @spec maybe_close_stream(t(), boolean()) :: t()
  def maybe_close_stream(stream, end_stream) do
    if end_stream do
      new_state =
        case stream.state do
          :open -> :half_closed_remote
          :half_closed_local -> :closed
          other -> other
        end

      %{stream | state: new_state}
    else
      stream
    end
  end

  @doc """
  Updates stream window size for flow control.
  """
  @spec update_window(t(), integer()) :: {:ok, t()} | {:error, :flow_control_error}
  def update_window(stream, increment) do
    case GRPC.HTTP2.FlowControl.update_window(stream.window_size, increment) do
      {:ok, new_size} ->
        {:ok, %{stream | window_size: new_size}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Checks if stream has enough window to send data.
  """
  @spec has_window?(t(), integer()) :: boolean()
  def has_window?(stream, size) do
    stream.window_size >= size
  end

  ## Private Functions

  defp decode_headers(stream, headers) do
    headers_map =
      Enum.reduce(headers.headers, %{}, fn {name, value}, acc ->
        Map.put(acc, name, value)
      end)

    # Extract pseudo-headers
    method = Map.get(headers_map, ":method")
    path = Map.get(headers_map, ":path")
    authority = Map.get(headers_map, ":authority")
    content_type = Map.get(headers_map, "content-type")

    # Validate gRPC request
    with :ok <- validate_method(method),
         :ok <- validate_path(path),
         :ok <- validate_content_type(content_type) do
      # Extract metadata (non-pseudo headers)
      metadata =
        headers_map
        |> Enum.filter(fn {name, _value} -> !String.starts_with?(name, ":") end)
        |> Enum.into(%{})

      stream = %{
        stream
        | state: :open,
          method: method,
          path: path,
          authority: authority,
          content_type: content_type,
          metadata: metadata
      }

      stream = maybe_close_stream(stream, headers.end_stream)

      {:ok, stream}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp decode_trailers(stream, headers) do
    trailers =
      Enum.reduce(headers.headers, %{}, fn {name, value}, acc ->
        Map.put(acc, name, value)
      end)

    stream = %{stream | trailers: trailers}
    stream = maybe_close_stream(stream, headers.end_stream)

    {:ok, stream}
  end

  defp validate_method("POST"), do: :ok
  defp validate_method(_), do: {:error, :invalid_method}

  defp validate_path("/" <> _rest), do: :ok
  defp validate_path(_), do: {:error, :invalid_path}

  defp validate_content_type("application/grpc" <> _), do: :ok
  defp validate_content_type(_), do: {:error, :invalid_content_type}

  defp process_data(stream, data) do
    # Append to buffer
    buffer = stream.data_buffer <> data.data

    # Extract complete messages
    {messages, remaining} = extract_messages(buffer, [])

    stream = %{
      stream
      | data_buffer: remaining,
        message_buffer: stream.message_buffer ++ messages
    }

    stream = maybe_close_stream(stream, data.end_stream)

    {:ok, stream, messages}
  end

  # Extract 5-byte length-prefixed messages
  defp extract_messages(
         <<compressed::8, length::32, payload::binary-size(length), rest::binary>>,
         acc
       ) do
    message = %{compressed: compressed == 1, data: payload}
    extract_messages(rest, acc ++ [message])
  end

  defp extract_messages(buffer, acc) do
    # Not enough data for a complete message
    {acc, buffer}
  end
end
