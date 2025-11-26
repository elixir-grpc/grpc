defmodule Mint.HTTP2 do
  @moduledoc """
  Process-less HTTP/2 client connection.

  This module provides a data structure that represents an HTTP/2 connection to
  a given server. The connection is represented as an opaque struct `%Mint.HTTP2{}`.
  The connection is a data structure and is not backed by a process, and all the
  connection handling happens in the process that creates the struct.

  This module and data structure work exactly like the ones described in the `Mint.HTTP`
  module, with the exception that `Mint.HTTP2` specifically deals with HTTP/2 while
  `Mint.HTTP` deals seamlessly with HTTP/1.1 and HTTP/2. For more information on
  how to use the data structure and client architecture, see `Mint.HTTP`.

  ## HTTP/2 Streams and Requests

  HTTP/2 introduces the concept of **streams**. A stream is an isolated conversation
  between the client and the server. Each stream is unique and identified by a unique
  **stream ID**, which means that there's no order when data comes on different streams
  since they can be identified uniquely. A stream closely corresponds to a request, so
  in this documentation and client we will mostly refer to streams as "requests".
  We mentioned data on streams can come in arbitrary order, and streams are requests,
  so the practical effect of this is that performing request A and then request B
  does not mean that the response to request A will come before the response to request B.
  This is why we identify each request with a unique reference returned by `request/5`.
  See `request/5` for more information.

  ## Closed Connection

  In HTTP/2, the connection can either be open, closed, or only closed for writing.
  When a connection is closed for writing, the client cannot send requests or stream
  body chunks, but it can still read data that the server might be sending. When the
  connection gets closed on the writing side, a `:server_closed_connection` error is
  returned. `{:error, request_ref, error}` is returned for requests that haven't been
  processed by the server, with the reason of `error` being `:unprocessed`.
  These requests are safe to retry.

  ## HTTP/2 Settings

  HTTP/2 supports settings negotiation between servers and clients. The server advertises
  its settings to the client and the client advertises its settings to the server. A peer
  (server or client) has to acknowledge the settings advertised by the other peer before
  those settings come into action (that's why it's called a negotiation).

  A first settings negotiation happens right when the connection starts.
  Servers and clients can renegotiate settings at any time during the life of the
  connection.

  Mint users don't need to care about settings acknowledgements directly since they're
  handled transparently by `stream/2`.

  To retrieve the server settings, you can use `get_server_setting/2`. Doing so is often
  useful to be able to tune your requests based on the server settings.

  To communicate client settings to the server, use `put_settings/2` or pass them when
  starting up a connection with `connect/4`. Note that the server needs to acknowledge
  the settings sent through `put_setting/2` before those settings come into effect. The
  server ack is processed transparently by `stream/2`, but this means that if you change
  a setting through `put_settings/2` and try to retrieve the value of that setting right
  after with `get_client_setting/2`, you'll likely get the old value of that setting. Once
  the server acknowledges the new settings, the updated value will be returned by
  `get_client_setting/2`.

  ## Server Push

  HTTP/2 supports [server push](https://en.wikipedia.org/wiki/HTTP/2_Server_Push), which
  is a way for a server to send a response to a client without the client needing to make
  the corresponding request. The server sends a `:push_promise` response to a normal request:
  this creates a new request reference. Then, the server sends normal responses for the newly
  created request reference.

  Let's see an example. We will ask the server for `"/index.html"` and the server will
  send us a push promise for `"/style.css"`.

      {:ok, conn} = Mint.HTTP2.connect(:https, "example.com", 443)
      {:ok, conn, request_ref} = Mint.HTTP2.request(conn, "GET", "/index.html", _headers = [], _body = "")

      next_message =
        receive do
          msg -> msg
        end

      {:ok, conn, responses} = Mint.HTTP2.stream(conn, next_message)

      [
        {:push_promise, ^request_ref, promised_request_ref, promised_headers},
        {:status, ^request_ref, 200},
        {:headers, ^request_ref, []},
        {:data, ^request_ref, "<html>..."},
        {:done, ^request_ref}
      ] = responses

      promised_headers
      #=> [{":method", "GET"}, {":path", "/style.css"}]

  As you can see in the example above, when the server sends a push promise then a
  `:push_promise` response is returned as a response to a request. The `:push_promise`
  response contains a `promised_request_ref` and some `promised_headers`. The
  `promised_request_ref` is the new request ref that pushed responses will be tagged with.
  `promised_headers` are headers that tell the client *what request* the promised response
  will respond to. The idea is that the server tells the client a request the client will
  want to make and then preemptively sends a response for that request. Promised headers
  will always include `:method`, `:path`, and `:authority`.

      next_message =
        receive do
          msg -> msg
        end

      {:ok, conn, responses} = Mint.HTTP2.stream(conn, next_message)

      [
        {:status, ^promised_request_ref, 200},
        {:headers, ^promised_request_ref, []},
        {:data, ^promised_request_ref, "body { ... }"},
        {:done, ^promised_request_ref}
      ]

  The response to a promised request is like a response to any normal request.

  > #### Disabling Server Pushes {: .tip}
  >
  > HTTP/2 exposes a boolean setting for enabling or disabling server pushes with `:enable_push`.
  > You can pass this option when connecting or in `put_settings/2`. By default server push
  > is enabled.
  """

  import Mint.HTTP2.Frame, except: [encode: 1, decode_next: 1, inspect: 1]

  alias Mint.{HTTPError, TransportError}
  alias Mint.Types
  alias Mint.Core.{Headers, Util}
  alias Mint.HTTP2.Frame

  require Logger
  require Integer

  @behaviour Mint.Core.Conn

  ## Constants

  @connection_preface "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
  @transport_opts [alpn_advertised_protocols: ["h2"]]

  @default_window_size 65_535
  @max_window_size 2_147_483_647

  @default_max_frame_size 16_384
  @valid_max_frame_size_range @default_max_frame_size..16_777_215

  @valid_client_settings [
    :max_concurrent_streams,
    :initial_window_size,
    :max_frame_size,
    :enable_push,
    :max_header_list_size
  ]

  @user_agent "mint/" <> Mix.Project.config()[:version]

  # HTTP/2 connection struct.
  defstruct [
    # Transport things.
    :transport,
    :socket,
    :mode,

    # Host things.
    :hostname,
    :port,
    :scheme,
    :authority,

    # Connection state (open, closed, and so on).
    :state,

    # Fields of the connection.
    buffer: "",
    window_size: @default_window_size,
    encode_table: HPAX.new(4096),
    decode_table: HPAX.new(4096),

    # Queue for sent PING frames.
    ping_queue: :queue.new(),

    # Queue for sent SETTINGS frames.
    client_settings_queue: :queue.new(),

    # Stream-set-related things.
    next_stream_id: 3,
    streams: %{},
    open_client_stream_count: 0,
    open_server_stream_count: 0,
    ref_to_stream_id: %{},

    # Settings that the server communicates to the client.
    server_settings: %{
      enable_push: true,
      max_concurrent_streams: 100,
      initial_window_size: @default_window_size,
      max_frame_size: @default_max_frame_size,
      max_header_list_size: :infinity,
      # Only supported by the server: https://www.rfc-editor.org/rfc/rfc8441.html#section-3
      enable_connect_protocol: false
    },

    # Settings that the client communicates to the server.
    client_settings: %{
      max_concurrent_streams: 100,
      initial_window_size: @default_window_size,
      max_header_list_size: :infinity,
      max_frame_size: @default_max_frame_size,
      enable_push: true
    },

    # Headers being processed (when headers are split into multiple frames with CONTINUATIONS, all
    # the continuation frames must come one right after the other).
    headers_being_processed: nil,

    # Stores the headers returned by the proxy in the `CONNECT` method
    proxy_headers: [],

    # Private store.
    private: %{},

    # Logging
    log: false
  ]

  defmacrop log(conn, level, message) do
    quote do
      conn = unquote(conn)

      if conn.log do
        Logger.log(unquote(level), unquote(message))
      else
        :ok
      end
    end
  end

  ## Types

  @typedoc """
  HTTP/2 setting with its value.

  This type represents both server settings as well as client settings. To retrieve
  server settings use `get_server_setting/2` and to retrieve client settings use
  `get_client_setting/2`. To send client settings to the server, see `put_settings/2`.

  The supported settings are the following:

    * `:header_table_size` - corresponds to `SETTINGS_HEADER_TABLE_SIZE`.

    * `:enable_push` - corresponds to `SETTINGS_ENABLE_PUSH`. Sets whether
      push promises are supported. If you don't want to support push promises,
      use `put_settings/2` to tell the server that your client doesn't want push promises.

    * `:max_concurrent_streams` - corresponds to `SETTINGS_MAX_CONCURRENT_STREAMS`.
      Tells what is the maximum number of streams that the peer sending this (client or server)
      supports. As mentioned in the module documentation, HTTP/2 streams are equivalent to
      requests, so knowing the maximum number of streams that the server supports can be useful
      to know how many concurrent requests can be open at any time. Use `get_server_setting/2`
      to find out how many concurrent streams the server supports.

    * `:initial_window_size` -  corresponds to `SETTINGS_INITIAL_WINDOW_SIZE`.
      Tells what is the value of the initial HTTP/2 window size for the peer
      that sends this setting.

    * `:max_frame_size` - corresponds to `SETTINGS_MAX_FRAME_SIZE`. Tells what is the
      maximum size of an HTTP/2 frame for the peer that sends this setting.

    * `:max_header_list_size` - corresponds to `SETTINGS_MAX_HEADER_LIST_SIZE`.

    * `:enable_connect_protocol` - corresponds to `SETTINGS_ENABLE_CONNECT_PROTOCOL`.
      Sets whether the client may invoke the extended connect protocol which is used to
      bootstrap WebSocket connections.

  """
  @type setting() ::
          {:enable_push, boolean()}
          | {:header_table_size, non_neg_integer()}
          | {:max_concurrent_streams, pos_integer()}
          | {:initial_window_size, 1..2_147_483_647}
          | {:max_frame_size, 16_384..16_777_215}
          | {:max_header_list_size, :infinity | pos_integer()}
          | {:enable_connect_protocol, boolean()}

  @typedoc """
  HTTP/2 settings.

  See `t:setting/0`.
  """
  @type settings() :: [setting()]

  @typedoc """
  An HTTP/2-specific error reason.

  The values can be:

    * `:closed` - when you try to make a request or stream a body chunk but the connection
      is closed.

    * `:closed_for_writing` - when you try to make a request or stream a body chunk but
      the connection is closed for writing. This means you cannot issue any more requests.
      See the "Closed connection" section in the module documentation for more information.

    * `:too_many_concurrent_requests` - when the maximum number of concurrent requests
      allowed by the server is reached. To find out what this limit is, use `get_setting/2`
      with the `:max_concurrent_streams` setting name.

    * `{:max_header_list_size_exceeded, size, max_size}` - when the maximum size of
      the header list is reached. `size` is the actual value of the header list size,
      `max_size` is the maximum value allowed. See `get_setting/2` to retrieve the
      value of the max size.

    * `{:exceeds_window_size, what, window_size}` - when the data you're trying to send
      exceeds the window size of the connection (if `what` is `:connection`) or of a request
      (if `what` is `:request`). `window_size` is the allowed window size. See
      `get_window_size/2`.

    * `{:stream_not_found, stream_id}` - when the given request is not found.

    * `:unknown_request_to_stream` - when you're trying to stream data on an unknown
      request.

    * `:request_is_not_streaming` - when you try to send data (with `stream_request_body/3`)
      on a request that is not open for streaming.

    * `:unprocessed` - when a request was closed because it was not processed by the server.
      When this error is returned, it means that the server hasn't processed the request at all,
      so it's safe to retry the given request on a different or new connection.

    * `{:server_closed_request, error_code}` - when the server closes the request.
      `error_code` is the reason why the request was closed.

    * `{:server_closed_connection, reason, debug_data}` - when the server closes the connection
      gracefully or because of an error. In HTTP/2, this corresponds to a `GOAWAY` frame.
      `error` is the reason why the connection was closed. `debug_data` is additional debug data.

    * `{:frame_size_error, frame}` - when there's an error with the size of a frame.
      `frame` is the frame type, such as `:settings` or `:window_update`.

    * `{:protocol_error, debug_data}` - when there's a protocol error.
      `debug_data` is a string that explains the nature of the error.

    * `{:compression_error, debug_data}` - when there's a header compression error.
      `debug_data` is a string that explains the nature of the error.

    * `{:flow_control_error, debug_data}` - when there's a flow control error.
      `debug_data` is a string that explains the nature of the error.

  """
  @type error_reason() :: term()

  @typedoc """
  A Mint HTTP/2 connection struct.

  The struct's fields are private.
  """
  @opaque t() :: %__MODULE__{}

  ## Public interface

  @doc """
  Same as `Mint.HTTP.connect/4`, but forces a HTTP/2 connection.
  """
  @spec connect(Types.scheme(), Types.address(), :inet.port_number(), keyword()) ::
          {:ok, t()} | {:error, Types.error()}
  def connect(scheme, address, port, opts \\ []) do
    hostname = Mint.Core.Util.hostname(opts, address)

    transport_opts =
      opts
      |> Keyword.get(:transport_opts, [])
      |> Keyword.merge(@transport_opts)
      |> Keyword.put(:hostname, hostname)

    case negotiate(address, port, scheme, transport_opts) do
      {:ok, socket} ->
        initiate(scheme, socket, hostname, port, opts)

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc false
  @spec upgrade(
          Types.scheme(),
          Mint.Types.socket(),
          Types.scheme(),
          String.t(),
          :inet.port_number(),
          keyword()
        ) :: {:ok, t()} | {:error, Types.error()}
  def upgrade(old_scheme, socket, new_scheme, hostname, port, opts) do
    transport = Util.scheme_to_transport(new_scheme)

    transport_opts =
      opts
      |> Keyword.get(:transport_opts, [])
      |> Keyword.merge(@transport_opts)

    with {:ok, socket} <- transport.upgrade(socket, old_scheme, hostname, port, transport_opts) do
      initiate(new_scheme, socket, hostname, port, opts)
    end
  end

  @doc """
  See `Mint.HTTP.close/1`.
  """
  @impl true
  @spec close(t()) :: {:ok, t()}
  def close(conn)

  def close(%__MODULE__{state: :open} = conn) do
    send_connection_error!(conn, :no_error, "connection peacefully closed by client")
  catch
    {:mint, conn, %HTTPError{reason: {:no_error, _}}} ->
      {:ok, conn}
  end

  def close(%__MODULE__{state: {:goaway, _error_code, _debug_data}} = conn) do
    _ = conn.transport.close(conn.socket)
    {:ok, put_in(conn.state, :closed)}
  end

  def close(%__MODULE__{state: :handshaking} = conn) do
    _ = conn.transport.close(conn.socket)
    {:ok, put_in(conn.state, :closed)}
  end

  def close(%__MODULE__{state: :closed} = conn) do
    {:ok, conn}
  end

  @doc """
  See `Mint.HTTP.open?/1`.
  """
  @impl true
  @spec open?(t(), :read | :write) :: boolean()
  def open?(%__MODULE__{state: state} = _conn, type \\ :write)
      when type in [:read, :write, :read_write] do
    case state do
      :handshaking -> true
      :open -> true
      {:goaway, _error_code, _debug_data} -> type == :read
      :closed -> false
    end
  end

  @doc """
  See `Mint.HTTP.request/5`.

  In HTTP/2, opening a request means opening a new HTTP/2 stream (see the
  module documentation). This means that a request could fail because the
  maximum number of concurrent streams allowed by the server has been reached.
  In that case, the error reason `:too_many_concurrent_requests` is returned.
  If you want to avoid incurring in this error, you can retrieve the value of
  the maximum number of concurrent streams supported by the server through
  `get_server_setting/2` (passing in the `:max_concurrent_streams` setting name).

  ## Header list size

  In HTTP/2, the server can optionally specify a maximum header list size that
  the client needs to respect when sending headers. The header list size is calculated
  by summing the length (in bytes) of each header name plus value, plus 32 bytes for
  each header. Note that pseudo-headers (like `:path` or `:method`) count towards
  this size. If the size is exceeded, an error is returned. To check what the size
  is, use `get_server_setting/2`.

  ## Request body size

  If the request body size will exceed the window size of the HTTP/2 stream created by the
  request or the window size of the connection Mint will return a `:exceeds_window_size`
  error.

  To ensure you do not exceed the window size it is recommended to stream the request
  body by initially passing `:stream` as the body and sending the body in chunks using
  `stream_request_body/3` and using `get_window_size/2` to get the window size of the
  request and connection.
  """
  @impl true
  @spec request(
          t(),
          method :: String.t(),
          path :: String.t(),
          Types.headers(),
          body :: iodata() | nil | :stream
        ) ::
          {:ok, t(), Types.request_ref()}
          | {:error, t(), Types.error()}
  def request(conn, method, path, headers, body)

  def request(%__MODULE__{state: :closed} = conn, _method, _path, _headers, _body) do
    {:error, conn, wrap_error(:closed)}
  end

  def request(
        %__MODULE__{state: {:goaway, _error_code, _debug_data}} = conn,
        _method,
        _path,
        _headers,
        _body
      ) do
    {:error, conn, wrap_error(:closed_for_writing)}
  end

  def request(%__MODULE__{} = conn, method, path, headers, body)
      when is_binary(method) and is_binary(path) and is_list(headers) do
    headers =
      headers
      |> Headers.lower_raws()
      |> add_pseudo_headers(conn, method, path)
      |> add_default_headers(body)
      |> sort_pseudo_headers_to_front()

    {conn, stream_id, ref} = open_stream(conn)
    {conn, payload} = encode_request_payload(conn, stream_id, headers, body)
    conn = send!(conn, payload)
    {:ok, conn, ref}
  catch
    :throw, {:mint, _conn, reason} ->
      # The stream is invalid and "_conn" may be tracking it, so we return the original connection instead.
      {:error, conn, reason}
  end

  @doc """
  See `Mint.HTTP.stream_request_body/3`.
  """
  @impl true
  @spec stream_request_body(
          t(),
          Types.request_ref(),
          iodata() | :eof | {:eof, trailer_headers :: Types.headers()}
        ) :: {:ok, t()} | {:error, t(), Types.error()}
  def stream_request_body(conn, request_ref, chunk)

  def stream_request_body(%__MODULE__{state: :closed} = conn, _request_ref, _chunk) do
    {:error, conn, wrap_error(:closed)}
  end

  def stream_request_body(
        %__MODULE__{state: {:goaway, _error_code, _debug_data}} = conn,
        _request_ref,
        _chunk
      ) do
    {:error, conn, wrap_error(:closed_for_writing)}
  end

  def stream_request_body(%__MODULE__{} = conn, request_ref, chunk)
      when is_reference(request_ref) do
    case Map.fetch(conn.ref_to_stream_id, request_ref) do
      {:ok, stream_id} ->
        {conn, payload} = encode_stream_body_request_payload(conn, stream_id, chunk)
        conn = send!(conn, payload)
        {:ok, conn}

      :error ->
        {:error, conn, wrap_error(:unknown_request_to_stream)}
    end
  catch
    :throw, {:mint, _conn, reason} ->
      # The stream is invalid and "_conn" may be tracking it, so we return the original connection instead.
      {:error, conn, reason}
  end

  @doc """
  Pings the server.

  This function is specific to HTTP/2 connections. It sends a **ping** request to
  the server `conn` is connected to. A `{:ok, conn, request_ref}` tuple is returned,
  where `conn` is the updated connection and `request_ref` is a unique reference that
  identifies this ping request. The response to a ping request is returned by `stream/2`
  as a `{:pong, request_ref}` tuple. If there's an error, this function returns
  `{:error, conn, reason}` where `conn` is the updated connection and `reason` is the
  error reason.

  `payload` must be an 8-byte binary with arbitrary content. When the server responds to
  a ping request, it will use that same payload. By default, the payload is an 8-byte
  binary with all bits set to `0`.

  Pinging can be used to measure the latency with the server and to ensure the connection
  is alive and well.

  ## Examples

      {:ok, conn, ref} = Mint.HTTP2.ping(conn)

  """
  @spec ping(t(), <<_::8>>) :: {:ok, t(), Types.request_ref()} | {:error, t(), Types.error()}
  def ping(%__MODULE__{} = conn, payload \\ :binary.copy(<<0>>, 8))
      when byte_size(payload) == 8 do
    {conn, ref} = send_ping(conn, payload)
    {:ok, conn, ref}
  catch
    :throw, {:mint, conn, error} -> {:error, conn, error}
  end

  @doc """
  Communicates the given **client settings** to the server.

  This function is HTTP/2-specific.

  This function takes a connection and a keyword list of HTTP/2 settings and sends
  the values of those settings to the server. The settings won't be effective until
  the server acknowledges them, which will be handled transparently by `stream/2`.

  This function returns `{:ok, conn}` when sending the settings to the server is
  successful, with `conn` being the updated connection. If there's an error, this
  function returns `{:error, conn, reason}` with `conn` being the updated connection
  and `reason` being the reason of the error.

  ## Supported Settings

  See `t:setting/0` for the supported settings. You can see the meaning
  of these settings [in the corresponding section in the HTTP/2
  RFC](https://httpwg.org/specs/rfc7540.html#SettingValues).

  See the "HTTP/2 settings" section in the module documentation for more information.

  ## Examples

      {:ok, conn} = Mint.HTTP2.put_settings(conn, max_frame_size: 100)

  """
  @spec put_settings(t(), settings()) :: {:ok, t()} | {:error, t(), Types.error()}
  def put_settings(%__MODULE__{} = conn, settings) when is_list(settings) do
    conn = send_settings(conn, settings)
    {:ok, conn}
  catch
    :throw, {:mint, conn, error} -> {:error, conn, error}
  end

  @doc """
  Gets the value of the given HTTP/2 server settings.

  This function returns the value of the given HTTP/2 setting that the server
  advertised to the client. This function is HTTP/2 specific.
  For more information on HTTP/2 settings, see [the related section in
  the RFC](https://httpwg.org/specs/rfc7540.html#SettingValues).

  See the "HTTP/2 settings" section in the module documentation for more information.

  ## Supported settings

  The possible settings that can be retrieved are described in `t:setting/0`.
  Any other atom passed as `name` will raise an error.

  ## Examples

      Mint.HTTP2.get_server_setting(conn, :max_concurrent_streams)
      #=> 500

  """
  @spec get_server_setting(t(), atom()) :: term()
  def get_server_setting(%__MODULE__{} = conn, name) when is_atom(name) do
    get_setting(conn.server_settings, name)
  end

  @doc """
  Gets the value of the given HTTP/2 client setting.

  This function returns the value of the given HTTP/2 setting that the client
  advertised to the server. Client settings can be advertised through `put_settings/2`
  or when starting up a connection.

  Client settings have to be acknowledged by the server before coming into effect.

  This function is HTTP/2 specific. For more information on HTTP/2 settings, see
  [the related section in the RFC](https://httpwg.org/specs/rfc7540.html#SettingValues).

  See the "HTTP/2 settings" section in the module documentation for more information.

  ## Supported settings

  The possible settings that can be retrieved are described in `t:setting/0`.
  Any other atom passed as `name` will raise an error.

  ## Examples

      Mint.HTTP2.get_client_setting(conn, :max_concurrent_streams)
      #=> 500

  """
  @spec get_client_setting(t(), atom()) :: term()
  def get_client_setting(%__MODULE__{} = conn, name) when is_atom(name) do
    get_setting(conn.client_settings, name)
  end

  defp get_setting(settings, name) do
    case Map.fetch(settings, name) do
      {:ok, value} -> value
      :error -> raise ArgumentError, "unknown HTTP/2 setting: #{inspect(name)}"
    end
  end

  @doc """
  Cancels an in-flight request.

  This function is HTTP/2 specific. It cancels an in-flight request. The server could have
  already sent responses for the request you want to cancel: those responses will be parsed
  by the connection but not returned to the user. No more responses
  to a request will be returned after you call `cancel_request/2` on that request.

  If there's no error in canceling the request, `{:ok, conn}` is returned where `conn` is
  the updated connection. If there's an error, `{:error, conn, reason}` is returned where
  `conn` is the updated connection and `reason` is the error reason.

  ## Examples

      {:ok, conn, ref} = Mint.HTTP2.request(conn, "GET", "/", _headers = [])
      {:ok, conn} = Mint.HTTP2.cancel_request(conn, ref)

  """
  @spec cancel_request(t(), Types.request_ref()) :: {:ok, t()} | {:error, t(), Types.error()}
  def cancel_request(%__MODULE__{} = conn, request_ref) when is_reference(request_ref) do
    case Map.fetch(conn.ref_to_stream_id, request_ref) do
      {:ok, stream_id} ->
        conn = close_stream!(conn, stream_id, _error_code = :cancel)
        {:ok, conn}

      :error ->
        {:ok, conn}
    end
  catch
    :throw, {:mint, conn, error} -> {:error, conn, error}
  end

  @doc """
  Returns the window size of the connection or of a single request.

  This function is HTTP/2 specific. It returns the window size of
  either the connection if `connection_or_request` is `:connection` or of a single
  request if `connection_or_request` is `{:request, request_ref}`.

  Use this function to check the window size of the connection before sending a
  full request. Also use this function to check the window size of both the
  connection and of a request if you want to stream body chunks on that request.

  For more information on flow control and window sizes in HTTP/2, see the section
  below.

  ## HTTP/2 Flow Control

  In HTTP/2, flow control is implemented through a
  window size. When the client sends data to the server, the window size is decreased
  and the server needs to "refill" it on the client side. You don't need to take care of
  the refilling of the client window as it happens behind the scenes in `stream/2`.

  A window size is kept for the entire connection and all requests affect this window
  size. A window size is also kept per request.

  The only thing that affects the window size is the body of a request, regardless of
  if it's a full request sent with `request/5` or body chunks sent through
  `stream_request_body/3`. That means that if we make a request with a body that is
  five bytes long, like `"hello"`, the window size of the connection and the window size
  of that particular request will decrease by five bytes.

  If we use all the window size before the server refills it, functions like
  `request/5` will return an error.

  ## Examples

  On the connection:

      HTTP2.get_window_size(conn, :connection)
      #=> 65_536

  On a single streamed request:

      {:ok, conn, request_ref} = HTTP2.request(conn, "GET", "/", [], :stream)
      HTTP2.get_window_size(conn, {:request, request_ref})
      #=> 65_536

      {:ok, conn} = HTTP2.stream_request_body(conn, request_ref, "hello")
      HTTP2.get_window_size(conn, {:request, request_ref})
      #=> 65_531

  """
  @spec get_window_size(t(), :connection | {:request, Types.request_ref()}) :: non_neg_integer()
  def get_window_size(conn, connection_or_request)

  def get_window_size(%__MODULE__{} = conn, :connection) do
    conn.window_size
  end

  def get_window_size(%__MODULE__{} = conn, {:request, request_ref}) do
    case Map.fetch(conn.ref_to_stream_id, request_ref) do
      {:ok, stream_id} ->
        conn.streams[stream_id].window_size

      :error ->
        raise ArgumentError,
              "request with request reference #{inspect(request_ref)} was not found"
    end
  end

  @doc """
  See `Mint.HTTP.stream/2`.
  """
  @impl true
  @spec stream(t(), term()) ::
          {:ok, t(), [Types.response()]}
          | {:error, t(), Types.error(), [Types.response()]}
          | :unknown
  def stream(conn, message)

  def stream(%__MODULE__{socket: socket} = conn, {tag, socket, reason})
      when tag in [:tcp_error, :ssl_error] do
    error = conn.transport.wrap_error(reason)
    {:error, %{conn | state: :closed}, error, _responses = []}
  end

  def stream(%__MODULE__{socket: socket} = conn, {tag, socket})
      when tag in [:tcp_closed, :ssl_closed] do
    handle_closed(conn)
  end

  def stream(%__MODULE__{transport: transport, socket: socket} = conn, {tag, socket, data})
      when tag in [:tcp, :ssl] do
    case maybe_concat_and_handle_new_data(conn, data) do
      {:ok, %{mode: mode, state: state} = conn, responses}
      when mode == :active and state != :closed ->
        case transport.setopts(socket, active: :once) do
          :ok -> {:ok, conn, responses}
          {:error, reason} -> {:error, put_in(conn.state, :closed), reason, responses}
        end

      other ->
        other
    end
  catch
    :throw, {:mint, conn, error, responses} -> {:error, conn, error, responses}
  end

  def stream(%__MODULE__{}, _message) do
    :unknown
  end

  @doc """
  See `Mint.HTTP.open_request_count/1`.

  In HTTP/2, the number of open requests is the number of requests **opened by the client**
  that have not yet received a `:done` response. It's important to note that only
  requests opened by the client (with `request/5`) count towards the number of open
  requests, as requests opened from the server with server pushes (see the "Server push"
  section in the module documentation) are not considered open requests. We do this because
  clients might need to know how many open requests there are because the server limits
  the number of concurrent requests the client can open. To know how many requests the client
  can open, see `get_server_setting/2` with the `:max_concurrent_streams` setting.
  """
  @impl true
  @spec open_request_count(t()) :: non_neg_integer()
  def open_request_count(%__MODULE__{} = conn) do
    conn.open_client_stream_count
  end

  @doc """
  See `Mint.HTTP.recv/3`.
  """
  @impl true
  @spec recv(t(), non_neg_integer(), timeout()) ::
          {:ok, t(), [Types.response()]}
          | {:error, t(), Types.error(), [Types.response()]}
  def recv(conn, byte_count, timeout)

  def recv(%__MODULE__{mode: :passive} = conn, byte_count, timeout) do
    case conn.transport.recv(conn.socket, byte_count, timeout) do
      {:ok, data} ->
        maybe_concat_and_handle_new_data(conn, data)

      {:error, %TransportError{reason: :closed}} ->
        handle_closed(conn)

      {:error, error} ->
        {:error, %{conn | state: :closed}, error, _responses = []}
    end
  catch
    :throw, {:mint, conn, error, responses} -> {:error, conn, error, responses}
  end

  def recv(_conn, _byte_count, _timeout) do
    raise ArgumentError,
          "can't use recv/3 to synchronously receive data when the mode is :active. " <>
            "Use Mint.HTTP.set_mode/2 to set the connection to passive mode"
  end

  @doc """
  See `Mint.HTTP.set_mode/2`.
  """
  @impl true
  @spec set_mode(t(), :active | :passive) :: {:ok, t()} | {:error, Types.error()}
  def set_mode(%__MODULE__{} = conn, mode) when mode in [:active, :passive] do
    active =
      case mode do
        :active -> :once
        :passive -> false
      end

    with :ok <- conn.transport.setopts(conn.socket, active: active) do
      {:ok, put_in(conn.mode, mode)}
    end
  end

  @doc """
  See `Mint.HTTP.controlling_process/2`.
  """
  @impl true
  @spec controlling_process(t(), pid()) :: {:ok, t()} | {:error, Types.error()}
  def controlling_process(%__MODULE__{} = conn, new_pid) when is_pid(new_pid) do
    with :ok <- conn.transport.controlling_process(conn.socket, new_pid) do
      {:ok, conn}
    end
  end

  @doc """
  See `Mint.HTTP.put_private/3`.
  """
  @impl true
  @spec put_private(t(), atom(), term()) :: t()
  def put_private(%__MODULE__{private: private} = conn, key, value) when is_atom(key) do
    %{conn | private: Map.put(private, key, value)}
  end

  @doc """
  See `Mint.HTTP.get_private/3`.
  """
  @impl true
  @spec get_private(t(), atom(), term()) :: term()
  def get_private(%__MODULE__{private: private} = _conn, key, default \\ nil) when is_atom(key) do
    Map.get(private, key, default)
  end

  @doc """
  See `Mint.HTTP.delete_private/2`.
  """
  @impl true
  @spec delete_private(t(), atom()) :: t()
  def delete_private(%__MODULE__{private: private} = conn, key) when is_atom(key) do
    %{conn | private: Map.delete(private, key)}
  end

  @doc """
  See `Mint.HTTP.put_log/2`.
  """
  @doc since: "1.5.0"
  @impl true
  @spec put_log(t(), boolean()) :: t()
  def put_log(%__MODULE__{} = conn, log?) when is_boolean(log?) do
    %{conn | log: log?}
  end

  # http://httpwg.org/specs/rfc7540.html#rfc.section.6.5
  # SETTINGS parameters are not negotiated. We keep client settings and server settings separate.
  @doc false
  @impl true
  @spec initiate(
          Types.scheme(),
          Types.socket(),
          String.t(),
          :inet.port_number(),
          keyword()
        ) :: {:ok, t()} | {:error, Types.error()}
  def initiate(scheme, socket, hostname, port, opts) do
    transport = Util.scheme_to_transport(scheme)
    scheme_string = Atom.to_string(scheme)
    mode = Keyword.get(opts, :mode, :active)
    log? = Keyword.get(opts, :log, false)
    client_settings_params = Keyword.get(opts, :client_settings, [])
    validate_client_settings!(client_settings_params)
    # If the port is the default for the scheme, don't add it to the :authority pseudo-header
    authority =
      if URI.default_port(scheme_string) == port do
        hostname
      else
        "#{hostname}:#{port}"
      end

    unless mode in [:active, :passive] do
      raise ArgumentError,
            "the :mode option must be either :active or :passive, got: #{inspect(mode)}"
    end

    unless is_boolean(log?) do
      raise ArgumentError,
            "the :log option must be a boolean, got: #{inspect(log?)}"
    end

    conn = %__MODULE__{
      hostname: hostname,
      port: port,
      authority: authority,
      transport: Util.scheme_to_transport(scheme),
      socket: socket,
      mode: mode,
      scheme: scheme_string,
      state: :handshaking,
      log: log?
    }

    with :ok <- Util.inet_opts(transport, socket),
         client_settings = settings(stream_id: 0, params: client_settings_params),
         preface = [@connection_preface, Frame.encode(client_settings)],
         :ok <- transport.send(socket, preface),
         conn = update_in(conn.client_settings_queue, &:queue.in(client_settings_params, &1)),
         conn = put_in(conn.socket, socket),
         :ok <- if(mode == :active, do: transport.setopts(socket, active: :once), else: :ok) do
      {:ok, conn}
    else
      error ->
        transport.close(socket)
        error
    end
  end

  @doc """
  See `Mint.HTTP.get_socket/1`.
  """
  @impl true
  @spec get_socket(t()) :: Mint.Types.socket()
  def get_socket(%__MODULE__{socket: socket} = _conn) do
    socket
  end

  @doc """
  See `Mint.HTTP.get_proxy_headers/1`.
  """
  @doc since: "1.4.0"
  @impl true
  @spec get_proxy_headers(t()) :: Mint.Types.headers()
  def get_proxy_headers(%__MODULE__{proxy_headers: proxy_headers} = _conn), do: proxy_headers

  # Made public since the %Mint.HTTP2{} struct is opaque.
  @doc false
  @impl true
  def put_proxy_headers(%__MODULE__{} = conn, headers) when is_list(headers) do
    %{conn | proxy_headers: headers}
  end

  ## Helpers

  defp handle_closed(conn) do
    conn = put_in(conn.state, :closed)

    if conn.open_client_stream_count > 0 or conn.open_server_stream_count > 0 do
      error = conn.transport.wrap_error(:closed)
      {:error, conn, error, _responses = []}
    else
      {:ok, conn, _responses = []}
    end
  end

  defp negotiate(address, port, :http, transport_opts) do
    # We don't support protocol negotiation for TCP connections
    # so currently we just assume the HTTP/2 protocol
    transport = Util.scheme_to_transport(:http)
    transport.connect(address, port, transport_opts)
  end

  defp negotiate(address, port, :https, transport_opts) do
    transport = Util.scheme_to_transport(:https)

    with {:ok, socket} <- transport.connect(address, port, transport_opts),
         {:ok, protocol} <- transport.negotiated_protocol(socket) do
      if protocol == "h2" do
        {:ok, socket}
      else
        {:error, transport.wrap_error({:bad_alpn_protocol, protocol})}
      end
    end
  end

  defp open_stream(conn) do
    max_concurrent_streams = conn.server_settings.max_concurrent_streams

    if conn.open_client_stream_count >= max_concurrent_streams do
      throw({:mint, conn, wrap_error(:too_many_concurrent_requests)})
    end

    stream = %{
      id: conn.next_stream_id,
      ref: make_ref(),
      state: :idle,
      window_size: conn.server_settings.initial_window_size,
      received_first_headers?: false
    }

    conn = put_in(conn.streams[stream.id], stream)
    conn = put_in(conn.ref_to_stream_id[stream.ref], stream.id)
    conn = update_in(conn.next_stream_id, &(&1 + 2))
    {conn, stream.id, stream.ref}
  end

  defp encode_stream_body_request_payload(conn, stream_id, :eof) do
    encode_data(conn, stream_id, "", [:end_stream])
  end

  defp encode_stream_body_request_payload(conn, stream_id, {:eof, trailers}) do
    trailers = Headers.from_raw(trailers)

    if unallowed_trailer_header = Headers.find_unallowed_trailer(trailers) do
      error = wrap_error({:unallowed_trailing_header, unallowed_trailer_header})
      throw({:mint, conn, error})
    end

    trailer_headers = Headers.to_raw(trailers, _case_sensitive = false)
    encode_headers(conn, stream_id, trailer_headers, [:end_headers, :end_stream])
  end

  defp encode_stream_body_request_payload(conn, stream_id, iodata) do
    encode_data(conn, stream_id, iodata, [])
  end

  defp encode_request_payload(conn, stream_id, headers, :stream) do
    encode_headers(conn, stream_id, headers, [:end_headers])
  end

  defp encode_request_payload(conn, stream_id, headers, nil) do
    encode_headers(conn, stream_id, headers, [:end_stream, :end_headers])
  end

  defp encode_request_payload(conn, stream_id, headers, iodata) do
    {conn, headers_payload} = encode_headers(conn, stream_id, headers, [:end_headers])
    {conn, data_payload} = encode_data(conn, stream_id, iodata, [:end_stream])
    {conn, [headers_payload, data_payload]}
  end

  defp encode_headers(conn, stream_id, headers, enabled_flags) do
    assert_headers_smaller_than_max_header_list_size(conn, headers)

    headers = Enum.map(headers, fn {name, value} -> {:store_name, name, value} end)
    {hbf, conn} = get_and_update_in(conn.encode_table, &HPAX.encode(headers, &1))

    payload = headers_to_encoded_frames(conn, stream_id, hbf, enabled_flags)

    stream_state = if :end_stream in enabled_flags, do: :half_closed_local, else: :open

    conn = put_in(conn.streams[stream_id].state, stream_state)
    conn = update_in(conn.open_client_stream_count, &(&1 + 1))

    {conn, payload}
  end

  defp assert_headers_smaller_than_max_header_list_size(
         %{server_settings: %{max_header_list_size: :infinity}},
         _headers
       ) do
    :ok
  end

  defp assert_headers_smaller_than_max_header_list_size(conn, headers) do
    # The value is based on the uncompressed size of header fields, including the length
    # of the name and value in octets plus an overhead of 32 octets for each header field.
    total_size =
      Enum.reduce(headers, 0, fn {name, value}, acc ->
        acc + byte_size(name) + byte_size(value) + 32
      end)

    max_header_list_size = conn.server_settings.max_header_list_size

    if total_size <= max_header_list_size do
      :ok
    else
      error = wrap_error({:max_header_list_size_exceeded, total_size, max_header_list_size})
      throw({:mint, conn, error})
    end
  end

  defp headers_to_encoded_frames(conn, stream_id, hbf, enabled_flags) do
    if IO.iodata_length(hbf) > conn.server_settings.max_frame_size do
      hbf
      |> IO.iodata_to_binary()
      |> split_payload_in_chunks(conn.server_settings.max_frame_size)
      |> split_hbf_to_encoded_frames(stream_id, enabled_flags)
    else
      Frame.encode(
        headers(stream_id: stream_id, hbf: hbf, flags: set_flags(:headers, enabled_flags))
      )
    end
  end

  defp split_hbf_to_encoded_frames({[first_chunk | chunks], last_chunk}, stream_id, enabled_flags) do
    flags = set_flags(:headers, enabled_flags -- [:end_headers])
    first_frame = Frame.encode(headers(stream_id: stream_id, hbf: first_chunk, flags: flags))

    middle_frames =
      Enum.map(chunks, fn chunk ->
        Frame.encode(continuation(stream_id: stream_id, hbf: chunk))
      end)

    flags =
      if :end_headers in enabled_flags do
        set_flags(:continuation, [:end_headers])
      else
        set_flags(:continuation, [])
      end

    last_frame = Frame.encode(continuation(stream_id: stream_id, hbf: last_chunk, flags: flags))

    [first_frame, middle_frames, last_frame]
  end

  defp encode_data(conn, stream_id, data, enabled_flags) do
    stream = fetch_stream!(conn, stream_id)

    if stream.state != :open do
      error = wrap_error(:request_is_not_streaming)
      throw({:mint, conn, error})
    end

    data_size = IO.iodata_length(data)

    cond do
      data_size > stream.window_size ->
        throw({:mint, conn, wrap_error({:exceeds_window_size, :request, stream.window_size})})

      data_size > conn.window_size ->
        throw({:mint, conn, wrap_error({:exceeds_window_size, :connection, conn.window_size})})

      # If the data size is greater than the max frame size, we chunk automatically based
      # on the max frame size.
      data_size > conn.server_settings.max_frame_size ->
        {chunks, last_chunk} =
          data
          |> IO.iodata_to_binary()
          |> split_payload_in_chunks(conn.server_settings.max_frame_size)

        {encoded_chunks, conn} =
          Enum.map_reduce(chunks, conn, fn chunk, acc ->
            {acc, encoded} = encode_data_chunk(acc, stream_id, chunk, [])
            {encoded, acc}
          end)

        {conn, encoded_last_chunk} = encode_data_chunk(conn, stream_id, last_chunk, enabled_flags)
        {conn, [encoded_chunks, encoded_last_chunk]}

      true ->
        encode_data_chunk(conn, stream_id, data, enabled_flags)
    end
  end

  defp encode_data_chunk(%__MODULE__{} = conn, stream_id, chunk, enabled_flags)
       when is_integer(stream_id) and is_list(enabled_flags) do
    chunk_size = IO.iodata_length(chunk)
    frame = data(stream_id: stream_id, flags: set_flags(:data, enabled_flags), data: chunk)
    conn = update_in(conn.streams[stream_id].window_size, &(&1 - chunk_size))
    conn = update_in(conn.window_size, &(&1 - chunk_size))

    conn =
      if :end_stream in enabled_flags do
        put_in(conn.streams[stream_id].state, :half_closed_local)
      else
        conn
      end

    {conn, Frame.encode(frame)}
  end

  defp split_payload_in_chunks(binary, chunk_size),
    do: split_payload_in_chunks(binary, chunk_size, [])

  defp split_payload_in_chunks(chunk, chunk_size, acc) when byte_size(chunk) <= chunk_size do
    {Enum.reverse(acc), chunk}
  end

  defp split_payload_in_chunks(binary, chunk_size, acc) do
    <<chunk::size(chunk_size)-binary, rest::binary>> = binary
    split_payload_in_chunks(rest, chunk_size, [chunk | acc])
  end

  defp send_ping(conn, payload) do
    frame = Frame.ping(stream_id: 0, opaque_data: payload)
    conn = send!(conn, Frame.encode(frame))
    ref = make_ref()
    conn = update_in(conn.ping_queue, &:queue.in({ref, payload}, &1))
    {conn, ref}
  end

  defp send_settings(conn, settings) do
    validate_client_settings!(settings)
    frame = settings(stream_id: 0, params: settings)
    conn = send!(conn, Frame.encode(frame))
    conn = update_in(conn.client_settings_queue, &:queue.in(settings, &1))
    conn
  end

  defp validate_client_settings!(settings) do
    unless Keyword.keyword?(settings) do
      raise ArgumentError, "settings must be a keyword list"
    end

    Enum.each(settings, fn
      {:header_table_size, value} ->
        unless is_integer(value) do
          raise ArgumentError, ":header_table_size must be an integer, got: #{inspect(value)}"
        end

      {:enable_push, value} ->
        unless is_boolean(value) do
          raise ArgumentError, ":enable_push must be a boolean, got: #{inspect(value)}"
        end

      {:max_concurrent_streams, value} ->
        unless is_integer(value) do
          raise ArgumentError,
                ":max_concurrent_streams must be an integer, got: #{inspect(value)}"
        end

      {:initial_window_size, value} ->
        unless is_integer(value) and value <= @max_window_size do
          raise ArgumentError,
                ":initial_window_size must be an integer < #{@max_window_size}, " <>
                  "got: #{inspect(value)}"
        end

      {:max_frame_size, value} ->
        unless is_integer(value) and value in @valid_max_frame_size_range do
          raise ArgumentError,
                ":max_frame_size must be an integer in #{inspect(@valid_max_frame_size_range)}, " <>
                  "got: #{inspect(value)}"
        end

      {:max_header_list_size, value} ->
        unless is_integer(value) do
          raise ArgumentError, ":max_header_list_size must be an integer, got: #{inspect(value)}"
        end

      {:enable_connect_protocol, _value} ->
        raise ArgumentError, ":enable_connect_protocol is only valid for server settings"

      {name, _value} ->
        raise ArgumentError, "unknown setting parameter #{inspect(name)}"
    end)
  end

  defp add_default_headers(headers, body) do
    headers
    |> Util.put_new_header("user-agent", @user_agent)
    |> add_default_content_length_header(body)
  end

  defp add_default_content_length_header(headers, body) when body in [nil, :stream] do
    headers
  end

  defp add_default_content_length_header(headers, body) do
    Util.put_new_header_lazy(headers, "content-length", fn ->
      body |> IO.iodata_length() |> Integer.to_string()
    end)
  end

  defp add_pseudo_headers(headers, conn, method, path) do
    if same_method?(method, "CONNECT") do
      [
        {":method", method},
        {":authority", conn.authority}
        | headers
      ]
    else
      [
        {":method", method},
        {":path", path},
        {":scheme", conn.scheme},
        {":authority", conn.authority}
        | headers
      ]
    end
  end

  # same_method?/2 is pretty optimized, so bench before changing.

  # Same binary, which is a common case.
  defp same_method?(bin, bin), do: true

  # Get out early if the size is different, these can't be the same.
  defp same_method?(bin1, bin2) when byte_size(bin1) != byte_size(bin2), do: false

  defp same_method?(<<ch, rest1::binary>>, <<ch, rest2::binary>>), do: same_method?(rest1, rest2)

  defp same_method?(<<lower, rest1::binary>>, <<char, rest2::binary>>) when lower - 32 == char,
    do: same_method?(rest1, rest2)

  defp same_method?(_method1, _method2), do: false

  defp sort_pseudo_headers_to_front(headers) do
    Enum.sort_by(headers, fn {key, _value} ->
      not String.starts_with?(key, ":")
    end)
  end

  ## Frame handling

  defp maybe_concat_and_handle_new_data(conn, data) do
    data = Util.maybe_concat(conn.buffer, data)
    {conn, responses} = handle_new_data(conn, data, [])
    {:ok, conn, Enum.reverse(responses)}
  end

  defp handle_new_data(%__MODULE__{} = conn, data, responses) do
    case Frame.decode_next(data, conn.client_settings.max_frame_size) do
      {:ok, frame, rest} ->
        log(conn, :debug, "Received frame: #{Frame.inspect(frame)}")
        conn = validate_frame(conn, frame)
        {conn, responses} = handle_frame(conn, frame, responses)
        handle_new_data(conn, rest, responses)

      :more ->
        conn = put_in(conn.buffer, data)
        handle_consumed_all_frames(conn, responses)

      {:error, :payload_too_big} ->
        debug_data = "frame payload exceeds connection's max frame size"
        send_connection_error!(conn, :frame_size_error, debug_data)

      {:error, {:frame_size_error, frame}} ->
        debug_data = "error with size of frame: #{inspect(frame)}"
        send_connection_error!(conn, :frame_size_error, debug_data)

      {:error, {:protocol_error, info}} ->
        debug_data = "error when decoding frame: #{inspect(info)}"
        send_connection_error!(conn, :protocol_error, debug_data)
    end
  catch
    :throw, {:mint, conn, error} -> throw({:mint, conn, error, responses})
    :throw, {:mint, _conn, _error, _responses} = thrown -> throw(thrown)
  end

  defp handle_consumed_all_frames(%{state: state} = conn, responses) do
    case state do
      {:goaway, :no_error, _debug_data} ->
        {conn, responses}

      {:goaway, error_code, debug_data} ->
        error = wrap_error({:server_closed_connection, error_code, debug_data})
        throw({:mint, conn, error, responses})

      _ ->
        {conn, responses}
    end
  end

  defp validate_frame(conn, unknown()) do
    # Unknown frames MUST be ignored:
    # https://datatracker.ietf.org/doc/html/rfc7540#section-4.1
    conn
  end

  defp validate_frame(conn, frame) do
    type = elem(frame, 0)
    stream_id = elem(frame, 1)

    # The SETTINGS frame MUST be the first frame that the server sends.
    # https://www.rfc-editor.org/rfc/rfc7540#section-3.5
    # > The server connection preface consists of a potentially empty SETTINGS frame
    # > that MUST be the first frame the server sends in the HTTP/2 connection.
    conn =
      cond do
        conn.state == :handshaking and type == :goaway ->
          goaway(error_code: error_code, debug_data: debug_data) = frame
          error = wrap_error({:server_closed_connection, error_code, debug_data})
          throw({:mint, %{conn | state: :closed}, error, []})

        conn.state == :handshaking and type != :settings ->
          debug_data = "received invalid frame #{type} during handshake"
          send_connection_error!(conn, :protocol_error, debug_data)

        conn.state == :handshaking ->
          %{conn | state: :open}

        true ->
          conn
      end

    assert_frame_on_right_level(conn, elem(frame, 0), stream_id)
    assert_stream_id_is_allowed(conn, stream_id)
    assert_frame_doesnt_interrupt_header_streaming(conn, frame)
    conn
  end

  # http://httpwg.org/specs/rfc7540.html#HttpSequence
  defp assert_frame_doesnt_interrupt_header_streaming(conn, frame) do
    case {conn.headers_being_processed, frame} do
      {nil, continuation()} ->
        debug_data = "CONTINUATION received outside of headers streaming"
        send_connection_error!(conn, :protocol_error, debug_data)

      {nil, _frame} ->
        :ok

      {{stream_id, _, _}, continuation(stream_id: stream_id)} ->
        :ok

      _other ->
        debug_data =
          "headers are streaming but got a #{inspect(elem(frame, 0))} frame instead " <>
            "of a CONTINUATION frame"

        send_connection_error!(conn, :protocol_error, debug_data)
    end
  end

  stream_level_frames = [:data, :headers, :priority, :rst_stream, :push_promise, :continuation]
  connection_level_frames = [:settings, :ping, :goaway]

  defp assert_frame_on_right_level(conn, frame, _stream_id = 0)
       when frame in unquote(stream_level_frames) do
    debug_data = "frame #{inspect(frame)} not allowed at the connection level (stream_id = 0)"
    send_connection_error!(conn, :protocol_error, debug_data)
  end

  defp assert_frame_on_right_level(conn, frame, stream_id)
       when frame in unquote(connection_level_frames) and stream_id != 0 do
    debug_data = "frame #{inspect(frame)} only allowed at the connection level"
    send_connection_error!(conn, :protocol_error, debug_data)
  end

  defp assert_frame_on_right_level(_conn, _frame, _stream_id) do
    :ok
  end

  defp assert_stream_id_is_allowed(conn, stream_id) do
    if Integer.is_odd(stream_id) and stream_id >= conn.next_stream_id do
      debug_data = "frame with stream ID #{inspect(stream_id)} has not been opened yet"
      send_connection_error!(conn, :protocol_error, debug_data)
    else
      :ok
    end
  end

  for frame_name <- stream_level_frames ++ connection_level_frames ++ [:window_update, :unknown] do
    function_name = :"handle_#{frame_name}"

    defp handle_frame(conn, Frame.unquote(frame_name)() = frame, responses) do
      unquote(function_name)(conn, frame, responses)
    end
  end

  defp handle_unknown(conn, _frame, responses) do
    # Implementations MUST ignore and discard any frame that has a type that is unknown.
    # see: https://datatracker.ietf.org/doc/html/rfc7540#section-4.1

    {conn, responses}
  end

  # DATA

  defp handle_data(conn, frame, responses) do
    data(stream_id: stream_id, flags: flags, data: data, padding: padding) = frame

    # Regardless of whether we have the stream or not, we need to abide by flow
    # control rules so we still refill the client window for the stream_id we got.
    window_size_increment = byte_size(data) + byte_size(padding || "")

    conn =
      if window_size_increment > 0 do
        refill_client_windows(conn, stream_id, window_size_increment)
      else
        conn
      end

    case Map.fetch(conn.streams, stream_id) do
      {:ok, stream} ->
        assert_stream_in_state(conn, stream, [:open, :half_closed_local])
        responses = [{:data, stream.ref, data} | responses]

        if flag_set?(flags, :data, :end_stream) do
          conn = close_stream!(conn, stream.id, :remote_end_stream)
          {conn, [{:done, stream.ref} | responses]}
        else
          {conn, responses}
        end

      :error ->
        log(conn, :debug, "Received DATA frame on closed stream ID #{stream_id}")
        {conn, responses}
    end
  end

  defp refill_client_windows(conn, stream_id, data_size) do
    connection_frame = window_update(stream_id: 0, window_size_increment: data_size)
    stream_frame = window_update(stream_id: stream_id, window_size_increment: data_size)

    if open?(conn) do
      send!(conn, [Frame.encode(connection_frame), Frame.encode(stream_frame)])
    else
      conn
    end
  end

  # HEADERS

  defp handle_headers(conn, frame, responses) do
    headers(stream_id: stream_id, flags: flags, hbf: hbf) = frame

    stream = Map.get(conn.streams, stream_id)
    end_stream? = flag_set?(flags, :headers, :end_stream)

    if stream do
      assert_stream_in_state(conn, stream, [:open, :half_closed_local, :reserved_remote])
    end

    if flag_set?(flags, :headers, :end_headers) do
      decode_hbf_and_add_responses(conn, responses, hbf, stream, end_stream?)
    else
      callback = &decode_hbf_and_add_responses(&1, &2, &3, &4, end_stream?)
      conn = put_in(conn.headers_being_processed, {stream_id, hbf, callback})
      {conn, responses}
    end
  end

  # Here, "stream" can be nil in case the stream was closed. In that case, we
  # still need to process the hbf so that the HPACK table is updated, but then
  # we don't add any responses.
  defp decode_hbf_and_add_responses(conn, responses, hbf, stream, end_stream?) do
    {conn, headers} = decode_hbf(conn, hbf)

    if stream do
      handle_decoded_headers_for_stream(conn, responses, stream, headers, end_stream?)
    else
      log(conn, :debug, "Received HEADERS frame on closed stream ID")
      {conn, responses}
    end
  end

  defp handle_decoded_headers_for_stream(conn, responses, stream, headers, end_stream?) do
    %{ref: ref, received_first_headers?: received_first_headers?} = stream

    case headers do
      # Interim response (1xx), which is made of only one HEADERS plus zero or more CONTINUATIONs.
      # There can be zero or more interim responses before a "proper" response.
      # https://httpwg.org/specs/rfc9113.html#HttpFraming
      [{":status", <<?1, _, _>> = status} | headers] ->
        cond do
          received_first_headers? ->
            conn = close_stream!(conn, stream.id, :protocol_error)

            debug_data =
              "informational response (1xx) must appear before final response, got a #{status} status"

            error = wrap_error({:protocol_error, debug_data})
            responses = [{:error, stream.ref, error} | responses]
            {conn, responses}

          end_stream? ->
            conn = close_stream!(conn, stream.id, :protocol_error)
            debug_data = "informational response (1xx) must not have the END_STREAM flag set"
            error = wrap_error({:protocol_error, debug_data})
            responses = [{:error, stream.ref, error} | responses]
            {conn, responses}

          true ->
            assert_stream_in_state(conn, stream, [:open, :half_closed_local])
            status = String.to_integer(status)
            headers = join_cookie_headers(headers)
            new_responses = [{:headers, ref, headers}, {:status, ref, status} | responses]
            {conn, new_responses}
        end

      [{":status", status} | headers] when not received_first_headers? ->
        conn = put_in(conn.streams[stream.id].received_first_headers?, true)
        status = String.to_integer(status)
        headers = join_cookie_headers(headers)
        new_responses = [{:headers, ref, headers}, {:status, ref, status} | responses]

        cond do
          # :reserved_remote means that this was a promised stream. As soon as headers come,
          # the stream goes in the :half_closed_local state (unless it's not allowed because
          # of the client's max concurrent streams limit, or END_STREAM is set).
          stream.state == :reserved_remote ->
            cond do
              conn.open_server_stream_count >= conn.client_settings.max_concurrent_streams ->
                conn = close_stream!(conn, stream.id, :refused_stream)
                {conn, responses}

              end_stream? ->
                conn = close_stream!(conn, stream.id, :remote_end_stream)
                {conn, [{:done, ref} | new_responses]}

              true ->
                conn = update_in(conn.open_server_stream_count, &(&1 + 1))
                conn = put_in(conn.streams[stream.id].state, :half_closed_local)
                {conn, new_responses}
            end

          end_stream? ->
            conn = close_stream!(conn, stream.id, :remote_end_stream)
            {conn, [{:done, ref} | new_responses]}

          true ->
            {conn, new_responses}
        end

      # Trailer headers. We don't care about the :status header here.
      headers when received_first_headers? ->
        if end_stream? do
          conn = close_stream!(conn, stream.id, :remote_end_stream)
          headers = headers |> Headers.remove_unallowed_trailer() |> join_cookie_headers()
          {conn, [{:done, ref}, {:headers, ref, headers} | responses]}
        else
          # Trailer headers must set the END_STREAM flag because they're
          # the last thing allowed on the stream (other than RST_STREAM and
          # the usual frames).
          conn = close_stream!(conn, stream.id, :protocol_error)
          debug_data = "trailer headers didn't set the END_STREAM flag"
          error = wrap_error({:protocol_error, debug_data})
          responses = [{:error, stream.ref, error} | responses]
          {conn, responses}
        end

      # Non-trailer headers need to have a :status header, otherwise
      # it's a protocol error.
      _headers ->
        conn = close_stream!(conn, stream.id, :protocol_error)
        error = wrap_error(:missing_status_header)
        responses = [{:error, stream.ref, error} | responses]
        {conn, responses}
    end
  end

  defp decode_hbf(conn, hbf) do
    case HPAX.decode(hbf, conn.decode_table) do
      {:ok, headers, decode_table} ->
        conn = put_in(conn.decode_table, decode_table)
        {conn, headers}

      {:error, reason} ->
        debug_data = "unable to decode headers: #{inspect(reason)}"
        send_connection_error!(conn, :compression_error, debug_data)
    end
  end

  defp join_cookie_headers(headers) do
    # If we have 0 or 1 Cookie headers, we just use the old list of headers.
    case Enum.split_with(headers, fn {name, _value} -> Headers.lower_raw(name) == "cookie" end) do
      {[], _headers} ->
        headers

      {[_], _headers} ->
        headers

      {cookies, headers} ->
        cookie = Enum.map_join(cookies, "; ", fn {_name, value} -> value end)
        [{"cookie", cookie} | headers]
    end
  end

  # PRIORITY

  # For now we ignore all PRIORITY frames. This shouldn't cause practical trouble.
  defp handle_priority(conn, frame, responses) do
    log(conn, :warning, "Ignoring PRIORITY frame: #{inspect(frame)}")
    {conn, responses}
  end

  # RST_STREAM

  defp handle_rst_stream(conn, frame, responses) do
    rst_stream(stream_id: stream_id, error_code: error_code) = frame

    # If we receive RST_STREAM on a closed stream, we ignore it.
    case Map.fetch(conn.streams, stream_id) do
      {:ok, stream} ->
        # If we receive RST_STREAM then the stream is definitely closed.
        # We won't send anything else on the stream so we can simply delete
        # it, so that if we get things like DATA on that stream we error out.
        conn = delete_stream(conn, stream)

        if error_code == :no_error do
          {conn, [{:done, stream.ref} | responses]}
        else
          error = wrap_error({:server_closed_request, error_code})
          {conn, [{:error, stream.ref, error} | responses]}
        end

      :error ->
        {conn, responses}
    end
  end

  # SETTINGS

  defp handle_settings(conn, frame, responses) do
    settings(flags: flags, params: params) = frame

    if flag_set?(flags, :settings, :ack) do
      conn = apply_client_settings(conn)
      {conn, responses}
    else
      conn = apply_server_settings(conn, params)
      frame = settings(flags: set_flags(:settings, [:ack]), params: [])
      conn = send!(conn, Frame.encode(frame))
      {conn, responses}
    end
  end

  defp apply_server_settings(conn, server_settings) do
    Enum.reduce(server_settings, conn, fn
      {:header_table_size, header_table_size}, conn ->
        update_in(conn.encode_table, &HPAX.resize(&1, header_table_size))

      {:enable_push, enable_push?}, conn ->
        put_in(conn.server_settings.enable_push, enable_push?)

      {:max_concurrent_streams, max_concurrent_streams}, conn ->
        put_in(conn.server_settings.max_concurrent_streams, max_concurrent_streams)

      {:initial_window_size, initial_window_size}, conn ->
        if initial_window_size > @max_window_size do
          debug_data = "INITIAL_WINDOW_SIZE setting of #{initial_window_size} is too big"
          send_connection_error!(conn, :flow_control_error, debug_data)
        end

        update_server_initial_window_size(conn, initial_window_size)

      {:max_frame_size, max_frame_size}, conn ->
        if max_frame_size not in @valid_max_frame_size_range do
          debug_data = "MAX_FRAME_SIZE setting parameter outside of allowed range"
          send_connection_error!(conn, :protocol_error, debug_data)
        end

        put_in(conn.server_settings.max_frame_size, max_frame_size)

      {:max_header_list_size, max_header_list_size}, conn ->
        put_in(conn.server_settings.max_header_list_size, max_header_list_size)

      {:enable_connect_protocol, enable_connect_protocol?}, conn ->
        put_in(conn.server_settings.enable_connect_protocol, enable_connect_protocol?)
    end)
  end

  defp apply_client_settings(conn) do
    case get_and_update_in(conn.client_settings_queue, &:queue.out/1) do
      {{:value, params}, conn} ->
        apply_client_settings(conn, params)

      {:empty, conn} ->
        log(
          conn,
          :warning,
          "Received SETTINGS ACK but client is not waiting for ACKs; ignoring it"
        )

        conn
    end
  end

  defp apply_client_settings(conn, client_settings) do
    Enum.reduce(client_settings, conn, fn
      {setting, value}, conn when setting in @valid_client_settings ->
        update_in(conn.client_settings, &%{&1 | setting => value})

      {setting, _value}, _conn ->
        raise "received ack from server for invalid client setting: #{inspect(setting)}}"
    end)
  end

  defp update_server_initial_window_size(conn, new_iws) do
    diff = new_iws - conn.server_settings.initial_window_size

    conn =
      update_in(conn.streams, fn streams ->
        for {stream_id, stream} <- streams,
            stream.state in [:open, :half_closed_remote],
            into: streams do
          window_size = stream.window_size + diff

          if window_size > @max_window_size do
            debug_data =
              "INITIAL_WINDOW_SIZE parameter of #{window_size} makes some window sizes too big"

            send_connection_error!(conn, :flow_control_error, debug_data)
          end

          {stream_id, %{stream | window_size: window_size}}
        end
      end)

    put_in(conn.server_settings.initial_window_size, new_iws)
  end

  # PUSH_PROMISE

  defp handle_push_promise(
         %__MODULE__{client_settings: %{enable_push: false}} = conn,
         push_promise(),
         _responses
       ) do
    debug_data = "received PUSH_PROMISE frame when SETTINGS_ENABLE_PUSH was false"
    send_connection_error!(conn, :protocol_error, debug_data)
  end

  defp handle_push_promise(conn, push_promise() = frame, responses) do
    push_promise(
      stream_id: stream_id,
      flags: flags,
      promised_stream_id: promised_stream_id,
      hbf: hbf
    ) = frame

    assert_valid_promised_stream_id(conn, promised_stream_id)

    stream = fetch_stream!(conn, stream_id)
    assert_stream_in_state(conn, stream, [:open, :half_closed_local])

    if flag_set?(flags, :push_promise, :end_headers) do
      decode_push_promise_headers_and_add_response(
        conn,
        responses,
        hbf,
        stream,
        promised_stream_id
      )
    else
      callback = &decode_push_promise_headers_and_add_response(&1, &2, &3, &4, promised_stream_id)
      conn = put_in(conn.headers_being_processed, {stream_id, hbf, callback})
      {conn, responses}
    end
  end

  defp decode_push_promise_headers_and_add_response(
         conn,
         responses,
         hbf,
         stream,
         promised_stream_id
       ) do
    {conn, headers} = decode_hbf(conn, hbf)

    promised_stream = %{
      id: promised_stream_id,
      ref: make_ref(),
      state: :reserved_remote,
      window_size: conn.server_settings.initial_window_size,
      received_first_headers?: false
    }

    conn = put_in(conn.streams[promised_stream.id], promised_stream)
    new_response = {:push_promise, stream.ref, promised_stream.ref, headers}
    {conn, [new_response | responses]}
  end

  defp assert_valid_promised_stream_id(conn, promised_stream_id) do
    cond do
      not is_integer(promised_stream_id) or Integer.is_odd(promised_stream_id) ->
        debug_data = "invalid promised stream ID: #{inspect(promised_stream_id)}"
        send_connection_error!(conn, :protocol_error, debug_data)

      Map.has_key?(conn.streams, promised_stream_id) ->
        debug_data =
          "stream with ID #{inspect(promised_stream_id)} already exists and can't be " <>
            "reserved by the server"

        send_connection_error!(conn, :protocol_error, debug_data)

      true ->
        :ok
    end
  end

  # PING

  defp handle_ping(conn, Frame.ping() = frame, responses) do
    Frame.ping(flags: flags, opaque_data: opaque_data) = frame

    if flag_set?(flags, :ping, :ack) do
      handle_ping_ack(conn, opaque_data, responses)
    else
      ack = Frame.ping(stream_id: 0, flags: set_flags(:ping, [:ack]), opaque_data: opaque_data)
      conn = send!(conn, Frame.encode(ack))
      {conn, responses}
    end
  end

  defp handle_ping_ack(conn, opaque_data, responses) do
    case :queue.peek(conn.ping_queue) do
      {:value, {ref, ^opaque_data}} ->
        conn = update_in(conn.ping_queue, &:queue.drop/1)
        {conn, [{:pong, ref} | responses]}

      {:value, _} ->
        log(conn, :warning, "Received PING ack that doesn't match next PING request in the queue")
        {conn, responses}

      :empty ->
        log(conn, :warning, "Received PING ack but no PING requests are pending")
        {conn, responses}
    end
  end

  # GOAWAY

  defp handle_goaway(conn, frame, responses) do
    goaway(
      last_stream_id: last_stream_id,
      error_code: error_code,
      debug_data: debug_data
    ) = frame

    # We gather all the unprocessed requests and form {:error, _, _} tuples for each one.
    # At the same time, we delete all the unprocessed requests from the stream set.
    {unprocessed_request_responses, conn} =
      Enum.flat_map_reduce(conn.streams, conn, fn
        {stream_id, _stream}, conn_acc when stream_id <= last_stream_id ->
          {[], conn_acc}

        {_stream_id, stream}, conn_acc ->
          conn_acc = delete_stream(conn_acc, stream)
          {[{:error, stream.ref, wrap_error(:unprocessed)}], conn_acc}
      end)

    message =
      case error_code do
        :no_error -> "Server closed connection normally"
        _other -> "Server closed connection with error #{inspect(error_code)}"
      end

    log(conn, :debug, "#{message} (with debug data: #{inspect(debug_data)})")

    conn = put_in(conn.state, {:goaway, error_code, debug_data})
    {conn, unprocessed_request_responses ++ responses}
  end

  # WINDOW_UPDATE

  defp handle_window_update(
         conn,
         window_update(stream_id: 0, window_size_increment: wsi),
         responses
       ) do
    new_window_size = conn.window_size + wsi

    if new_window_size > @max_window_size do
      send_connection_error!(conn, :flow_control_error, "window size too big")
    else
      conn = put_in(conn.window_size, new_window_size)
      {conn, responses}
    end
  end

  defp handle_window_update(
         conn,
         window_update(stream_id: stream_id, window_size_increment: wsi),
         responses
       ) do
    stream = fetch_stream!(conn, stream_id)
    new_window_size = conn.streams[stream_id].window_size + wsi

    if new_window_size > @max_window_size do
      conn = close_stream!(conn, stream_id, :flow_control_error)
      error = wrap_error({:flow_control_error, "window size too big"})
      {conn, [{:error, stream.ref, error} | responses]}
    else
      conn = put_in(conn.streams[stream_id].window_size, new_window_size)
      {conn, responses}
    end
  end

  # CONTINUATION

  defp handle_continuation(conn, frame, responses) do
    continuation(stream_id: stream_id, flags: flags, hbf: hbf_chunk) = frame
    stream = Map.get(conn.streams, stream_id)

    if stream do
      assert_stream_in_state(conn, stream, [:open, :half_closed_local, :reserved_remote])
    end

    {^stream_id, hbf_acc, callback} = conn.headers_being_processed

    if flag_set?(flags, :continuation, :end_headers) do
      hbf = IO.iodata_to_binary([hbf_acc, hbf_chunk])
      conn = put_in(conn.headers_being_processed, nil)
      callback.(conn, responses, hbf, stream)
    else
      conn = put_in(conn.headers_being_processed, {stream_id, [hbf_acc, hbf_chunk], callback})
      {conn, responses}
    end
  end

  ## General helpers

  defp send_connection_error!(conn, error_code, debug_data) do
    frame =
      goaway(stream_id: 0, last_stream_id: 2, error_code: error_code, debug_data: debug_data)

    # Try to send the GOAWAY frame and close connection.
    # If the frame fails to send, we still want to set the close
    # the socket, set the connection state to :closed, and return an error.
    _ = conn.transport.send(conn.socket, Frame.encode(frame))
    _ = conn.transport.close(conn.socket)

    throw({:mint, %{conn | state: :closed}, wrap_error({error_code, debug_data})})
  end

  # Reason is either an error code or `remote_end_stream`
  defp close_stream!(conn, stream_id, reason) do
    stream = Map.fetch!(conn.streams, stream_id)

    conn =
      cond do
        # If the stream is ended on both sides, it is already deemed closed and
        # there's no need to send a RST_STREAM frame
        reason == :remote_end_stream and stream.state == :half_closed_local ->
          conn

        # We send a RST_STREAM with the given error code so that we move the
        # stream to the :closed state (that is, we remove it).
        open?(conn) ->
          error_code = if reason == :remote_end_stream, do: :no_error, else: reason
          rst_stream_frame = rst_stream(stream_id: stream_id, error_code: error_code)
          send!(conn, Frame.encode(rst_stream_frame))

        # If the connection is already closed, no-op
        true ->
          conn
      end

    delete_stream(conn, stream)
  end

  defp delete_stream(conn, stream) do
    conn = update_in(conn.streams, &Map.delete(&1, stream.id))
    conn = update_in(conn.ref_to_stream_id, &Map.delete(&1, stream.ref))

    stream_open? = stream.state in [:open, :half_closed_local, :half_closed_remote]

    conn =
      cond do
        # Stream initiated by the client.
        stream_open? and Integer.is_odd(stream.id) ->
          update_in(conn.open_client_stream_count, &(&1 - 1))

        # Stream initiated by the server.
        stream_open? and Integer.is_even(stream.id) ->
          update_in(conn.open_server_stream_count, &(&1 - 1))

        true ->
          conn
      end

    conn
  end

  defp fetch_stream!(conn, stream_id) do
    case Map.fetch(conn.streams, stream_id) do
      {:ok, stream} -> stream
      :error -> throw({:mint, conn, wrap_error({:stream_not_found, stream_id})})
    end
  end

  defp assert_stream_in_state(conn, %{state: state}, expected_states) do
    if state not in expected_states do
      debug_data =
        "stream was in state #{inspect(state)} and not in one of the expected states: " <>
          Enum.map_join(expected_states, ", ", &inspect/1)

      send_connection_error!(conn, :protocol_error, debug_data)
    end
  end

  defp send!(%__MODULE__{transport: transport, socket: socket} = conn, bytes) do
    case transport.send(socket, bytes) do
      :ok ->
        conn

      {:error, %TransportError{reason: :closed} = error} ->
        throw({:mint, %{conn | state: :closed}, error})

      {:error, reason} ->
        throw({:mint, conn, reason})
    end
  end

  defp wrap_error(reason) do
    %HTTPError{reason: reason, module: __MODULE__}
  end

  @doc false
  def format_error(reason)

  def format_error(:closed) do
    "the connection is closed"
  end

  def format_error(:closed_for_writing) do
    "the connection is closed for writing, which means that you cannot issue any more " <>
      "requests on the connection but you can expect responses to still be delivered for " <>
      "part of the requests that are in flight. If a connection is closed for writing, " <>
      "it usually means that you got a :server_closed_request error already."
  end

  def format_error(:too_many_concurrent_requests) do
    "the number of max concurrent HTTP/2 requests supported by the server has been reached. " <>
      "Use Mint.HTTP2.get_server_setting/2 with the :max_concurrent_streams setting name " <>
      "to find out the maximum number of concurrent requests supported by the server."
  end

  def format_error({:max_header_list_size_exceeded, size, max_size}) do
    "the given header list (of size #{size}) goes over the max header list size of " <>
      "#{max_size} supported by the server. In HTTP/2, the header list size is calculated " <>
      "by summing up the size in bytes of each header name, value, plus 32 for each header."
  end

  def format_error({:exceeds_window_size, what, window_size}) do
    what =
      case what do
        :request -> "request"
        :connection -> "connection"
      end

    "the given data exceeds the #{what} window size, which is #{window_size}. " <>
      "The server will refill the window size of the #{what} when ready. This will be " <>
      "handled transparently by stream/2."
  end

  def format_error({:stream_not_found, stream_id}) do
    "request not found (with stream_id #{inspect(stream_id)})"
  end

  def format_error(:unknown_request_to_stream) do
    "can't stream chunk of data because the request is unknown"
  end

  def format_error(:request_is_not_streaming) do
    "can't send more data on this request since it's not streaming"
  end

  def format_error({:unallowed_trailing_header, name}) do
    "header #{inspect(name)} is not allowed as a trailer header"
  end

  def format_error(:missing_status_header) do
    "the :status pseudo-header (which is required in HTTP/2) is missing from the response"
  end

  def format_error({:server_closed_request, error_code}) do
    "server closed request with error code #{inspect(error_code)}"
  end

  def format_error({:server_closed_connection, error, debug_data}) do
    "server closed connection with error code #{inspect(error)} and debug data: " <> debug_data
  end

  def format_error(:unprocessed) do
    "request was not processed by the server, which means that it's safe to retry on a " <>
      "different or new connection"
  end

  def format_error({:frame_size_error, frame}) do
    "frame size error for #{inspect(frame)} frame"
  end

  def format_error({:protocol_error, debug_data}) do
    "protocol error: " <> debug_data
  end

  def format_error({:compression_error, debug_data}) do
    "compression error: " <> debug_data
  end

  def format_error({:flow_control_error, debug_data}) do
    "flow control error: " <> debug_data
  end
end
