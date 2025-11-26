defmodule Mint.HTTP do
  @moduledoc """
  Process-less HTTP connection data structure and functions.

  Single interface for `Mint.HTTP1` and `Mint.HTTP2` with support for version
  negotiation and proxies.

  ## Usage

  To establish a connection with a given server, use `connect/4`. This will
  return an opaque data structure that represents the connection
  to the server. To send a request, you can use `request/5`. Sending a request
  does not take care of the response to that request, instead we use `Mint.HTTP.stream/2`
  to process the response, which we will look at in just a bit. The connection is a
  wrapper around a TCP (`:gen_tcp` module) or SSL (`:ssl` module) socket that is
  set in **active mode** (with `active: :once`). This means that TCP/SSL messages
  will be delivered to the process that started the connection.

  The process that owns the connection is responsible for receiving the messages
  (for example, a GenServer is responsible for defining `handle_info/2`). However,
  `Mint.HTTP` makes it easy to identify TCP/SSL messages that are coming from the
  connection with the server with the `stream/2` function. This function takes the
  connection and a term and returns `:unknown` if the term is not a TCP/SSL message
  belonging to the connection. If the term *is* a message for the connection, then
  a response and a new connection are returned. It's important to store the new
  returned connection data structure over the old one since the connection is an
  immutable data structure.

  Let's see an example of a common workflow of connecting to a server, sending a
  request, and processing the response. We start by using `connect/3` to connect
  to a server.

      {:ok, conn} = Mint.HTTP.connect(:http, "httpbin.org", 80)

  `conn` is a data structure that represents the connection.

  To send a request, we use `request/5`.

      {:ok, conn, request_ref} = Mint.HTTP.request(conn, "GET", "/", [], nil)

  As you can see, sending a request returns a new updated `conn` struct and a
  `request_ref`. The updated connection struct is returned because the connection
  is an immutable structure keeping the connection state, so every action we do on it must return a new,
  possibly updated, connection that we're responsible for storing over the old
  one. `request_ref` is a unique reference that can be used to identify which
  request a given response belongs to.

  Now that we sent our request, we're responsible for receiving the messages that
  the TCP/SSL socket will send to our process. For example, in a GenServer
  we would do that with a `handle_info/2` callback. In our case, we're going to
  use a simple `receive`. `Mint.HTTP` provides a way to tell if a message comes
  from the socket wrapped by our connection or not: the `stream/2` function. If
  the message we pass to it is not destined for our connection, this function returns
  `:unknown`. Otherwise, it returns an updated connection and one or more responses.

      receive do
        message ->
          case Mint.HTTP.stream(conn, message) do
            :unknown -> handle_normal_message(message)
            {:ok, conn, responses} -> handle_responses(conn, responses)
          end
      end

  `responses` is a list of possible responses. The most common responses are:

    * `{:status, request_ref, status_code}` for the status code
    * `{:headers, request_ref, headers}` for the response headers
    * `{:data, request_ref, binary}` for pieces of the response body
    * `{:done, request_ref}` for the end of the response

  As you can see, all responses have the unique request reference as the second
  element of the tuple, so that we know which request the response belongs to.
  See `t:Mint.Types.response/0` for the full list of responses returned by `Mint.HTTP.stream/2`.

  ## Architecture

  A processless architecture like the one here requires a few modifications to how
  we use this HTTP client. Usually, you will want to create this data structure
  in a process that acts as *connection manager*. Sometimes, you might want to
  have a single process responsible for multiple connections, either to just one
  host or multiple hosts. For more discussion on architectures based off of this
  HTTP client, see the [*Architecture*](architecture.html) page in the docs.

  ## SSL certificates

  When using SSL, you can pass in your own CA certificate store or use one provided by Mint. Mint
  doesn't ship with the certificate store itself, but it has an optional dependency on
  [CAStore](https://github.com/elixir-mint/castore), which provides an up-to-date certificate store. If
  you don't want to use your own certificate store, just add `:castore` to your dependencies.

  Starting [from OTP
  25](https://www.erlang.org/blog/my-otp-25-highlights/#ca-certificates-can-be-fetched-from-the-os-standard-place),
  you can also load certificates from a file
  ([`:public_key.cacerts_load/1`](https://www.erlang.org/doc/man/public_key.html#cacerts_load-1)).
  You can also get certificate from the OS trust store using
  [`:public_key.cacerts_get/0`](https://www.erlang.org/doc/man/public_key.html#cacerts_get-0).
  If you are using OTP 25+ it is recommended to set this option.

      Mint.HTTP.connect(:https, host, port, transport_opts: [cacerts: :public_key.cacerts_get()])

  ## Mode

  By default Mint operates in **active mode** meaning that the process that started the
  connection receives socket messages. Mint also supports **passive mode**, where no messages
  are sent to the process and the process needs to fetch data out of the socket manually.
  The mode can be controlled at connection time through the `:mode` option in `connect/4`
  or changed dynamically through `set_mode/2`. Passive mode is generally only recommended
  for special use cases.

  ## Logging

  Mint uses the `Logger` module to log information about the connection. Most logs are
  emitted *since version 1.5.0*. The logs are not emitted by default, since we consider
  Mint to be too low level. However, you can enable logging by passing `log: true` to
  `connect/4`.

  > #### Changes to the Format of Logs {: .warning}
  >
  > The format of logs emitted by Mint might change without notice between any versions,
  > without it being considered a breaking change. You are only meant to control what
  > gets logged by using the `Logger` API and Erlang's `:logger` module.
  """

  alias Mint.{Types, TunnelProxy, UnsafeProxy}
  alias Mint.Core.{Transport, Util}

  @behaviour Mint.Core.Conn

  @opaque t() :: Mint.HTTP1.t() | Mint.HTTP2.t()

  defguardp is_data_message(message)
            when elem(message, 0) in [:ssl, :tcp] and tuple_size(message) == 3

  defguardp is_closed_message(message)
            when elem(message, 0) in [:ssl_closed, :tcp_closed] and tuple_size(message) == 2

  defguardp is_error_message(message)
            when elem(message, 0) in [:ssl_error, :tcp_error] and tuple_size(message) == 3

  defguardp is_non_proxy_connection_message(conn, message)
            when is_struct(conn) and
                   is_tuple(message) and
                   is_map_key(conn, :socket) and
                   elem(message, 1) == :erlang.map_get(:socket, conn) and
                   (is_data_message(message) or is_closed_message(message) or
                      is_error_message(message))

  defguardp is_proxy_conn(conn) when is_struct(conn, Mint.UnsafeProxy)

  @doc """
  Macro to check that a given received `message` is intended for the given connection `conn`.

  This guard is useful in `receive` loops or in callbacks that handle generic messages (such as a
  `c:GenServer.handle_info/2` callback) so that you don't have to hand the `message` to
  `Mint.HTTP.stream/2` and check for the `:unknown_message` return value.

  This macro can be used in guards.

  **Note**: this macro is only available if you compile Mint with Elixir 1.10.0 or greater (and
  OTP 21+, which is required by Elixir 1.10.0 and on).

  ## Examples

      require Mint.HTTP

      {:ok, conn, request_ref} = Mint.HTTP.request(conn, "POST", "/", headers, "")

      receive do
        message when Mint.HTTP.is_connection_message(conn, message) ->
          Mint.HTTP.stream(conn, message)

        other ->
          # This message is related to something else or to some other connection
      end

  """
  @doc since: "1.1.0"
  defguard is_connection_message(conn, message)
           when (is_proxy_conn(conn) and
                   is_non_proxy_connection_message(
                     :erlang.map_get(:state, conn),
                     message
                   )) or is_non_proxy_connection_message(conn, message)

  @doc """
  Creates a new connection to a given server.

  Creates a new connection struct and establishes the connection to the given server,
  identified by the given `host` and `port` combination. Both HTTP and HTTPS are supported
  by passing respectively `:http` and `:https` as the `scheme`.

  The connection struct wraps a socket, which is created once the connection
  is established inside this function. If HTTP is used, then the created socket is a TCP
  socket and the `:gen_tcp` module is used to create that socket. If HTTPS is used, then
  the created socket is an SSL socket and the `:ssl` module is used to create that socket.
  The socket is created in active mode (with `active: :once`), which is why it is important
  to know the type of the socket: messages from the socket will be delivered directly to the
  process that creates the connection and tagged appropriately by the socket module (see the
  `:gen_tcp` and `:ssl` modules). See `stream/2` for more information on the messages and
  how to process them and on the socket mode.

  ## Options

    * `:hostname` - (string) explicitly provide the hostname used for the `Host` header,
      hostname verification, SNI, and so on. **Required when `address` is not a string.**

    * `:transport_opts` - (keyword) options to be given to the transport being used.
      These options will be merged with some default options that cannot be overridden.
      For more details, refer to the "Transport options" section below.

    * `:mode` - (`:active` or `:passive`) whether to set the socket to active or
      passive mode. See the "Mode" section in the module documentation and `set_mode/2`.

    * `:protocols` - (list of atoms) a list of protocols to try when connecting to the
      server. The possible values in the list are `:http1` for HTTP/1 and HTTP/1.1 and
      `:http2` for HTTP/2. If only one protocol is present in the list, then the connection
      will be forced to use that protocol. If both `:http1` and `:http2` are present in the
      list, then Mint will negotiate the protocol. See the section "Protocol negotiation"
      below for more information. Defaults to `[:http1, :http2]`.

    * `:proxy_headers` - a list of headers (`t:Mint.Types.headers/0`) to pass when using
      a proxy. They will be used for the `CONNECT` request in tunnel proxies or merged
      with every request for forward proxies.

    * `:log` - (boolean) whether this connection logs or not. See the ["Logging"
      section](#module-logging) in the module documentation. Defaults to `false`.
      *Available since v1.5.0*.

  The following options are HTTP/1-specific and will force the connection
  to be an HTTP/1 connection.

    * `:proxy` - a `{scheme, address, port, opts}` tuple that identifies a proxy to
      connect to. See the "Proxying" section below for more information.

  The following options are HTTP/2-specific and will only be used on HTTP/2 connections.

    * `:client_settings` - (keyword) a list of client HTTP/2 settings to send to the
      server. See `Mint.HTTP2.put_settings/2` for more information. This is only used
      in HTTP/2 connections.

  There may be further protocol specific options that only take effect when the corresponding
  connection is established. Check `Mint.HTTP1.connect/4` and `Mint.HTTP2.connect/4` for
  details.

  ## Protocol negotiation

  If both `:http1` and `:http2` are present in the list passed in the `:protocols` option,
  the protocol negotiation happens in the following way:

    * If the scheme used to connect to the server is `:http`, then HTTP/1 or HTTP/1.1 is used.

    * If the scheme is `:https`, then ALPN negotiation is used to determine the right
      protocol. This means that the server will decide whether to use HTTP/1 or
      HTTP/2. If the server doesn't support protocol negotiation, we will fall back to
      HTTP/1. If the server negotiates a protocol that we don't know how to handle,
      `{:error, {:bad_alpn_protocol, protocol}}` is returned.

  ## Proxying

  You can set up proxying through the `:proxy` option, which is a tuple
  `{scheme, address, port, opts}` that identifies the proxy to connect to.
  Once a proxied connection is returned, the proxy is transparent to you and you
  can use the connection like a normal HTTP/1 connection.

  If the `scheme` is `:http`, we will connect to the host in the most compatible
  way, supporting older proxy servers. Data will be sent in clear text.

  If the connection scheme is `:https`, we will connect to the host with a tunnel
  through the proxy. Using `:https` for both the proxy and the connection scheme
  is not supported, it is recommended to use `:https` for the end host connection
  instead of the proxy.

  ## Transport options

  The options specified in `:transport_opts` are passed to the module that
  implements the socket interface: `:gen_tcp` when the scheme is `:http`, and
  `:ssl` when the scheme is `:https`. Please refer to the documentation for those
  modules, as well as for `:inet.setopts/2`, for a detailed description of all
  available options.

  The behaviour of some options is modified by Mint, as described below.

  A special case is the `:timeout` option, which is passed to the transport
  module's `connect` function to limit the amount of time to wait for the
  network connection to be established.

  Common options for `:http` and `:https`:

    * `:active` - controlled by the `:mode` option. Cannot be overridden.

    * `:mode` - set to `:binary`. Cannot be overridden.

    * `:packet` - set to `:raw`. Cannot be overridden.

    * `:timeout` - connect timeout in milliseconds. Defaults to `30_000` (30
      seconds), and may be overridden by the caller. Set to `:infinity` to
      disable the connect timeout.

    * `:inet6` - if set to `true` enables IPv6 connection. Defaults to `false`
      and may be overridden by the caller.

    * `:inet4` - if set to `true` falls back to IPv4 if IPv6 connection fails.
      Defaults to `true` and may be overridden by the caller. *Available since
      v1.6.0*.

  Options for `:https` only:

    * `:alpn_advertised_protocols` - managed by Mint. Cannot be overridden.

    * `:cacerts` - certificates of types `:ssl.client_cacerts()`.
      If `:verify` is set to `:verify_peer` (the default) and
      no CA trust store is specified using the `:cacertfile` or `:cacerts`
      option, Mint will attempt to use the trust store from the
      [CAStore](https://github.com/elixir-mint/castore) package or raise an
      exception if this package is not available. It is recommended to set this
      option to `:public_key.cacerts_get()`.

    * `:cacertfile` - path to a file containing PEM-encoded CA certificates.
      See the `:cacerts` option for the defaults to this value.

    * `:ciphers` - defaults to the lists returned by
      `:ssl.filter_cipher_suites(:ssl.cipher_suites(:all, version), [])`
      where `version` is each value in the `:versions` setting. This list is
      then filtered according to the blocklist in
      [RFC7540 appendix A](https://tools.ietf.org/html/rfc7540#appendix-A);
      May be overridden by the caller. See the "Supporting older cipher suites"
      section below for some examples.

    * `:depth` - defaults to `4`. May be overridden by the caller.

    * `:partial_chain` - unless a custom `:partial_chain` function is specified,
      Mint will enable its own partial chain handler, which accepts server
      certificate chains containing a certificate that was issued by a
      CA certificate in the CA trust store, even if that certificate is not
      last in the chain. This improves interoperability with some servers
      (for example, with a cross-signed intermediate CA or some misconfigured servers),
      but is a less strict interpretation of the TLS specification than the
      Erlang/OTP default behaviour.

    * `:reuse_sessions` - defaults to `true`. May be overridden by the caller. If
      `:"tlsv1.3"` is the only TLS version specified, `:reuse_sessions` will be
      removed from the options.

    * `:secure_renegotiate` - defaults to `true`. May be overridden by the
      caller. If `:"tlsv1.3"` is the only TLS version specified, `:secure_renegotiate`
      will be removed from the options.

    * `:server_name_indication` - defaults to specified destination hostname.
      May be overridden by the caller.

    * `:verify` - defaults to `:verify_peer`. May be overridden by the caller.

    * `:verify_fun` - unless a custom `:verify_fun` is specified, or `:verify`
      is set to `:verify_none`, Mint will enable hostname verification with
      support for wildcards in the server's 'SubjectAltName' extension, similar
      to the behaviour implemented in
      `:public_key.pkix_verify_hostname_match_fun(:https)` in recent Erlang/OTP
      releases. This improves compatibility with recently issued wildcard
      certificates also on older Erlang/OTP releases.

    * `:versions` - defaults to `[:"tlsv1.2"]` (TLS v1.2 only). May be
      overridden by the caller.

  ### Supporting older cipher suites

  By default only a small list of modern cipher suites is enabled, in compliance
  with the HTTP/2 specification. Some servers, in particular HTTP/1 servers, may
  not support any of these cipher suites, resulting in TLS handshake failures or
  closed connections.

  To select the default cipher suites of Erlang/OTP (including for example
  AES-CBC), use the following `:transport_opts`:

      # Erlang/OTP 20.3 or later:
      transport_opts: [ciphers: :ssl.cipher_suites(:default, :"tlsv1.2")]
      # Older versions:
      transport_opts: [ciphers: :ssl.cipher_suites()]

  Recent Erlang/OTP releases do not enable RSA key exchange by default, due to
  known weaknesses. If necessary, you can build a cipher list with RSA exchange
  and use it in `:transport_opts`:

      ciphers =
        :ssl.cipher_suites(:all, :"tlsv1.2")
        |> :ssl.filter_cipher_suites(
          key_exchange: &(&1 == :rsa),
          cipher: &(&1 in [:aes_256_gcm, :aes_128_gcm, :aes_256_cbc, :aes_128_cbc])
        )
        |> :ssl.append_cipher_suites(:ssl.cipher_suites(:default, :"tlsv1.2"))

  ## Examples

      {:ok, conn} = Mint.HTTP.connect(:http, "httpbin.org", 80)

  Using a proxy:

      proxy = {:http, "myproxy.example.com", 80, []}
      {:ok, conn} = Mint.HTTP.connect(:https, "httpbin.org", 443, proxy: proxy)

  Forcing the connection to be an HTTP/2 connection:

      {:ok, conn} = Mint.HTTP.connect(:https, "httpbin.org", 443, protocols: [:http2])

  Enable all default cipher suites of Erlang/OTP (release 20.3 or later):

      opts = [transport_opts: [ciphers: :ssl.cipher_suites(:default, :"tlsv1.2")]]
      {:ok, conn} = Mint.HTTP.connect(:https, "httpbin.org", 443, opts)

  """
  @spec connect(Types.scheme(), Types.address(), :inet.port_number(), keyword()) ::
          {:ok, t()} | {:error, Types.error()}
  def connect(scheme, address, port, opts \\ []) do
    case Keyword.fetch(opts, :proxy) do
      {:ok, {proxy_scheme, proxy_address, proxy_port, proxy_opts}} ->
        case Util.scheme_to_transport(scheme) do
          Transport.TCP ->
            proxy = {proxy_scheme, proxy_address, proxy_port}
            host = {scheme, address, port}
            opts = Keyword.merge(opts, proxy_opts)
            UnsafeProxy.connect(proxy, host, opts)

          Transport.SSL ->
            proxy = {proxy_scheme, proxy_address, proxy_port, proxy_opts}
            host = {scheme, address, port, opts}
            TunnelProxy.connect(proxy, host)
        end

      :error ->
        Mint.Negotiate.connect(scheme, address, port, opts)
    end
  end

  @doc false
  @spec upgrade(
          module(),
          Mint.Types.socket(),
          Types.scheme(),
          String.t(),
          :inet.port_number(),
          keyword()
        ) :: {:ok, t()} | {:error, Types.error()}
  def upgrade(old_transport, transport_state, scheme, hostname, port, opts),
    do: Mint.Negotiate.upgrade(old_transport, transport_state, scheme, hostname, port, opts)

  @doc """
  Returns the protocol used by the current connection.

  ## Examples

      iex> Mint.HTTP.protocol(%Mint.HTTP1{})
      :http1

      iex> Mint.HTTP.protocol(%Mint.HTTP2{})
      :http2

  """
  @doc since: "1.4.0"
  @spec protocol(t()) :: :http1 | :http2
  def protocol(conn)

  def protocol(%Mint.HTTP1{}), do: :http1
  def protocol(%Mint.HTTP2{}), do: :http2
  def protocol(%Mint.UnsafeProxy{state: internal_conn}), do: protocol(internal_conn)

  @doc false
  @impl true
  @spec initiate(
          module(),
          Types.socket(),
          String.t(),
          :inet.port_number(),
          keyword()
        ) :: {:ok, t()} | {:error, Types.error()}
  def initiate(transport, transport_state, hostname, port, opts),
    do: Mint.Negotiate.initiate(transport, transport_state, hostname, port, opts)

  @doc """
  Closes the given connection.

  This function closes the socket wrapped by the given connection. Once the socket
  is closed, the connection goes into the "closed" state and `open?/1` returns `false`.
  You can throw away a closed connection.

  Closing a connection does not guarantee that data that is in flight gets delivered
  to the server.

  Always returns `{:ok, conn}` where `conn` is the updated connection.

  ## Examples

      {:ok, conn} = Mint.HTTP.close(conn)

  """
  @impl true
  @spec close(t()) :: {:ok, t()}
  def close(conn), do: conn_apply(conn, :close, [conn])

  @doc """
  Checks whether the connection is open.

  This function returns `true` if the connection is open for the given `type`,
  `false` otherwise. It should be used to check that a connection is open before
  sending requests or performing operations that involve talking to the server.

  The `type` argument can be used to tell whether the connection is open for both reading
  and writing, only open for reading, or closed for both. In HTTP/1, a connection is always
  either open, or closed (for both reading and writing). In HTTP/2, the connection can be closed only
  for writing but not for reading, meaning that you cannot send any more data to the
  server but you can still receive data from the server. In this case, `Mint.HTTP.open?(conn, :read)`
  would return `true` but `Mint.HTTP.open?(conn, :write)` would return `false`.
  See the "Closed connection" section in the module documentation of `Mint.HTTP2`.

  If a connection is *completely closed* (that is, `Mint.HTTP.open?(conn, :read)` returns `false`),
  it has become useless and you should get rid of it. If you still need a connection
  to the server, start a new connection with `connect/4`.

  > #### The default value of `type` is `:write` {: .warning}
  >
  > With the default value of `type` being `:write`, a call to
  > `Mint.HTTP.open?(conn)` will return `false` if `conn` was closed for writing
  > but is still open for reading. If you need to make sure the connection is
  > completely closed, check that `Mint.HTTP.open?(conn, :read)` returns `false`.

  ## Examples

      {:ok, conn} = Mint.HTTP.connect(:http, "httpbin.org", 80)
      Mint.HTTP.open?(conn)
      #=> true

  """
  @impl true
  @spec open?(t(), :read | :write) :: boolean()
  def open?(conn, type \\ :write), do: conn_apply(conn, :open?, [conn, type])

  @doc """
  Sends a request to the connected server.

  This function sends a new request to the server that `conn` is connected to.
  `method` is a string representing the method for the request, such as `"GET"`
  or `"POST"`. `path` is the path on the host to send the request to. `headers`
  is a list of request headers in the form `{header_name, header_value}` with
  `header_name` and `header_value` being strings. `body` can have one of three
  values:

    * `nil` - no body is sent with the request.

    * iodata - the body to send for the request.

    * `:stream` - when the value of the body is `:stream` the request
      body can be streamed on the connection. See `stream_request_body/3`.
      In HTTP/1, you can't open a request if the body of another request is
      streaming.

  If the request is sent correctly, this function returns `{:ok, conn, request_ref}`.
  `conn` is an updated connection that should be stored over the old connection.
  `request_ref` is a unique reference that can be used to match on responses for this
  request that are returned by `stream/2`. See `stream/2` for more information.

  If there's an error with sending the request, `{:error, conn, reason}` is returned.
  `reason` is the cause of the error. `conn` is an updated connection. It's important
  to store the returned connection over the old connection in case of errors too, because
  the state of the connection might change when there are errors as well. An error when
  sending a request **does not** necessarily mean that the connection is closed. Use
  `open?/1` to verify that the connection is open.

  Requests can be pipelined so the full response does not have to received
  before the next request can be sent. It is up to users to verify that the
  server supports pipelining and that the request is safe to pipeline.

  In HTTP/1, you can't open a request if the body of another request is streaming.
  See `Mint.HTTP1.request/5` for more information.

  For a quick discussion on HTTP/2 streams and requests, see the `Mint.HTTP2` module and
  `Mint.HTTP2.request/5`.

  ## The `content-length` header

  If you don't set the `content-length` header and you send a body with the request (that
  is, not `nil` and not `:stream`), then Mint will add a default `content-length` header
  to your request. If you're using HTTP/2 and streaming the request, you may provide the
  `content-length` header yourself. If you're using HTTP/1, Mint will do chunked
  transfer-encoding when a content-length is not provided (see `Mint.HTTP1.request/5`).

  ## Examples

      Mint.HTTP.request(conn, "GET", "/", _headers = [], _body = nil)
      Mint.HTTP.request(conn, "POST", "/path", [{"content-type", "application/json"}], "{}")

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

  def request(conn, method, path, headers, body),
    do: conn_apply(conn, :request, [conn, method, path, headers, body])

  @doc """
  Streams a chunk of the request body on the connection or signals the end of the body.

  If a request is opened (through `request/5`) with the body as `:stream`, then the
  body can be streamed through this function. The function takes a `conn`, a
  `request_ref` returned by `request/5` to identify the request to stream the body for,
  and a chunk of body to stream. The value of chunk can be:

    * iodata - a chunk of iodata is transmitted to the server as part of the body
      of the request. If the chunk is empty, in HTTP/1 it's a no-op, while in HTTP/2
      a `DATA` frame will be sent.

    * `:eof` - signals the end of the streaming of the request body for the given
      request. Usually the server won't send any reply until this is sent.

    * `{:eof, trailer_headers}` - sends **trailer headers** and signals the end
      of the streaming of the request body for the given request. This behaves the
      same way as `:eof` but first sends the trailer headers. See the
      [*Trailer headers*](#module-trailer-headers) section below.

  This function always returns an updated connection to be stored over the old connection.

  For information about transfer encoding and content length in HTTP/1, see
  `Mint.HTTP1.stream_request_body/3`.

  ## Trailer headers

  HTTP trailer headers can be sent after the body of a request. trailer headers are described
  [in RFC 9110](https://www.rfc-editor.org/rfc/rfc9110#section-6.5).

  The behaviour is slightly different for HTTP/1 and HTTP/2:

    * In HTTP/1, trailer headers are only supported if the transfer encoding is set to
      `chunked`. See `Mint.HTTP1.stream_request_body/3` for more information on chunked
      transfer encoding.

    * In HTTP/2, trailer headers behave like normal headers. You don't need to care
      about the transfer encoding.

  ### The `trailer` header

  As specified in [section 4.4 of RFC 7230](https://tools.ietf.org/html/rfc7230#section-4.4),
  in HTTP/1 you need to specify which headers you're going to send as traoler
  headers using the `trailer` header. The `trailer` header applies to both HTTP/1
  and HTTP/2. See the examples below for more information.

  ### The `te` header

  As specified in  [section 4.3 of RFC 7230](https://tools.ietf.org/html/rfc7230#section-4.3),
  the `te` (or `TE`) header is used to specify which transfer-encodings the client
  is willing to accept (besides `chunked`). Mint supports decoding of trailer headers,
  but if you want to notify the server that you are accepting trailer headers,
  use the `trailers` value in the `te` header. For example:

      Mint.HTTP.request(conn, "GET", "/", [{"te", "trailers"}], "some body")

  Note that the `te` header can also be used to communicate which encodings you
  support to the server.

  ## Examples

  Let's see an example of streaming an empty JSON object (`{}`) by streaming one curly
  brace at a time.

      headers = [{"content-type", "application/json"}, {"content-length", "2"}]
      {:ok, conn, request_ref} = Mint.HTTP.request(conn, "POST", "/", headers, :stream)
      {:ok, conn} = Mint.HTTP.stream_request_body(conn, request_ref, "{")
      {:ok, conn} = Mint.HTTP.stream_request_body(conn, request_ref, "}")
      {:ok, conn} = Mint.HTTP.stream_request_body(conn, request_ref, :eof)

  Here's an example of sending trailer headers:

      headers = [{"content-type", "application/json"}, {"trailer", "my-trailer, x-expires"}]
      {:ok, conn, request_ref} = Mint.HTTP.request(conn, "POST", "/", headers, :stream)

      {:ok, conn} = Mint.HTTP.stream_request_body(conn, request_ref, "{}")

      trailer_headers = [{"my-trailer", "xxx"}, {"x-expires", "10 days"}]
      {:ok, conn} = Mint.HTTP.stream_request_body(conn, request_ref, {:eof, trailer_headers})

  """
  @impl true
  @spec stream_request_body(
          t(),
          Types.request_ref(),
          iodata() | :eof | {:eof, trailer_headers :: Types.headers()}
        ) ::
          {:ok, t()} | {:error, t(), Types.error()}
  def stream_request_body(conn, ref, body),
    do: conn_apply(conn, :stream_request_body, [conn, ref, body])

  @doc """
  Streams the next batch of responses from the given `message`.

  This function processes a "message" which can be any term, but should be
  a message received by the process that owns the connection. **Processing**
  a message means that this function will parse it and check if it's a message
  that is directed to this connection, that is, a TCP/SSL message received on the
  connection's socket. If it is, then this function will parse the message,
  turn it into a list of responses, and possibly take action given the responses.
  As an example of an action that this function could perform, if the server sends
  a ping request this function will transparently take care of pinging the server back.

  If there's no error, this function returns `{:ok, conn, responses}` where `conn` is
  the updated connection and `responses` is a list of responses. See the "Responses"
  section below. If there's an error, `{:error, conn, reason, responses}` is returned,
  where `conn` is the updated connection, `reason` is the error reason, and `responses`
  is a list of responses that were correctly parsed before the error.

  > #### Graceful Close {: .tip}
  >
  > If this function returns `{:ok, conn, responses}`, it doesn't *necessarily* mean
  > that the connection is still open. For example, TCP/SSL **close** messages are treated
  > as errors only if there are in-flight requests. If there are no in-flight requests,
  > the connection is closed gracefully and `{:ok, conn, responses}` is returned.
  > Always check with `open?/1` to see if the connection is still open.

  If the given `message` is not from the connection's socket,
  this function returns `:unknown`.

  > #### Receiving Multiple Messages {: .warning}
  >
  > Your connection and the HTTP server can exchange multiple **protocol-specific messages**
  > on the socket that don't necessarily *produce responses*. For example, the HTTP server
  > might tell the connection to update some internal settings. For this reason, you
  > should always receive as many messages coming to your process as possible, for example
  > by using `receive` recursively. You can see an example of this approach in the
  > ["Usage Examples" documentation](architecture.html#usage-examples).

  ## Socket mode

  Mint sets the socket in `active: :once` mode. This means that a single socket
  message at a time is delivered to the process that owns the connection. After
  a message is delivered, then no other messages are delivered (we say the socket
  goes in *passive* mode). When `stream/2` is called to process the message that
  was received, Mint sets the socket back to `active: :once`. This is good to know
  in order to understand how the socket is handled by Mint, but in normal usage
  it just means that you will process one message at a time with `stream/2` and not
  pay too much attention to the socket mode.

  Mint also supports passive mode to avoid receiving messages. See the "Mode" section
  in the module documentation.

  ## Responses

  Each possible response returned by this function is a tuple with two or more elements.
  The first element is always an atom that identifies the kind of response. The second
  element is a unique reference `t:Mint.Types.request_ref/0` that identifies the request
  that the response belongs to. This is the term returned by `request/5`. After these
  two elements, there can be response-specific terms as well, documented below.

  These are the possible responses that can be returned.

    * `{:status, request_ref, status_code}` - returned when the server replied
      with a response status code. The status code is a non-negative integer.
      You can have zero or more `1xx` `:status` and `:headers` responses for a
      single request, but they all precede a single non-`1xx` `:status` response.

    * `{:headers, request_ref, headers}` - returned when the server replied
      with a list of headers. Headers are in the form `{header_name, header_value}`
      with `header_name` and `header_value` being strings. A single `:headers` response
      will come after the `:status` response. A single `:headers` response may come
      after all the `:data` responses if **trailer headers** are present.

    * `{:data, request_ref, binary}` - returned when the server replied with
      a chunk of response body (as a binary). The request shouldn't be considered done
      when a piece of body is received because multiple chunks could be received. The
      request is done when the `:done` response is returned.

    * `{:done, request_ref}` - returned when the server signaled the request
      as done. When this is received, the response body and headers can be considered
      complete and it can be assumed that no more responses will be received for this
      request. This means that for example, you can stop holding on to the request ref
      for this request.

    * `{:error, request_ref, reason}` - returned when there is an error that
      only affects the request and not the whole connection. For example, if the
      server sends bad data on a given request, that request will be closed and an error
      for that request will be returned among the responses, but the connection will
      remain alive and well.

    * `{:pong, request_ref}` - returned when a server replies to a ping
      request sent by the client. This response type is HTTP/2-specific
      and will never be returned by an HTTP/1 connection. See `Mint.HTTP2.ping/2`
      for more information.

    * `{:push_promise, request_ref, promised_request_ref, headers}` - returned when
      the server sends a server push to the client. This response type is HTTP/2 specific
      and will never be returned by an HTTP/1 connection. See `Mint.HTTP2` for more
      information on server pushes.

  ## Examples

  Let's assume we have a function called `receive_next_and_stream/1` that takes
  a connection and then receives the next message, calls `stream/2` with that message
  as an argument, and then returns the result of `stream/2`:

      defp receive_next_and_stream(conn) do
        receive do
          message -> Mint.HTTP.stream(conn, message)
        end
      end

  Now, we can see an example of a workflow involving `stream/2`.

      {:ok, conn, request_ref} = Mint.HTTP.request(conn, "GET", "/", _headers = [])

      {:ok, conn, responses} = receive_next_and_stream(conn)
      responses
      #=> [{:status, ^request_ref, 200}]

      {:ok, conn, responses} = receive_next_and_stream(conn)
      responses
      #=> [{:headers, ^request_ref, [{"Content-Type", "application/json"}]},
      #=>  {:data, ^request_ref, "{"}]

      {:ok, conn, responses} = receive_next_and_stream(conn)
      responses
      #=> [{:data, ^request_ref, "}"}, {:done, ^request_ref}]

  """
  @impl true
  @spec stream(t(), term()) ::
          {:ok, t(), [Types.response()]}
          | {:error, t(), Types.error(), [Types.response()]}
          | :unknown
  def stream(conn, message), do: conn_apply(conn, :stream, [conn, message])

  @doc """
  Returns the number of open requests.

  Open requests are requests that have not yet received a `:done` response.
  This function returns the number of open requests for both HTTP/1 and HTTP/2,
  but for HTTP/2 only client-initiated requests are considered as open requests.
  See `Mint.HTTP2.open_request_count/1` for more information.

  ## Examples

      {:ok, conn, _ref} = Mint.HTTP.request(conn, "GET", "/", [])
      Mint.HTTP.open_request_count(conn)
      #=> 1

  """
  @impl true
  @spec open_request_count(t()) :: non_neg_integer()
  def open_request_count(conn), do: conn_apply(conn, :open_request_count, [conn])

  @doc """
  Receives data from the socket in a blocking way.

  By default Mint operates in active mode, meaning that messages are delivered
  to the process that started the connection. However, Mint also supports passive
  mode (see the "Mode" section in the module documentation).

  In passive mode, you'll need to manually get bytes out of the socket. You can
  do that with this function.

  `byte_count` is the number of bytes you want out of the socket. If `byte_count`
  is `0`, all available bytes will be returned.

  `timeout` is the maximum time to wait before returning an error.

  This function will raise an error if the socket is in active mode.

  > #### Hanging Waiting for Bytes {: .warning}
  >
  > If `byte_count` is greater than `0` and the socket doesn't receive
  > *at least* `byte_count` bytes withing the `timeout`, then the function
  > will block for the duration of `timeout` and then return a timeout error.
  > This behavior is the same as the `recv` function in [`:gen_tcp`](`:gen_tcp`)
  > and [`:ssl`](`:ssl`).

  ## Examples

      {:ok, conn, responses} = Mint.HTTP.recv(conn, 0, 5000)

  """
  @impl true
  @spec recv(t(), non_neg_integer(), timeout()) ::
          {:ok, t(), [Types.response()]}
          | {:error, t(), Types.error(), [Types.response()]}
  def recv(conn, byte_count, timeout), do: conn_apply(conn, :recv, [conn, byte_count, timeout])

  @doc """
  Changes the mode of the underlying socket.

  To use the connection in *active mode*, where the process that started the
  connection receives socket messages, set the mode to `:active` (see also `stream/2`).
  To use the connection in *passive mode*, where you need to manually receive data
  from the socket, set the mode to `:passive` (see also `recv/3`).

  The mode can also be controlled at connection time by the `:mode` option passed
  to `connect/4`.

  Note that if you're switching from active to passive mode, you still might have
  socket messages in the process mailbox that you need to consume before doing
  any other operation on the connection.

  See the "Mode" section in the module documentation for more information on modes.

  ## Examples

      {:ok, conn} = Mint.HTTP.set_mode(conn, :passive)

  """
  @impl true
  @spec set_mode(t(), :active | :passive) :: {:ok, t()} | {:error, Types.error()}
  def set_mode(conn, mode), do: conn_apply(conn, :set_mode, [conn, mode])

  @doc """
  Changes the *controlling process* of the given connection to `new_pid`.

  The **controlling process** is a concept that comes from the Erlang TCP and
  SSL implementations. The controlling process of a connection is the process
  that started the connection and that receives the messages for that connection.
  You can change the controlling process of a connection through this function.

  This function also takes care of "transferring" all the connection messages
  that are in the mailbox of the current controlling process to the new
  controlling process.

  Remember that the connection is a data structure, so if you
  change the controlling process it doesn't mean you "transferred" the
  connection data structure itself to the other process, which you have
  to do manually (for example by sending the connection data structure to the
  new controlling process). If you do that, be careful of race conditions
  and be sure to retrieve the connection in the new controlling process
  before accepting connection messages in the new controlling process.
  In fact, this function is guaranteed to return the connection unchanged,
  so you are free to ignore the connection entry returned in `{:ok, conn}`.

  ## Examples

      send(new_pid, {:conn, conn})
      {:ok, conn} = Mint.HTTP.controlling_process(conn, new_pid)

      # In the "new_pid" process
      receive do
        {:conn, conn} ->
          # Will receive connection messages.
      end

  """
  @impl true
  @spec controlling_process(t(), pid()) :: {:ok, t()} | {:error, Types.error()}
  def controlling_process(conn, new_pid),
    do: conn_apply(conn, :controlling_process, [conn, new_pid])

  @doc """
  Assigns a new private key and value in the connection.

  This storage is meant to be used to associate metadata with the connection and
  it can be useful when handling multiple connections.

  The given `key` must be an atom, while the given `value` can be an arbitrary
  term. The return value of this function is an updated connection.

  See also `get_private/3` and `delete_private/2`.

  ## Examples

  Let's see an example of putting a value and then getting it:

      conn = Mint.HTTP.put_private(conn, :client_name, "Mint")
      Mint.HTTP.get_private(conn, :client_name)
      #=> "Mint"

  """
  @impl true
  @spec put_private(t(), atom(), term()) :: t()
  def put_private(conn, key, value), do: conn_apply(conn, :put_private, [conn, key, value])

  @doc """
  Gets a private value from the connection.

  Retrieves a private value previously set with `put_private/3` from the connection.
  `key` is the key under which the value to retrieve is stored. `default` is a default
  value returned in case there's no value under the given key.

  See also `put_private/3` and `delete_private/2`.

  ## Examples

      conn = Mint.HTTP.put_private(conn, :client_name, "Mint")

      Mint.HTTP.get_private(conn, :client_name)
      #=> "Mint"

      Mint.HTTP.get_private(conn, :non_existent)
      #=> nil

  """
  @impl true
  @spec get_private(t(), atom(), term()) :: term()
  def get_private(conn, key, default \\ nil),
    do: conn_apply(conn, :get_private, [conn, key, default])

  @doc """
  Deletes a value in the private store.

  Deletes the private value stored under `key` in the connection. Returns the
  updated connection.

  See also `put_private/3` and `get_private/3`.

  ## Examples

      conn = Mint.HTTP.put_private(conn, :client_name, "Mint")

      Mint.HTTP.get_private(conn, :client_name)
      #=> "Mint"

      conn = Mint.HTTP.delete_private(conn, :client_name)
      Mint.HTTP.get_private(conn, :client_name)
      #=> nil

  """
  @impl true
  @spec delete_private(t(), atom()) :: t()
  def delete_private(conn, key), do: conn_apply(conn, :delete_private, [conn, key])

  @doc """
  Gets the socket associated with the connection.

  Do not use the returned socket to change its internal state. Only read information from the socket.
  For instance, use `:ssl.connection_information/2` to retrieve TLS-specific information from the
  socket.
  """
  @impl true
  @spec get_socket(t()) :: Mint.Types.socket()
  def get_socket(conn), do: conn_apply(conn, :get_socket, [conn])

  @doc """
  Sets whether the connection should log information or not.

  See the ["Logging" section](#module-logging) in the module documentation for more information.
  """
  @doc since: "1.5.0"
  @impl true
  @spec put_log(t(), boolean()) :: t()
  def put_log(conn, log?), do: conn_apply(conn, :put_log, [conn, log?])

  @doc """
  Gets the proxy headers associated with the connection in the `CONNECT` method.

  When using tunnel proxy and HTTPs, the only way to exchange data with
  the proxy is through headers in the `CONNECT` method.
  """
  @doc since: "1.4.0"
  @impl true
  @spec get_proxy_headers(t()) :: Mint.Types.headers()
  def get_proxy_headers(conn), do: conn_apply(conn, :get_proxy_headers, [conn])

  # Made public since the struct is opaque.
  @doc false
  @impl true
  def put_proxy_headers(conn, headers), do: conn_apply(conn, :put_proxy_headers, [conn, headers])

  ## Helpers

  defp conn_apply(%UnsafeProxy{}, fun, args), do: apply(UnsafeProxy, fun, args)
  defp conn_apply(%Mint.HTTP1{}, fun, args), do: apply(Mint.HTTP1, fun, args)
  defp conn_apply(%Mint.HTTP2{}, fun, args), do: apply(Mint.HTTP2, fun, args)
end
