defmodule Mint.Types do
  @moduledoc """
  HTTP-related types.
  """

  @typedoc """
  A hostname, IP address, Unix domain socket path, `:loopback`, or any
  other term representing an internet address.
  """
  @type address() :: :inet.socket_address() | String.t()

  @typedoc """
  A request reference that uniquely identifies a request.

  Responses for a request are always tagged with a request reference so that you
  can connect each response to the right request. Also see `Mint.HTTP.request/5`.
  """
  @type request_ref() :: reference()

  @typedoc """
  An HTTP/2-specific response to a request.

  This type of response is only returned on HTTP/2 connections. See `t:response/0` for
  more response types.
  """
  @type http2_response() ::
          {:pong, request_ref()}
          | {:push_promise, request_ref(), promised_request_ref :: request_ref(), headers()}

  @typedoc """
  A response to a request.

  Terms of this type are returned as responses to requests. See `Mint.HTTP.stream/2`
  for more information.
  """
  @type response() ::
          {:status, request_ref(), status()}
          | {:headers, request_ref(), headers()}
          | {:data, request_ref(), body_chunk :: binary()}
          | {:done, request_ref()}
          | {:error, request_ref(), reason :: term()}
          | http2_response()

  @typedoc """
  An HTTP status code.

  The type for an HTTP is a generic non-negative integer since we don't formally check that
  the response code is in the "common" range (`200..599`).
  """
  @type status() :: non_neg_integer()

  @typedoc """
  HTTP headers.

  Headers are sent and received as lists of two-element tuples containing two strings,
  the header name and header value.
  """
  @type headers() :: [{header_name :: String.t(), header_value :: String.t()}]

  @typedoc """
  The scheme to use when connecting to an HTTP server.
  """
  @type scheme() :: :http | :https

  @typedoc """
  An error reason.
  """
  @type error() :: Mint.TransportError.t() | Mint.HTTPError.t()

  @typedoc """
  The connection socket.
  """
  @type socket() :: term()
end
