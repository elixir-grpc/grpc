defmodule Mint.TransportError do
  @moduledoc """
  Represents an error with the transport used by an HTTP connection.

  A `Mint.TransportError` struct is an exception, so it can be raised as any
  other exception.

  ## Struct fields

  This exception represents an error with the transport (TCP or SSL) used
  by an HTTP connection. The exception struct itself is opaque, that is,
  not all fields are public. The following are the public fields:

    * `:reason` - a term representing the error reason. The value of this field
      can be:

        * `:timeout` - if there's a timeout in interacting with the socket.

        * `:closed` - if the connection has been closed.

        * `:protocol_not_negotiated` - if the ALPN protocol negotiation failed.

        * `{:bad_alpn_protocol, protocol}` - when the ALPN protocol is not
          one of the supported protocols, which are `http/1.1` and `h2`.

        * `t::inet.posix/0` - if there's any other error with the socket,
          such as `:econnrefused` or `:nxdomain`.

        * `t::ssl.error_alert/0` - if there's an SSL error.

  ## Message representation

  If you want to convert an error reason to a human-friendly message (for example
  for using in logs), you can use `Exception.message/1`:

      iex> {:error, %Mint.TransportError{} = error} = Mint.HTTP.connect(:http, "nonexistent", 80)
      iex> Exception.message(error)
      "non-existing domain"

  """

  reason_type =
    quote do
      :timeout
      | :closed
      | :protocol_not_negotiated
      | {:bad_alpn_protocol, String.t()}
      | :inet.posix()
    end

  reason_type =
    if System.otp_release() >= "21" do
      quote do: unquote(reason_type) | :ssl.error_alert()
    else
      reason_type
    end

  @type t() :: %__MODULE__{reason: unquote(reason_type) | term()}

  defexception [:reason]

  def message(%__MODULE__{reason: reason}) do
    format_reason(reason)
  end

  ## Our reasons.

  defp format_reason(:protocol_not_negotiated) do
    "ALPN protocol not negotiated"
  end

  defp format_reason({:bad_alpn_protocol, protocol}) do
    "bad ALPN protocol #{inspect(protocol)}, supported protocols are \"http/1.1\" and \"h2\""
  end

  defp format_reason(:closed) do
    "socket closed"
  end

  defp format_reason(:timeout) do
    "timeout"
  end

  # :ssl.format_error/1 falls back to :inet.format_error/1 when the error is not an SSL-specific
  # error (at least since OTP 19+), so we can just use that.
  defp format_reason(reason) do
    case :ssl.format_error(reason) do
      ~c"Unexpected error:" ++ _ -> inspect(reason)
      message -> List.to_string(message)
    end
  end
end
