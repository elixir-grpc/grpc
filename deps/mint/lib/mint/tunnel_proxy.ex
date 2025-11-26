defmodule Mint.TunnelProxy do
  @moduledoc false

  alias Mint.{HTTP, HTTP1, HTTPError, Negotiate, TransportError}

  @tunnel_timeout 30_000

  @spec connect(tuple(), tuple()) :: {:ok, Mint.HTTP.t()} | {:error, term()}
  def connect(proxy, host) do
    case establish_proxy(proxy, host) do
      {:ok, conn} -> upgrade_connection(conn, proxy, host)
      {:error, reason} -> {:error, reason}
    end
  end

  defp establish_proxy(proxy, host) do
    {proxy_scheme, proxy_address, proxy_port, proxy_opts} = proxy
    {_scheme, address, port, opts} = host
    hostname = Mint.Core.Util.hostname(opts, address)

    path = "#{hostname}:#{port}"

    with {:ok, conn} <- HTTP1.connect(proxy_scheme, proxy_address, proxy_port, proxy_opts),
         timeout_deadline = timeout_deadline(proxy_opts),
         headers = Keyword.get(opts, :proxy_headers, []),
         {:ok, conn, ref} <- HTTP1.request(conn, "CONNECT", path, headers, nil),
         {:ok, proxy_headers} <- receive_response(conn, ref, timeout_deadline) do
      {:ok, HTTP1.put_proxy_headers(conn, proxy_headers)}
    else
      {:error, reason} ->
        {:error, wrap_in_proxy_error(reason)}

      {:error, conn, reason} ->
        {:ok, _conn} = HTTP1.close(conn)
        {:error, wrap_in_proxy_error(reason)}
    end
  end

  defp upgrade_connection(
         conn,
         {proxy_scheme, _proxy_address, _proxy_port, _proxy_opts} = _proxy,
         {scheme, hostname, port, opts} = _host
       ) do
    proxy_headers = HTTP1.get_proxy_headers(conn)
    socket = HTTP1.get_socket(conn)

    # Note that we may leak messages if the server sent data after the CONNECT response
    case Negotiate.upgrade(proxy_scheme, socket, scheme, hostname, port, opts) do
      {:ok, conn} -> {:ok, HTTP.put_proxy_headers(conn, proxy_headers)}
      {:error, reason} -> {:error, wrap_in_proxy_error(reason)}
    end
  end

  defp receive_response(conn, ref, timeout_deadline) do
    timeout = timeout_deadline - System.monotonic_time(:millisecond)
    socket = HTTP1.get_socket(conn)

    receive do
      {tag, ^socket, _data} = msg when tag in [:tcp, :ssl] ->
        stream(conn, ref, timeout_deadline, msg)

      {tag, ^socket} = msg when tag in [:tcp_closed, :ssl_closed] ->
        stream(conn, ref, timeout_deadline, msg)

      {tag, ^socket, _reason} = msg when tag in [:tcp_error, :ssl_error] ->
        stream(conn, ref, timeout_deadline, msg)
    after
      timeout ->
        {:error, conn, wrap_error({:proxy, :tunnel_timeout})}
    end
  end

  defp stream(conn, ref, timeout_deadline, msg) do
    case HTTP1.stream(conn, msg) do
      {:ok, conn, responses} ->
        case handle_responses(ref, timeout_deadline, responses) do
          {:done, proxy_headers} -> {:ok, proxy_headers}
          :more -> receive_response(conn, ref, timeout_deadline)
          {:error, reason} -> {:error, conn, reason}
        end

      {:error, conn, reason, _responses} ->
        {:error, conn, wrap_in_proxy_error(reason)}
    end
  end

  defp handle_responses(ref, timeout_deadline, [response | responses]) do
    case response do
      {:status, ^ref, status} when status in 200..299 ->
        handle_responses(ref, timeout_deadline, responses)

      {:status, ^ref, status} ->
        {:error, wrap_error({:proxy, {:unexpected_status, status}})}

      {:headers, ^ref, headers} when responses == [] ->
        {:done, headers}

      {:headers, ^ref, _headers} ->
        {:error, wrap_error({:proxy, {:unexpected_trailing_responses, responses}})}

      {:error, ^ref, reason} ->
        {:error, wrap_in_proxy_error(reason)}
    end
  end

  defp handle_responses(_ref, _timeout_deadline, []) do
    :more
  end

  defp timeout_deadline(opts) do
    timeout = Keyword.get(opts, :tunnel_timeout, @tunnel_timeout)
    System.monotonic_time(:millisecond) + timeout
  end

  defp wrap_error(reason) do
    %HTTPError{module: __MODULE__, reason: reason}
  end

  defp wrap_in_proxy_error(%HTTPError{reason: {:proxy, _}} = error) do
    error
  end

  defp wrap_in_proxy_error(%HTTPError{reason: reason}) do
    %HTTPError{module: __MODULE__, reason: {:proxy, reason}}
  end

  defp wrap_in_proxy_error(%TransportError{} = error) do
    error
  end

  @doc false
  def format_error({:proxy, reason}) do
    case reason do
      :tunnel_timeout ->
        "proxy tunnel timeout"

      {:unexpected_status, status} ->
        "expected tunnel proxy to return a status between 200 and 299, got: #{inspect(status)}"

      {:unexpected_trailing_responses, responses} ->
        "tunnel proxy returned unexpected trailer responses: #{inspect(responses)}"

      http_reason ->
        "error when establishing the tunnel proxy connection: " <>
          HTTP1.format_error(http_reason)
    end
  end
end
