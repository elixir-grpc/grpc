defmodule Mint.UnsafeProxy do
  @moduledoc false

  alias Mint.{Types, UnsafeProxy}

  @behaviour Mint.Core.Conn

  defstruct [
    :hostname,
    :port,
    :scheme,
    :module,
    :proxy_headers,
    :state
  ]

  @opaque t() :: %UnsafeProxy{}

  @type host_triple() :: {Types.scheme(), address :: Types.address(), :inet.port_number()}

  @spec connect(host_triple(), host_triple(), opts :: keyword()) ::
          {:ok, t()} | {:error, Types.error()}
  def connect(proxy, host, opts \\ []) do
    {proxy_scheme, proxy_address, proxy_port} = proxy
    {scheme, address, port} = host
    hostname = Mint.Core.Util.hostname(opts, address)

    with {:ok, state} <- Mint.HTTP1.connect(proxy_scheme, proxy_address, proxy_port, opts) do
      conn = %UnsafeProxy{
        scheme: scheme,
        hostname: hostname,
        port: port,
        module: Mint.HTTP1,
        proxy_headers: Keyword.get(opts, :proxy_headers, []),
        state: state
      }

      {:ok, conn}
    end
  end

  @impl true
  @spec initiate(
          module(),
          Mint.Types.socket(),
          String.t(),
          :inet.port_number(),
          keyword()
        ) :: no_return()
  def initiate(_transport, _transport_state, _hostname, _port, _opts) do
    raise "initiate/5 does not apply for #{inspect(__MODULE__)}"
  end

  @impl true
  @spec close(t()) :: {:ok, t()}
  def close(%UnsafeProxy{module: module, state: state} = _conn) do
    module.close(state)
  end

  @impl true
  @spec open?(t(), :read | :write) :: boolean()
  def open?(%UnsafeProxy{module: module, state: state}, type \\ :write) do
    module.open?(state, type)
  end

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
  def request(
        %UnsafeProxy{module: module, state: state} = conn,
        method,
        path,
        headers,
        body \\ nil
      ) do
    path = request_line(conn, path)
    headers = headers ++ conn.proxy_headers

    case module.request(state, method, path, headers, body) do
      {:ok, state, request} -> {:ok, %{conn | state: state}, request}
      {:error, state, reason} -> {:error, %{conn | state: state}, reason}
    end
  end

  @impl true
  @spec stream_request_body(
          t(),
          Types.request_ref(),
          iodata() | :eof | {:eof, trailer_headers :: Types.headers()}
        ) ::
          {:ok, t()} | {:error, t(), Types.error()}
  def stream_request_body(%UnsafeProxy{module: module, state: state} = conn, ref, body) do
    case module.stream_request_body(state, ref, body) do
      {:ok, state} -> {:ok, %{conn | state: state}}
      {:error, state, reason} -> {:error, %{conn | state: state}, reason}
    end
  end

  @impl true
  @spec stream(t(), term()) ::
          {:ok, t(), [Types.response()]}
          | {:error, t(), Types.error(), [Types.response()]}
          | :unknown
  def stream(%UnsafeProxy{module: module, state: state} = conn, message) do
    case module.stream(state, message) do
      {:ok, state, responses} -> {:ok, %{conn | state: state}, responses}
      {:error, state, reason, responses} -> {:error, %{conn | state: state}, reason, responses}
      :unknown -> :unknown
    end
  end

  @impl true
  @spec open_request_count(t()) :: non_neg_integer()
  def open_request_count(%UnsafeProxy{module: module, state: state} = _conn) do
    module.open_request_count(state)
  end

  @impl true
  @spec recv(t(), non_neg_integer(), timeout()) ::
          {:ok, t(), [Types.response()]}
          | {:error, t(), Types.error(), [Types.response()]}
  def recv(%UnsafeProxy{module: module, state: state} = conn, byte_count, timeout) do
    case module.recv(state, byte_count, timeout) do
      {:ok, state, responses} -> {:ok, %{conn | state: state}, responses}
      {:error, state, reason, responses} -> {:error, %{conn | state: state}, reason, responses}
    end
  end

  @impl true
  @spec set_mode(t(), :active | :passive) :: {:ok, t()} | {:error, Types.error()}
  def set_mode(%UnsafeProxy{module: module, state: state} = conn, mode) do
    with {:ok, state} <- module.set_mode(state, mode) do
      {:ok, %{conn | state: state}}
    end
  end

  @impl true
  @spec controlling_process(t(), pid()) :: {:ok, t()} | {:error, Types.error()}
  def controlling_process(%UnsafeProxy{module: module, state: state} = conn, new_pid) do
    with {:ok, _} <- module.controlling_process(state, new_pid) do
      {:ok, conn}
    end
  end

  @impl true
  @spec put_private(t(), atom(), term()) :: t()
  def put_private(%UnsafeProxy{module: module, state: state} = conn, key, value) do
    state = module.put_private(state, key, value)
    %{conn | state: state}
  end

  @impl true
  @spec get_private(t(), atom(), term()) :: term()
  def get_private(%UnsafeProxy{module: module, state: state}, key, default \\ nil) do
    module.get_private(state, key, default)
  end

  @impl true
  @spec delete_private(t(), atom()) :: t()
  def delete_private(%UnsafeProxy{module: module, state: state} = conn, key) do
    state = module.delete_private(state, key)
    %{conn | state: state}
  end

  defp request_line(%UnsafeProxy{scheme: scheme, hostname: hostname, port: port}, path) do
    %URI{scheme: Atom.to_string(scheme), host: hostname, port: port, path: path}
    |> URI.to_string()
  end

  @impl true
  @spec get_socket(t()) :: Mint.Types.socket()
  def get_socket(%UnsafeProxy{module: module, state: state}) do
    module.get_socket(state)
  end

  @impl true
  @spec put_log(t(), boolean()) :: t()
  def put_log(%UnsafeProxy{module: module, state: state} = conn, log) do
    state = module.put_log(state, log)
    %{conn | state: state}
  end

  # The `%__MODULE__{proxy_headers: value}` here is the request headers,
  # not the proxy response ones. Unsafe proxy mixes its headers (if any)
  # with the regular response headers, so you can get them there.
  @impl true
  @spec get_proxy_headers(t()) :: Mint.Types.headers()
  def get_proxy_headers(%__MODULE__{}), do: []

  @impl true
  @spec put_proxy_headers(t(), Mint.Types.headers()) :: t()
  def put_proxy_headers(%__MODULE__{}, _headers) do
    raise "invalid function for proxy unsafe proxy connections"
  end
end
