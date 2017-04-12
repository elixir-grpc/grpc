defmodule GRPC.Channel do
  @moduledoc """
  Defines a struct to store the connection data. which should be passed to
  generated RPC functions as first argument:

      Greeter.Stub.say_hello(channel, request)

  ## Fields

    * `:host` - server's host to connect
    * `:port` - server's port to connect
    * `:scheme` - scheme of connection, like `http`
    * `:adapter` - a client adapter module, like `GRPC.Adapter.Chatterbox.Client`
    * `:payload` - the payload needed by the adapter
  """

  @type t :: %__MODULE__{
    host: String.t,
    port: non_neg_integer,
    scheme: String.t,
    adapter: atom,
    payload: %{atom => any}
  }
  defstruct [host: nil, port: nil, scheme: nil, adapter: nil, payload: %{}]

  @default_adapter GRPC.Adapter.Chatterbox.Client
  @insecure_scheme "http"
  @secure_scheme "https"

  @doc false
  @spec connect(String.t, keyword) :: {:ok, t}
  def connect(addr, opts) when is_binary(addr) do
    [host, port] = String.split(addr, ":")
    connect(host, port, opts)
  end

  @doc false
  @spec connect(String.t, non_neg_integer, keyword) :: {:ok, t}
  def connect(host, port, opts) when is_binary(port) do
    connect(host, String.to_integer(port), opts)
  end
  def connect(host, port, opts) when is_integer(port) do
    adapter = Keyword.get(opts, :adapter, @default_adapter)
    scheme = if opts[:cred], do: @secure_scheme, else: @insecure_scheme
    channel = %__MODULE__{host: host, port: port, scheme: scheme, adapter: adapter}
    {:ok, payload} = adapter.connect(channel, %{cred: opts[:cred]})
    channel = %{channel | payload: payload}
    {:ok, channel}
  end

  @doc false
  @spec unary(GRPC.Client.Stream.t, struct, keyword) :: any
  def unary(%{channel: channel} = stream, message, opts) do
    channel.adapter.unary(stream, message, opts)
  end

  @doc false
  @spec send_request(GRPC.Client.Stream.t, struct, keyword) :: any
  def send_request(%{channel: channel} = stream, message, opts) do
    channel.adapter.send_request(stream, message, opts)
  end

  @doc false
  @spec send_header(GRPC.Client.Stream.t, keyword) :: any
  def send_header(%{channel: channel} = stream, opts) do
    channel.adapter.send_header(stream, opts)
  end

  @doc false
  @spec send_body(GRPC.Client.Stream.t, struct, keyword) :: any
  def send_body(%{channel: channel} = stream, message, opts) do
    channel.adapter.send_body(stream, message, opts)
  end

  @doc false
  @spec recv_end(GRPC.Client.Stream.t, keyword) :: any
  def recv_end(%{channel: channel} = stream, opts) do
    channel.adapter.recv_end(stream, opts)
  end

  @doc false
  @spec recv(GRPC.Client.Stream.t, keyword) :: any
  def recv(%{channel: channel} = stream, opts) do
    channel.adapter.recv(stream, opts)
  end
end
