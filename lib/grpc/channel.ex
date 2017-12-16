defmodule GRPC.Channel do
  @moduledoc """
  Defines a struct to store the connection data. which should be passed to
  generated RPC functions as first argument:

      Greeter.Stub.say_hello(channel, request)

  ## Fields

    * `:host` - server's host to connect
    * `:port` - server's port to connect
    * `:scheme` - scheme of connection, like `http`
    * `:cred` - credentials used for authentication
    * `:adapter` - a client adapter module, like `GRPC.Adapter.Chatterbox.Client`
  """

  @type t :: %__MODULE__{
    host: String.t,
    port: non_neg_integer,
    scheme: String.t,
    cred: GRPC.Credential.t,
    adapter: atom
  }
  defstruct [host: nil, port: nil, scheme: nil, cred: nil, adapter: nil]

  @default_adapter GRPC.Adapter.Chatterbox.Client
  @insecure_scheme "http"
  @secure_scheme "https"

  @doc false
  @spec connect(String.t(), keyword) :: {:ok, t}
  def connect(addr, opts) when is_binary(addr) do
    [host, port] = String.split(addr, ":")
    connect(host, port, opts)
  end

  @doc false
  @spec connect(String.t, binary | non_neg_integer, keyword) :: {:ok, t} | {:error, any}
  def connect(host, port, opts) when is_binary(port) do
    connect(host, String.to_integer(port), opts)
  end

  def connect(host, port, opts) when is_integer(port) do
    adapter = Keyword.get(opts, :adapter, @default_adapter)
    cred = Keyword.get(opts, :cred)
    scheme = if cred, do: @secure_scheme, else: @insecure_scheme
    %__MODULE__{host: host, port: port, scheme: scheme,
                cred: cred, adapter: adapter}
    |> adapter.connect()
  end

  @doc false
  @spec unary(GRPC.Client.Stream.t(), struct, keyword) :: any
  def unary(%{channel: channel} = stream, message, opts) do
    channel.adapter.unary(stream, message, opts)
  end

  @doc false
  @spec send_request(GRPC.Client.Stream.t(), struct, keyword) :: any
  def send_request(%{channel: channel} = stream, message, opts) do
    channel.adapter.send_request(stream, message, opts)
  end

  @doc false
  @spec send_header(GRPC.Client.Stream.t(), keyword) :: any
  def send_header(%{channel: channel} = stream, opts) do
    channel.adapter.send_header(stream, opts)
  end

  @doc false
  @spec send_body(GRPC.Client.Stream.t, binary, keyword) :: any
  def send_body(%{channel: channel} = stream, message, opts) do
    channel.adapter.send_body(stream, message, opts)
  end

  @doc false
  @spec recv_end(GRPC.Client.Stream.t(), keyword) :: any
  def recv_end(%{channel: channel} = stream, opts) do
    channel.adapter.recv_end(stream, opts)
  end

  @doc false
  @spec recv(GRPC.Client.Stream.t(), keyword) :: any
  def recv(%{channel: channel} = stream, opts) do
    channel.adapter.recv(stream, opts)
  end
end
