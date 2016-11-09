defmodule GRPC.Channel do
  @type t :: %__MODULE__{
    host: String.t,
    port: non_neg_integer,
    scheme: String.t,
    adapter: atom,
    payload: %{atom => any}
  }
  # `payload` is used to store Data related to adapter
  defstruct [host: nil, port: nil, scheme: nil, adapter: nil, payload: %{}]

  @default_adapter GRPC.Adapter.Chatterbox.Client
  @insecure_scheme "http"

  @moduledoc """
  Channel is used to connect to s gRPC server for clients.

  It's also a struct to store connection data, which should be passed to gRPC
  functions as first argument.
  """

  @doc false
  def connect(addr, opts) when is_binary(addr) do
    [host, port] = String.split(addr, ":")
    connect(host, port, opts)
  end
  def connect(host, port, opts) when is_binary(port) do
    connect(host, String.to_integer(port), opts)
  end
  def connect(host, port, opts) when is_integer(port) do
    channel = %__MODULE__{host: host, port: port}
    adapter = Keyword.get(opts, :adapter, @default_adapter)
    channel =
      if opts[:insecure] do
        {:ok, payload} = adapter.connect_insecurely(channel)
        %{channel | scheme: @insecure_scheme, adapter: adapter, payload: payload}
      else
        # TODO: Secure connection
      end
    {:ok, channel}
  end

  @doc false
  def unary(%{channel: channel} = stream, message, opts) do
    channel.adapter.unary(stream, message, opts)
  end

  @doc false
  def send_request(%{channel: channel} = stream, message, opts) do
    channel.adapter.send_request(stream, message, opts)
  end

  @doc false
  def send_header(%{channel: channel} = stream, opts) do
    channel.adapter.send_header(stream, opts)
  end

  @doc false
  def send_body(%{channel: channel} = stream, message, opts) do
    channel.adapter.send_body(stream, message, opts)
  end

  @doc false
  def recv_end(%{channel: channel} = stream, opts) do
    channel.adapter.recv_end(stream, opts)
  end

  @doc false
  def recv(%{channel: channel} = stream, opts) do
    channel.adapter.recv(stream, opts)
  end
end
