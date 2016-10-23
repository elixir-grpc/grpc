defmodule GRPC.Channel do
  defstruct [host: nil, port: nil, scheme: nil, adapter: nil, payload: %{}]

  @default_adapter GRPC.Adapter.Chatterbox.Client
  @insecure_scheme "http"

  @spec connect(String.t, Integer.t, Keyword.t) :: {:ok, struct} | {:error, any}
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

  def unary(%{channel: channel} = stream, message, opts) do
    channel.adapter.unary(stream, message, opts)
  end

  def send_request(%{channel: channel} = stream, message, opts) do
    channel.adapter.send_request(stream, message, opts)
  end

  def send_header(%{channel: channel} = stream, opts) do
    channel.adapter.send_header(stream, opts)
  end

  def send_body(%{channel: channel} = stream, message, opts) do
    channel.adapter.send_body(stream, message, opts)
  end

  def recv_end(%{channel: channel} = stream, opts) do
    channel.adapter.recv_end(stream, opts)
  end

  def recv(%{channel: channel} = stream, opts) do
    channel.adapter.recv(stream, opts)
  end
end
