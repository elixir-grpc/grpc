defmodule GRPC.Test.ClientAdapter do
  def connect(channel, _opts), do: {:ok, channel}
end

defmodule GRPC.Test.ServerAdapter do
  def start(s, h, p, opts) do
    {s, h, p, opts}
  end

  def stop(server) do
    {server}
  end

  def stream_send(stream, data) do
    {stream, data}
  end

  def flow_control(_, size) do
    {:ok, size}
  end
end
