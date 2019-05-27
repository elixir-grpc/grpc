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

  def stop(endpoint, server) do
    {endpoint, server}
  end

  def send_reply(stream, data, _opts) do
    {stream, data}
  end

  def send_headers(stream, _headers) do
    stream
  end

  def has_sent_headers?(_stream) do
    false
  end
end
