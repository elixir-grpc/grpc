defmodule GRPC.Test.ClientAdapter do
  @behaviour GRPC.Client.Adapter

  def connect(channel, _opts), do: {:ok, channel}
  def disconnect(channel), do: {:ok, channel}
  def send_request(stream, _message, _opts), do: stream
  def receive_data(_stream, _opts), do: {:ok, nil}
  def send_data(stream, _message, _opts), do: stream
  def send_headers(stream, _opts), do: stream
  def end_stream(stream), do: stream
  def cancel(stream), do: stream
end

defmodule GRPC.Test.ServerAdapter do
  @behaviour GRPC.Server.Adapter

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
