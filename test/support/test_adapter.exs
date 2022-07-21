defmodule GRPC.Test.ClientAdapter do
  @behaviour GRPC.Client.Adapter

  def connect(channel, _opts), do: {:ok, channel}
  def disconnect(channel), do: {:ok, channel}
  def send_request(stream, _message, _opts), do: stream
  def recv_headers(_stream, _, _opts), do: {:ok, %{}, :fin}
  def recv_data_or_trailers(_stream, _, _opts), do: {:data, ""}
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
