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

  def set_headers(stream, headers) do
    send(self(), {:setting_headers, headers})
    stream
  end
end
