defmodule GRPC.Stream do
  @moduledoc """
  Some useful operations for streams.
  """

  @doc """
  Get headers from server stream.

  For the client side, you should use `:return_headers` option to get headers,
  see `GRPC.Stub` for details.
  """
  @spec get_headers(GRPC.Server.Stream.t()) :: map
  def get_headers(%GRPC.Server.Stream{adapter: adapter} = stream) do
    headers = adapter.get_headers(stream.payload)
    GRPC.Transport.HTTP2.decode_headers(headers)
  end

  @doc """
  Get server address (hostname and port)
  """
  @spec get_server_address(GRPC.Server.Stream.t() | GRPC.Client.Stream.t()) :: tuple()
  def get_server_address(%GRPC.Server.Stream{adapter: adapter} = stream) do
    {host, port} = adapter.get_sock(stream.payload)
    {to_string(:inet_parse.ntoa(host)), port}
  end

  def get_server_address(%GRPC.Client.Stream{channel: channel}) do
    {channel.host, channel.port}
  end
end
