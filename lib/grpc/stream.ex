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

  def get_cert(%GRPC.Server.Stream{adapter: adapter} = stream) do
    adapter.get_cert(stream.payload)
  end
end
