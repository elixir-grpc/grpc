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
    stream
    |> adapter.get_headers()
    |> GRPC.Transport.HTTP2.decode_headers()
  end
end
