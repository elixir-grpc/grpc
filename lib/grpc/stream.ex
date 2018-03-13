defmodule GRPC.Stream do
  def get_headers(%GRPC.Server.Stream{adapter: adapter} = stream) do
    stream
    |> adapter.get_headers()
    |> GRPC.Transport.HTTP2.decode_headers()
  end
end
