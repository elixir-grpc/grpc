defmodule GRPC.Test.ClientAdapter do
  def connect_insecurely(_) do
    {:ok, %{name: "Test.ClientAdapter"}}
  end
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
end
