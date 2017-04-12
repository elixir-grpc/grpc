defmodule GRPC.Test.ClientAdapter do
  def connect(_, %{cred: nil}) do
    {:ok, %{pname: :grpc_test_client_dapter, cred: nil}}
  end
  def connect(_, %{cred: cred}) do
    {:ok, %{pname: :grpc_test_client_ssl_dapter, cred: cred}}
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
