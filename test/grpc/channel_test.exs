defmodule GRPC.ChannelTest do
  use ExUnit.Case, async: true
  alias GRPC.Test.ClientAdapter
  alias GRPC.Channel

  test "connect/2 works for insecure" do
    {:ok, channel} = GRPC.Stub.connect("10.1.0.0:50051", adapter: ClientAdapter)
    assert %Channel{host: "10.1.0.0", port: 50051, scheme: "http",
            payload: %{pname: :grpc_test_client_dapter, cred: nil}} = channel
  end

  test "connect/2 works for ssl" do
    cred = %{tls: %{}}
    {:ok, channel} = GRPC.Stub.connect("10.1.0.0:50051", adapter: ClientAdapter, cred: cred)
    assert %Channel{host: "10.1.0.0", port: 50051, scheme: "https",
            payload: %{pname: :grpc_test_client_ssl_dapter, cred: ^cred}} = channel
  end
end
