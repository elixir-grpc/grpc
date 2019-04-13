defmodule GRPC.ChannelTest do
  use ExUnit.Case
  alias GRPC.Test.ClientAdapter
  alias GRPC.Channel

  test "connect/2 works for insecure" do
    {:ok, channel} = GRPC.Stub.connect("10.1.0.0:50051", adapter: ClientAdapter)
    assert %Channel{host: "10.1.0.0", port: 50051, scheme: "http", cred: nil} = channel
  end

  test "connect/2 works for ssl" do
    cred = %{ssl: []}
    {:ok, channel} = GRPC.Stub.connect("10.1.0.0:50051", adapter: ClientAdapter, cred: cred)
    assert %Channel{host: "10.1.0.0", port: 50051, scheme: "https", cred: ^cred} = channel
  end
end
