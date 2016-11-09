defmodule GRPC.ChannelTest do
  use ExUnit.Case, async: true
  alias GRPC.Test.ClientAdapter
  alias GRPC.Channel

  test "connect/2 works" do
    {:ok, channel} = GRPC.Stub.connect("10.1.0.0:50051", insecure: true, adapter: ClientAdapter)
    assert %Channel{host: "10.1.0.0", port: 50051, scheme: "http",
            payload: %{name: "Test.ClientAdapter"}} = channel
  end

  test "connect/3 works" do
    {:ok, channel} = GRPC.Stub.connect("10.1.0.0", 50051, insecure: true, adapter: ClientAdapter)
    assert %Channel{host: "10.1.0.0", port: 50051, scheme: "http",
            payload: %{name: "Test.ClientAdapter"}} = channel
  end
end
