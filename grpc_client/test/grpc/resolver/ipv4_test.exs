defmodule GRPC.Client.Resolver.IPv4Test do
  use ExUnit.Case, async: true

  alias GRPC.Client.Resolver.IPv4

  test "resolves multiple IPv4 addresses with ports" do
    target = "ipv4:10.0.0.1:50051,10.0.0.2:50052"

    assert {:ok, %{addresses: addresses, service_config: nil}} = IPv4.resolve(target)

    assert addresses == [
             %{address: "10.0.0.1", port: 50051},
             %{address: "10.0.0.2", port: 50052}
           ]
  end

  test "resolves single IPv4 address" do
    target = "ipv4:192.168.1.10:12345"

    assert {:ok, %{addresses: addresses, service_config: nil}} = IPv4.resolve(target)

    assert addresses == [
             %{address: "192.168.1.10", port: 12345}
           ]
  end
end
