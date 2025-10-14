defmodule GRPC.Client.Resolver.IPv6Test do
  use ExUnit.Case, async: true

  alias GRPC.Client.Resolver.IPv6

  test "resolves multiple IPv6 addresses with ports" do
    target = "ipv6:[2607:f8b0:400e:c00::ef]:443,[::1]:50051"

    assert {:ok, %{addresses: addresses, service_config: nil}} = IPv6.resolve(target)

    assert addresses == [
             %{address: "2607:f8b0:400e:c00::ef", port: 443},
             %{address: "::1", port: 50051}
           ]
  end

  test "resolves single IPv6 address with default port" do
    target = "ipv6:[::1]"

    assert {:ok, %{addresses: addresses, service_config: nil}} = IPv6.resolve(target)

    assert addresses == [
             %{address: "::1", port: 443}
           ]
  end
end
