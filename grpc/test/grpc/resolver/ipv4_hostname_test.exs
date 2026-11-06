defmodule GRPC.Client.Resolver.IPv4HostnameTest do
  # Changes the global Erlang resolver settings, so it can't run async.
  use ExUnit.Case, async: false

  alias GRPC.Client.Resolver.IPv4

  @moduletag :tmp_dir

  @ipv6_loopback "::1"

  setup %{tmp_dir: tmp_dir} do
    %{host: GRPC.Test.IPv6.aaaa_only_host(tmp_dir)}
  end

  test "a name with only AAAA records resolves to its IPv6 address", %{host: host} do
    assert {:ok, %{addresses: [address], service_config: nil}} =
             IPv4.resolve("ipv4:#{host}:50051")

    assert address == %{address: @ipv6_loopback, port: 50051, hostname: host}
  end

  test "a name with A and AAAA records is left for the adapter" do
    assert {:ok, %{addresses: [%{address: "dual-stack.test", port: 50051} = address]}} =
             IPv4.resolve("ipv4:dual-stack.test:50051")

    refute Map.has_key?(address, :hostname)
  end

  test "a name that does not resolve is left for the adapter" do
    assert {:ok, %{addresses: [%{address: "nowhere.test", port: 50051}]}} =
             IPv4.resolve("ipv4:nowhere.test:50051")
  end

  test "an IPv4 address is left as it is" do
    assert {:ok, %{addresses: [%{address: "10.0.0.1", port: 50051}]}} =
             IPv4.resolve("ipv4:10.0.0.1:50051")
  end
end
