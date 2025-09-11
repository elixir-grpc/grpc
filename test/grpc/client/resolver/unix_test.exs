defmodule GRPC.Client.Resolver.UnixTest do
  use ExUnit.Case, async: true

  alias GRPC.Client.Resolver.Unix

  test "resolves unix socket path" do
    target = "unix:///var/run/my.sock"

    assert {:ok, %{addresses: addresses, service_config: nil}} = Unix.resolve(target)

    assert addresses == [
             %{address: "/var/run/my.sock", port: nil, socket: :unix}
           ]
  end

  test "resolves unix socket with relative path" do
    target = "unix:/tmp/test.sock"

    assert {:ok, %{addresses: addresses, service_config: nil}} = Unix.resolve(target)

    assert addresses == [
             %{address: "/tmp/test.sock", port: nil, socket: :unix}
           ]
  end
end
