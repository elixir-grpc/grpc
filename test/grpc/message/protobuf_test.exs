defmodule GRPC.Message.ProtobufTest do
  use ExUnit.Case, async: true
  # use Protobuf.Case
  defmodule Helloworld do
    # @external_resource Path.expand("../../fixtures/helloworld.proto", __DIR__)
    # use Protobuf, from: Path.expand("../../fixtures/helloworld.proto", __DIR__)
    use Protobuf, """
  syntax = "proto3";
  package helloworld;

  service Greeter {
    rpc SayHello (HelloRequest) returns (HelloReply) {}
  }
  message HelloRequest {
    string name = 1;
  }
  message HelloReply {
    string message = 1;
  }
    """
  end

  test "encode/2 works for matched arguments" do
    request = Helloworld.HelloRequest.new(name: "elixir")
    assert <<10, 6, 101, 108, 105, 120, 105, 114>> ==
      GRPC.Message.Protobuf.encode(Helloworld.HelloRequest, request)
  end

  test "encode/2 returns empty for mismatched arguments" do
    request = Helloworld.HelloReply.new(name: "elixir")
    result = GRPC.Message.Protobuf.encode(Helloworld.HelloRequest, request)
    expected = result |> :unicode.characters_to_binary({:utf16, :little}) |> String.strip
    assert "" == expected
  end

  test "decode/2 works" do
    msg = <<10, 6, 101, 108, 105, 120, 105, 114>>
    request = Helloworld.HelloRequest.new(name: "elixir")
    assert request == GRPC.Message.Protobuf.decode(Helloworld.HelloRequest, msg)
  end

  test "decode/2 returns wrong result for mismatched arguments" do
    msg = <<10, 6, 101, 108, 105, 120, 105, 114>> # encoded HelloRequest
    request = Helloworld.HelloReply.new(message: "elixir")
    assert request == GRPC.Message.Protobuf.decode(Helloworld.HelloReply, msg)
  end
end
