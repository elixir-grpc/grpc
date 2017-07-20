defmodule GRPC.Message.ProtobufTest do
  use ExUnit.Case, async: true

  defmodule Helloworld do
    use Protobuf, """
  syntax = "proto3";
  package helloworld;

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
    assert <<10, 6, 101, 108, 105, 120, 105, 114>> =
            GRPC.Message.Protobuf.encode(Helloworld.HelloRequest, request)
  end

  test "decode/2 works" do
    msg = <<10, 6, 101, 108, 105, 120, 105, 114>>
    request = Helloworld.HelloRequest.new(name: "elixir")
    assert ^request = GRPC.Message.Protobuf.decode(Helloworld.HelloRequest, msg)
  end

  test "decode/2 returns wrong result for mismatched arguments" do
    msg = <<10, 6, 101, 108, 105, 120, 105, 114>> # encoded HelloRequest
    request = Helloworld.HelloReply.new(message: "elixir")
    assert ^request = GRPC.Message.Protobuf.decode(Helloworld.HelloReply, msg)
  end
end
