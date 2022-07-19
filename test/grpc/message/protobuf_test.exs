defmodule GRPC.Message.ProtobufTest do
  use ExUnit.Case, async: true

  defmodule Helloworld.HelloRequest do
    use Protobuf, syntax: :proto3

    field(:name, 1, optional: true, type: :string)
  end

  defmodule Helloworld.HelloReply do
    use Protobuf, syntax: :proto3

    field(:message, 1, optional: true, type: :string)
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
    # encoded HelloRequest
    msg = <<10, 6, 101, 108, 105, 120, 105, 114>>
    request = Helloworld.HelloReply.new(message: "elixir")
    assert ^request = GRPC.Message.Protobuf.decode(Helloworld.HelloReply, msg)
  end
end
