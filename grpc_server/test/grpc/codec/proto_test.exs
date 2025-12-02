defmodule GRPC.Codec.ProtoTest do
  use ExUnit.Case, async: true

  alias GRPC.Codec.Proto

  describe "name/0" do
    test "returns the codec name" do
      assert Proto.name() == "proto"
    end
  end

  describe "encode/1" do
    test "encodes a protobuf struct to binary" do
      request = %Helloworld.HelloRequest{name: "Alice", duration: 42}
      encoded = Proto.encode(request)

      assert is_binary(encoded) or is_list(encoded)
    end

    test "encodes empty struct" do
      request = %Helloworld.HelloRequest{name: "", duration: nil}
      encoded = Proto.encode(request)

      assert is_binary(encoded) or is_list(encoded)
    end

    test "encodes struct with only some fields set" do
      request = %Helloworld.HelloRequest{name: "Bob"}
      encoded = Proto.encode(request)

      assert is_binary(encoded) or is_list(encoded)
    end
  end

  describe "decode/2" do
    test "decodes binary to protobuf struct" do
      original = %Helloworld.HelloRequest{name: "Charlie", duration: 100}
      encoded = Proto.encode(original) |> IO.iodata_to_binary()
      decoded = Proto.decode(encoded, Helloworld.HelloRequest)

      assert decoded.name == "Charlie"
      assert decoded.duration == 100
    end

    test "decodes empty binary to empty struct" do
      decoded = Proto.decode(<<>>, Helloworld.HelloRequest)

      assert %Helloworld.HelloRequest{} = decoded
      assert decoded.name == ""
      assert decoded.duration == 0
    end

    test "decodes partial data correctly" do
      original = %Helloworld.HelloRequest{name: "Dave"}
      encoded = Proto.encode(original) |> IO.iodata_to_binary()
      decoded = Proto.decode(encoded, Helloworld.HelloRequest)

      assert decoded.name == "Dave"
      assert decoded.duration == 0
    end
  end

  describe "round-trip encoding/decoding" do
    test "preserves all data through encode and decode cycle" do
      original = %Helloworld.HelloRequest{name: "Eve", duration: 250}
      encoded = Proto.encode(original) |> IO.iodata_to_binary()
      decoded = Proto.decode(encoded, Helloworld.HelloRequest)

      assert decoded.name == original.name
      assert decoded.duration == original.duration
    end

    test "handles empty values correctly" do
      original = %Helloworld.HelloRequest{name: "", duration: nil}
      encoded = Proto.encode(original) |> IO.iodata_to_binary()
      decoded = Proto.decode(encoded, Helloworld.HelloRequest)

      assert decoded.name == ""
      assert decoded.duration == 0
    end

    test "handles different message types" do
      original = %Helloworld.HelloReply{message: "Hello, World!"}
      encoded = Proto.encode(original) |> IO.iodata_to_binary()
      decoded = Proto.decode(encoded, Helloworld.HelloReply)

      assert decoded.message == "Hello, World!"
    end
  end

  describe "encoding format" do
    test "produces consistent output for same input" do
      request = %Helloworld.HelloRequest{name: "Frank", duration: 500}
      encoded1 = Proto.encode(request) |> IO.iodata_to_binary()
      encoded2 = Proto.encode(request) |> IO.iodata_to_binary()

      assert encoded1 == encoded2
    end
  end
end
