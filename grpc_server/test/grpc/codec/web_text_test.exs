defmodule GRPC.Codec.WebTextTest do
  use ExUnit.Case, async: true

  alias GRPC.Codec.WebText

  describe "name/0" do
    test "returns the codec name" do
      assert WebText.name() == "text"
    end
  end

  describe "encode/1" do
    test "encodes a protobuf struct to binary" do
      request = %Helloworld.HelloRequest{name: "Alice", duration: 42}
      encoded = WebText.encode(request)

      assert is_binary(encoded)
    end

    test "encodes empty struct" do
      request = %Helloworld.HelloRequest{name: "", duration: nil}
      encoded = WebText.encode(request)

      assert is_binary(encoded)
    end

    test "encodes struct with only some fields set" do
      request = %Helloworld.HelloRequest{name: "Bob"}
      encoded = WebText.encode(request)

      assert is_binary(encoded)
    end
  end

  describe "decode/2" do
    test "decodes binary to protobuf struct" do
      original = %Helloworld.HelloRequest{name: "Charlie", duration: 100}
      encoded = WebText.encode(original)
      decoded = WebText.decode(encoded, Helloworld.HelloRequest)

      assert decoded.name == "Charlie"
      assert decoded.duration == 100
    end

    test "decodes empty binary to empty struct" do
      decoded = WebText.decode(<<>>, Helloworld.HelloRequest)

      assert %Helloworld.HelloRequest{} = decoded
      assert decoded.name == ""
      assert decoded.duration == 0
    end

    test "decodes partial data correctly" do
      original = %Helloworld.HelloRequest{name: "Dave"}
      encoded = WebText.encode(original)
      decoded = WebText.decode(encoded, Helloworld.HelloRequest)

      assert decoded.name == "Dave"
      assert decoded.duration == 0
    end
  end

  describe "pack_for_channel/1" do
    test "encodes binary data to base64" do
      binary = <<1, 2, 3, 4, 5>>
      packed = WebText.pack_for_channel(binary)

      assert is_binary(packed)
      assert packed == Base.encode64(binary)
    end

    test "encodes iodata list to base64" do
      iodata = [<<1, 2>>, <<3, 4>>, <<5>>]
      packed = WebText.pack_for_channel(iodata)

      assert is_binary(packed)
      expected = iodata |> IO.iodata_to_binary() |> Base.encode64()
      assert packed == expected
    end

    test "handles empty binary" do
      packed = WebText.pack_for_channel(<<>>)

      assert packed == ""
    end

    test "produces valid base64 string" do
      binary = "Hello, World!"
      packed = WebText.pack_for_channel(binary)

      # Should be decodable
      assert {:ok, _} = Base.decode64(packed)
    end
  end

  describe "unpack_from_channel/1" do
    test "decodes base64 to binary data" do
      original = <<1, 2, 3, 4, 5>>
      base64 = Base.encode64(original)
      unpacked = WebText.unpack_from_channel(base64)

      assert unpacked == original
    end

    test "handles empty base64 string" do
      unpacked = WebText.unpack_from_channel("")

      assert unpacked == <<>>
    end

    test "handles base64 encoded text" do
      original = "Hello, World!"
      base64 = Base.encode64(original)
      unpacked = WebText.unpack_from_channel(base64)

      assert unpacked == original
    end

    test "raises on invalid base64" do
      assert_raise ArgumentError, fn ->
        WebText.unpack_from_channel("not valid base64!!!")
      end
    end
  end

  describe "round-trip encoding/decoding" do
    test "preserves all data through encode and decode cycle" do
      original = %Helloworld.HelloRequest{name: "Eve", duration: 250}
      encoded = WebText.encode(original)
      decoded = WebText.decode(encoded, Helloworld.HelloRequest)

      assert decoded.name == original.name
      assert decoded.duration == original.duration
    end

    test "handles empty values correctly" do
      original = %Helloworld.HelloRequest{name: "", duration: nil}
      encoded = WebText.encode(original)
      decoded = WebText.decode(encoded, Helloworld.HelloRequest)

      assert decoded.name == ""
      assert decoded.duration == 0
    end

    test "handles different message types" do
      original = %Helloworld.HelloReply{message: "Hello, World!"}
      encoded = WebText.encode(original)
      decoded = WebText.decode(encoded, Helloworld.HelloReply)

      assert decoded.message == "Hello, World!"
    end
  end

  describe "pack/unpack round-trip" do
    test "preserves binary data through pack and unpack cycle" do
      original = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>
      packed = WebText.pack_for_channel(original)
      unpacked = WebText.unpack_from_channel(packed)

      assert unpacked == original
    end

    test "preserves protobuf encoded data" do
      request = %Helloworld.HelloRequest{name: "Test", duration: 42}
      encoded = WebText.encode(request)
      packed = WebText.pack_for_channel(encoded)
      unpacked = WebText.unpack_from_channel(packed)
      decoded = WebText.decode(unpacked, Helloworld.HelloRequest)

      assert decoded.name == "Test"
      assert decoded.duration == 42
    end

    test "handles iodata correctly" do
      iodata = [<<1, 2>>, [<<3>>, <<4, 5>>]]
      packed = WebText.pack_for_channel(iodata)
      unpacked = WebText.unpack_from_channel(packed)

      assert unpacked == IO.iodata_to_binary(iodata)
    end
  end

  describe "encoding format" do
    test "produces consistent output for same input" do
      request = %Helloworld.HelloRequest{name: "Frank", duration: 500}
      encoded1 = WebText.encode(request)
      encoded2 = WebText.encode(request)

      assert encoded1 == encoded2
    end

    test "base64 encoding is URL-safe" do
      binary = :crypto.strong_rand_bytes(100)
      packed = WebText.pack_for_channel(binary)

      # Base64 should only contain valid characters
      assert Regex.match?(~r/^[A-Za-z0-9+\/=]*$/, packed)
    end
  end
end
