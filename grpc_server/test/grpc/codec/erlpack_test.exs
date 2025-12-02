defmodule GRPC.Codec.ErlpackTest do
  use ExUnit.Case, async: true

  alias GRPC.Codec.Erlpack

  describe "name/0" do
    test "returns the codec name" do
      assert Erlpack.name() == "erlpack"
    end
  end

  describe "encode/1" do
    test "encodes a protobuf struct to erlang binary term" do
      request = %Helloworld.HelloRequest{name: "Alice", duration: 42}
      encoded = Erlpack.encode(request)

      assert is_binary(encoded)
    end

    test "encodes empty struct" do
      request = %Helloworld.HelloRequest{name: "", duration: nil}
      encoded = Erlpack.encode(request)

      assert is_binary(encoded)
    end

    test "encodes struct with complex data" do
      request = %Helloworld.HelloRequest{name: "Bob with special chars: äöü", duration: 9999}
      encoded = Erlpack.encode(request)

      assert is_binary(encoded)
    end

    test "encodes different message types" do
      reply = %Helloworld.HelloReply{message: "Test message"}
      encoded = Erlpack.encode(reply)

      assert is_binary(encoded)
    end
  end

  describe "decode/2" do
    test "decodes binary term to original struct" do
      original = %Helloworld.HelloRequest{name: "Charlie", duration: 100}
      encoded = Erlpack.encode(original)
      decoded = Erlpack.decode(encoded, Helloworld.HelloRequest)

      assert decoded == original
    end

    test "decodes empty struct" do
      original = %Helloworld.HelloRequest{name: "", duration: nil}
      encoded = Erlpack.encode(original)
      decoded = Erlpack.decode(encoded, Helloworld.HelloRequest)

      assert decoded == original
    end

    test "module parameter is not used" do
      original = %Helloworld.HelloRequest{name: "Dave"}
      encoded = Erlpack.encode(original)
      decoded = Erlpack.decode(encoded, SomeOtherModule)

      assert decoded == original
    end
  end

  describe "round-trip encoding/decoding" do
    test "preserves all data perfectly" do
      original = %Helloworld.HelloRequest{name: "Eve", duration: 250}
      encoded = Erlpack.encode(original)
      decoded = Erlpack.decode(encoded, Helloworld.HelloRequest)

      assert decoded == original
    end

    test "handles unicode and special characters" do
      original = %Helloworld.HelloRequest{name: "Unicode: 你好 🚀", duration: 123}
      encoded = Erlpack.encode(original)
      decoded = Erlpack.decode(encoded, Helloworld.HelloRequest)

      assert decoded == original
    end

    test "handles nil and empty values" do
      original = %Helloworld.HelloRequest{name: "", duration: nil}
      encoded = Erlpack.encode(original)
      decoded = Erlpack.decode(encoded, Helloworld.HelloRequest)

      assert decoded.name == ""
      assert is_nil(decoded.duration)
    end

    test "handles different message types" do
      original = %Helloworld.HelloReply{message: "Complex message with\nnewlines\tand\ttabs"}
      encoded = Erlpack.encode(original)
      decoded = Erlpack.decode(encoded, Helloworld.HelloReply)

      assert decoded == original
    end
  end

  describe "erlang term format" do
    test "produces binary in erlang external term format" do
      request = %Helloworld.HelloRequest{name: "Test"}
      encoded = Erlpack.encode(request)

      <<version, _rest::binary>> = encoded
      assert version == 131
    end

    test "can decode any erlang term" do
      term = {:hello, "world", 123}
      binary = :erlang.term_to_binary(term)
      decoded = Erlpack.decode(binary, AnyModule)

      assert decoded == term
    end
  end
end
