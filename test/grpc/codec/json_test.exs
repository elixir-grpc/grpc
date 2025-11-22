defmodule GRPC.Codec.JSONTest do
  use ExUnit.Case, async: true

  alias GRPC.Codec.JSON

  describe "name/0" do
    test "returns the codec name" do
      assert JSON.name() == "json"
    end
  end

  describe "encode/1" do
    test "encodes a protobuf struct to JSON" do
      request = %Helloworld.HelloRequest{name: "Alice"}
      encoded = JSON.encode(request)

      assert is_binary(encoded)
      decoded_map = Jason.decode!(encoded)
      assert decoded_map["name"] == "Alice"
    end

    test "includes unpopulated fields with default values" do
      request = %Helloworld.HelloRequest{name: "Bob", duration: nil}
      encoded = JSON.encode(request)

      decoded_map = Jason.decode!(encoded)
      assert Map.has_key?(decoded_map, "duration")
    end

    test "encodes empty struct with all default fields" do
      request = %Helloworld.HelloRequest{name: "", duration: nil}
      encoded = JSON.encode(request)

      decoded_map = Jason.decode!(encoded)
      assert Map.has_key?(decoded_map, "name")
      assert Map.has_key?(decoded_map, "duration")
      assert decoded_map["name"] == ""
    end

    test "encodes nested messages with defaults" do
      reply = %Helloworld.HelloReply{message: ""}
      encoded = JSON.encode(reply)

      decoded_map = Jason.decode!(encoded)
      assert Map.has_key?(decoded_map, "message")
      assert decoded_map["message"] == ""
    end
  end

  describe "decode/2" do
    test "decodes empty binary to empty map" do
      assert JSON.decode(<<>>, Helloworld.HelloRequest) == %{}
    end

    test "decodes JSON binary to map" do
      json = ~s({"name": "Charlie", "duration": 42})
      result = JSON.decode(json, Helloworld.HelloRequest)

      assert is_map(result)
      assert result["name"] == "Charlie"
      assert result["duration"] == 42
    end

    test "decodes JSON with missing fields" do
      json = ~s({"name": "Dave"})
      result = JSON.decode(json, Helloworld.HelloRequest)

      assert result["name"] == "Dave"
      refute Map.has_key?(result, "duration")
    end

    test "decodes complex JSON structures" do
      json = ~s({"message": "Hello, World!"})
      result = JSON.decode(json, Helloworld.HelloReply)

      assert result["message"] == "Hello, World!"
    end
  end

  describe "round-trip encoding/decoding" do
    test "preserves data through encode and decode cycle" do
      original = %Helloworld.HelloRequest{name: "Eve", duration: 100}
      encoded = JSON.encode(original)
      decoded = JSON.decode(encoded, Helloworld.HelloRequest)

      assert decoded["name"] == "Eve"
      assert decoded["duration"] == 100
    end

    test "handles empty values correctly" do
      original = %Helloworld.HelloRequest{name: "", duration: nil}
      encoded = JSON.encode(original)
      decoded = JSON.decode(encoded, Helloworld.HelloRequest)

      assert decoded["name"] == ""
      assert is_nil(decoded["duration"]) or not Map.has_key?(decoded, "duration")
    end
  end
end
