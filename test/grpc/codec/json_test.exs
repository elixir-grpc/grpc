defmodule GRPC.Codec.JSONTest do
  use ExUnit.Case, async: false

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
      decoded_map = JSON.decode(encoded, Helloworld.HelloRequest)
      assert decoded_map["name"] == "Alice"
    end

    test "includes unpopulated fields with default values" do
      request = %Helloworld.HelloRequest{name: "Bob", duration: nil}
      encoded = JSON.encode(request)

      decoded_map = JSON.decode(encoded, Helloworld.HelloRequest)
      assert Map.has_key?(decoded_map, "duration")
    end

    test "encodes empty struct with all default fields" do
      request = %Helloworld.HelloRequest{name: "", duration: nil}
      encoded = JSON.encode(request)

      decoded_map = JSON.decode(encoded, Helloworld.HelloRequest)
      assert Map.has_key?(decoded_map, "name")
      assert Map.has_key?(decoded_map, "duration")
      assert decoded_map["name"] == ""
    end

    test "encodes nested messages with defaults" do
      reply = %Helloworld.HelloReply{message: ""}
      encoded = JSON.encode(reply)

      decoded_map = JSON.decode(encoded, Helloworld.HelloReply)
      assert Map.has_key?(decoded_map, "message")
      assert decoded_map["message"] == ""
    end
  end

  describe "encode/2 with custom options" do
    test "can disable emit_unpopulated via options" do
      request = %Helloworld.HelloRequest{name: "Alice", duration: nil}
      encoded = JSON.encode(request, emit_unpopulated: false)

      decoded_map = JSON.decode(encoded, Helloworld.HelloRequest)
      assert decoded_map["name"] == "Alice"
      refute Map.has_key?(decoded_map, "duration")
    end

    test "respects emit_unpopulated: true when passed explicitly" do
      request = %Helloworld.HelloRequest{name: "Bob", duration: nil}
      encoded = JSON.encode(request, emit_unpopulated: true)

      decoded_map = JSON.decode(encoded, Helloworld.HelloRequest)
      assert decoded_map["name"] == "Bob"
      assert Map.has_key?(decoded_map, "duration")
    end

    test "only includes populated fields when emit_unpopulated is false" do
      request = %Helloworld.HelloRequest{name: "", duration: nil}
      encoded = JSON.encode(request, emit_unpopulated: false)

      decoded_map = JSON.decode(encoded, Helloworld.HelloRequest)
      refute Map.has_key?(decoded_map, "duration")
    end

    test "can pass other Protobuf.JSON options" do
      request = %Helloworld.HelloRequest{name: "Charlie", duration: 42}
      encoded = JSON.encode(request, emit_unpopulated: true, use_proto_names: true)

      assert is_binary(encoded)
      decoded_map = JSON.decode(encoded, Helloworld.HelloRequest)
      assert decoded_map["name"] == "Charlie"
    end

    test "merges passed options with default config" do
      request = %Helloworld.HelloRequest{name: "Merge Test", duration: nil}

      encoded_default = JSON.encode(request)
      decoded_default = JSON.decode(encoded_default, Helloworld.HelloRequest)
      assert Map.has_key?(decoded_default, "duration")

      encoded_override = JSON.encode(request, emit_unpopulated: false)
      decoded_override = JSON.decode(encoded_override, Helloworld.HelloRequest)
      refute Map.has_key?(decoded_override, "duration")

      encoded_merged = JSON.encode(request, use_proto_names: true)
      decoded_merged = JSON.decode(encoded_merged, Helloworld.HelloRequest)
      assert Map.has_key?(decoded_merged, "duration")
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

  describe "configuration via Application env" do
    setup do
      original_config = Application.get_env(:grpc, GRPC.Codec.JSON)

      on_exit(fn ->
        if original_config do
          Application.put_env(:grpc, GRPC.Codec.JSON, original_config)
        else
          Application.delete_env(:grpc, GRPC.Codec.JSON)
        end
      end)

      :ok
    end

    test "respects application configuration for emit_unpopulated: false" do
      Application.put_env(:grpc, GRPC.Codec.JSON, emit_unpopulated: false)

      request = %Helloworld.HelloRequest{name: "Config Test", duration: nil}
      encoded = JSON.encode(request)
      decoded_map = JSON.decode(encoded, Helloworld.HelloRequest)

      assert decoded_map["name"] == "Config Test"
      refute Map.has_key?(decoded_map, "duration")
    end

    test "respects application configuration for emit_unpopulated: true" do
      Application.put_env(:grpc, GRPC.Codec.JSON, emit_unpopulated: true)

      request = %Helloworld.HelloRequest{name: "Config Test 2", duration: nil}
      encoded = JSON.encode(request)
      decoded_map = JSON.decode(encoded, Helloworld.HelloRequest)

      assert decoded_map["name"] == "Config Test 2"
      assert Map.has_key?(decoded_map, "duration")
    end

    test "encode/2 options override application configuration" do
      Application.put_env(:grpc, GRPC.Codec.JSON, emit_unpopulated: false)

      request = %Helloworld.HelloRequest{name: "Override Test", duration: nil}
      encoded = JSON.encode(request, emit_unpopulated: true)
      decoded_map = JSON.decode(encoded, Helloworld.HelloRequest)

      assert decoded_map["name"] == "Override Test"
      assert Map.has_key?(decoded_map, "duration")
    end
  end
end
