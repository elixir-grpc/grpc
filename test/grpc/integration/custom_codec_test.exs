defmodule GRPC.Integration.CustomCodecTest do
  use GRPC.Integration.TestCase

  defmodule CustomJSONCodec do
    @behaviour GRPC.Codec

    @impl true
    def name(), do: "custom-json"

    @impl true
    def encode(struct, _opts \\ []) do
      json = Protobuf.JSON.encode!(struct)
      "CUSTOM:" <> json
    end

    @impl true
    def decode(binary, module) do
      "CUSTOM:" <> json = binary
      Protobuf.JSON.decode!(json, module)
    end
  end

  defmodule CustomMsgPackCodec do
    @behaviour GRPC.Codec

    @impl true
    def name(), do: "msgpack"

    @impl true
    def encode(struct, _opts \\ []) do
      :erlang.term_to_binary(struct)
    end

    @impl true
    def decode(binary, _module) do
      :erlang.binary_to_term(binary)
    end
  end

  defmodule HelloRequest do
    use Protobuf, syntax: :proto3

    field(:name, 1, type: :string)
  end

  defmodule HelloReply do
    use Protobuf, syntax: :proto3

    field(:message, 1, type: :string)
  end

  defmodule Greeter.Service do
    use GRPC.Service, name: "custom_codec.Greeter"

    rpc(:SayHello, HelloRequest, HelloReply)
  end

  defmodule Greeter.Server do
    use GRPC.Server,
      service: Greeter.Service,
      codecs: [CustomJSONCodec, CustomMsgPackCodec, GRPC.Codec.Proto]

    def say_hello(request, _stream) do
      %HelloReply{message: "Hello, #{request.name}!"}
    end
  end

  defmodule Greeter.Stub do
    use GRPC.Stub, service: Greeter.Service
  end

  defmodule GreeterEndpoint do
    use GRPC.Endpoint

    run(Greeter.Server)
  end

  describe "Custom Codec Integration" do
    test "client and server use CustomJSONCodec successfully" do
      run_endpoint(GreeterEndpoint, fn port ->
        {:ok, channel} =
          GRPC.Stub.connect("localhost:#{port}", codec: CustomJSONCodec)

        request = %HelloRequest{name: "CustomJSON"}
        {:ok, reply} = Greeter.Stub.say_hello(channel, request)

        assert reply.message == "Hello, CustomJSON!"

        GRPC.Stub.disconnect(channel)
      end)
    end

    test "client overrides codec via call option" do
      run_endpoint(GreeterEndpoint, fn port ->
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

        request = %HelloRequest{name: "OverrideCodec"}
        {:ok, reply} = Greeter.Stub.say_hello(channel, request, codec: CustomJSONCodec)

        assert reply.message == "Hello, OverrideCodec!"

        GRPC.Stub.disconnect(channel)
      end)
    end

    test "client and server use CustomMsgPackCodec" do
      run_endpoint(GreeterEndpoint, fn port ->
        {:ok, channel} =
          GRPC.Stub.connect("localhost:#{port}", codec: CustomMsgPackCodec)

        request = %HelloRequest{name: "MsgPack"}
        {:ok, reply} = Greeter.Stub.say_hello(channel, request)

        assert reply.message == "Hello, MsgPack!"

        GRPC.Stub.disconnect(channel)
      end)
    end

    test "server supports multiple codecs simultaneously" do
      run_endpoint(GreeterEndpoint, fn port ->
        {:ok, channel1} =
          GRPC.Stub.connect("localhost:#{port}", codec: CustomJSONCodec)

        {:ok, channel2} =
          GRPC.Stub.connect("localhost:#{port}", codec: CustomMsgPackCodec)

        {:ok, channel3} = GRPC.Stub.connect("localhost:#{port}")

        request1 = %HelloRequest{name: "JSON"}
        request2 = %HelloRequest{name: "MsgPack"}
        request3 = %HelloRequest{name: "Proto"}

        {:ok, reply1} = Greeter.Stub.say_hello(channel1, request1)
        {:ok, reply2} = Greeter.Stub.say_hello(channel2, request2)
        {:ok, reply3} = Greeter.Stub.say_hello(channel3, request3)

        assert reply1.message == "Hello, JSON!"
        assert reply2.message == "Hello, MsgPack!"
        assert reply3.message == "Hello, Proto!"

        GRPC.Stub.disconnect(channel1)
        GRPC.Stub.disconnect(channel2)
        GRPC.Stub.disconnect(channel3)
      end)
    end

    test "custom codec with encode/2 signature" do
      run_endpoint(GreeterEndpoint, fn port ->
        {:ok, channel} =
          GRPC.Stub.connect("localhost:#{port}", codec: CustomJSONCodec)

        request = %HelloRequest{name: "Options"}
        {:ok, reply} = Greeter.Stub.say_hello(channel, request)

        assert reply.message == "Hello, Options!"

        GRPC.Stub.disconnect(channel)
      end)
    end

    test "unregistered codec returns error" do
      run_endpoint(GreeterEndpoint, fn port ->
        defmodule UnregisteredCodec do
          @behaviour GRPC.Codec
          def name(), do: "unregistered"
          def encode(struct, _opts \\ []), do: inspect(struct)
          def decode(binary, _module), do: binary
        end

        {:ok, channel} =
          GRPC.Stub.connect("localhost:#{port}", codec: UnregisteredCodec)

        request = %HelloRequest{name: "Fail"}

        assert {:error, %GRPC.RPCError{status: 12}} =
                 Greeter.Stub.say_hello(channel, request)

        GRPC.Stub.disconnect(channel)
      end)
    end
  end

  describe "Codec name() and content-type" do
    test "codec is selected by content-type header" do
      run_endpoint(GreeterEndpoint, fn port ->
        {:ok, channel} =
          GRPC.Stub.connect("localhost:#{port}", codec: CustomJSONCodec)

        request = %HelloRequest{name: "ContentType"}
        {:ok, reply} = Greeter.Stub.say_hello(channel, request)

        assert reply.message == "Hello, ContentType!"

        GRPC.Stub.disconnect(channel)
      end)
    end
  end
end
