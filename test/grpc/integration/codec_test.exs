defmodule GRPC.Integration.CodecTest do
  use GRPC.Integration.TestCase

  defmodule NotRegisteredCodec do
    @behaviour GRPC.Codec

    def name() do
      "not-registered"
    end

    def encode(struct) do
      :erlang.term_to_binary(struct)
    end

    def decode(_binary, _module) do
      :fail
    end
  end

  defmodule HelloServer do
    use GRPC.Server,
      service: Helloworld.Greeter.Service,
      codecs: [GRPC.Codec.Proto, GRPC.Codec.Erlpack, GRPC.Codec.WebText]

    def say_hello(req, _stream) do
      %Helloworld.HelloReply{message: "Hello, #{req.name}"}
    end
  end

  defmodule HelloStub do
    use GRPC.Stub, service: Helloworld.Greeter.Service
  end

  test "Says hello over erlpack, GRPC-web-text" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      name = "Mairbek"
      req = %Helloworld.HelloRequest{name: name}

      for codec <- [GRPC.Codec.Erlpack, GRPC.Codec.WebText, GRPC.Codec.Proto] do
        {:ok, reply} = HelloStub.say_hello(channel, req, codec: codec)
        assert reply.message == "Hello, #{name}"
      end

      # codec not registered
      {:error, reply} = HelloStub.say_hello(channel, req, codec: NotRegisteredCodec)

      assert %GRPC.RPCError{
               status: GRPC.Status.unimplemented(),
               message: "No codec registered for content-type application/grpc+not-registered"
             } == reply
    end)
  end

  test "sets the correct content-type based on codec name" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      name = "Mairbek"
      req = %Helloworld.HelloRequest{name: name}

      for {expected_content_type, codec} <- [
            {"grpc-web-text", GRPC.Codec.WebText},
            {"grpc+erlpack", GRPC.Codec.Erlpack},
            {"grpc+proto", GRPC.Codec.Proto}
          ] do
        {:ok, _reply, headers} =
          HelloStub.say_hello(channel, req, codec: codec, return_headers: true)

        assert headers[:headers]["content-type"] == "application/#{expected_content_type}"
      end
    end)
  end
end
