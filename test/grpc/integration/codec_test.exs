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
      codecs: [GRPC.Codec.Proto, GRPC.Codec.Erlpack]

    def say_hello(req, _stream) do
      Helloworld.HelloReply.new(message: "Hello, #{req.name}")
    end
  end

  defmodule HelloErlpackStub do
    use GRPC.Stub, service: Helloworld.Greeter.Service
  end

  test "Says hello over erlpack" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      name = "Mairbek"
      req = Helloworld.HelloRequest.new(name: name)

      {:ok, reply} = channel |> HelloErlpackStub.say_hello(req, codec: GRPC.Codec.Erlpack)
      assert reply.message == "Hello, #{name}"

      # verify that proto still works
      {:ok, reply} = channel |> HelloErlpackStub.say_hello(req, codec: GRPC.Codec.Proto)
      assert reply.message == "Hello, #{name}"

      # codec not registered
      {:error, reply} = channel |> HelloErlpackStub.say_hello(req, codec: NotRegisteredCodec)

      assert %GRPC.RPCError{
               status: GRPC.Status.unimplemented(),
               message: "No codec registered for content-type application/grpc+not-registered"
             } == reply
    end)
  end
end
