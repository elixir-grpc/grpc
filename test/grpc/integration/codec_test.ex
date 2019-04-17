defmodule GRPC.Integration.CoderTest do
  use GRPC.Integration.TestCase, async: true

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service, codec: GRPC.Codec.Erlpack

    def say_hello(req, _stream) do
      Helloworld.HelloReply.new(message: "Hello, #{req.name}")
    end
  end

  defmodule HelloErlpackStub do
    use GRPC.Stub, service: Helloworld.Greeter.Service, codec: GRPC.Codec.Erlpack
  end

  test "Says hello over erlpack" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", interceptors: [GRPC.Logger.Client])
      name = "Mairbek"
      req = Helloworld.HelloRequest.new(name: name)
      {:ok, reply} = channel |> HelloErlpackStub.say_hello(req)
      assert reply.message == "Hello, #{name}"
    end)
  end
end
