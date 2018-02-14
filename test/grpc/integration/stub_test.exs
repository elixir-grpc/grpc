defmodule GRPC.Integration.StubTest do
  use GRPC.Integration.TestCase, async: true

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream) do
      Helloworld.HelloReply.new(message: "Hello, #{req.name}")
    end
  end

  defmodule SlowServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(_req, _stream) do
      Process.sleep(1000)
    end
  end

  test "body larger than 2^14 works" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      name = String.duplicate("a", round(:math.pow(2, 15)))
      req = Helloworld.HelloRequest.new(name: name)
      {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
      assert reply.message == "Hello, #{name}"
    end)
  end

  test "returns error when timeout" do
    run_server(SlowServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      req = Helloworld.HelloRequest.new(name: "Elixir")
      assert {:error, %GRPC.RPCError{message: "deadline exceeded", status: "4"}} ==
        channel |> Helloworld.Greeter.Stub.say_hello(req, timeout: 500)
    end)
  end
end
