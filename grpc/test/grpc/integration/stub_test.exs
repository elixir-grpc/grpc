defmodule GRPC.Integration.StubTest do
  use GRPC.Integration.TestCase

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream) do
      %Helloworld.HelloReply{message: "Hello, #{req.name}"}
    end
  end

  defmodule SlowServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(_req, _stream) do
      Process.sleep(1000)
    end
  end

  test "you can disconnect stubs" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      assert is_reference(channel.pool)

      {:ok, disconnected_channel} = GRPC.Stub.disconnect(channel)
      assert %{pool: nil} = disconnected_channel
    end)
  end

  test "disconnecting a disconnected channel is a no-op" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      {:ok, _channel} = GRPC.Stub.disconnect(channel)
    end)
  end

  test "body larger than 2^14 works" do
    run_server(HelloServer, fn port ->
      {:ok, channel} =
        GRPC.Stub.connect("localhost:#{port}", interceptors: [GRPC.Client.Interceptors.Logger])

      name = String.duplicate("a", round(:math.pow(2, 15)))
      req = %Helloworld.HelloRequest{name: name}
      {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
      assert reply.message == "Hello, #{name}"
    end)
  end

  test "use a channel name to send a message" do
    run_server(HelloServer, fn port ->
      assert {:ok, %GRPC.Channel{ref: :my_channel} = channel} =
               GRPC.Client.Connection.connect("localhost:#{port}",
                 interceptors: [GRPC.Client.Interceptors.Logger],
                 name: :my_channel
               )

      name = "GRPC user!"
      req = %Helloworld.HelloRequest{name: name}
      {:ok, reply} = Helloworld.Greeter.Stub.say_hello(channel, req)
      assert reply.message == "Hello, #{name}"
    end)
  end

  test "invalid channel function clause error" do
    req = %Helloworld.HelloRequest{name: "GRPC"}

    assert_raise FunctionClauseError, ~r/Helloworld.Greeter.Stub.say_hello/, fn ->
      Helloworld.Greeter.Stub.say_hello(nil, req)
    end
  end

  test "returns error when timeout" do
    run_server(SlowServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      req = %Helloworld.HelloRequest{name: "Elixir"}

      assert {:error,
              %GRPC.RPCError{
                message: "Deadline expired",
                status: GRPC.Status.deadline_exceeded()
              }} == channel |> Helloworld.Greeter.Stub.say_hello(req, timeout: 500)
    end)
  end
end
