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
    alias GRPC.Server

    def say_hello(_req, _stream) do
      Process.sleep(1000)
    end

    def say_hello_stream(_req, stream) do
      Enum.each(1..3, fn(x)->
        send_message(x, stream)
      end)
    end

    def send_message(message, stream) do
      response = Helloworld.HelloReply.new(message: "Stream, #{inspect message}")
      Process.sleep(200)
      Server.send_reply(stream, response)
    end
  end

  def port_for(pid) do
    Port.list()
    |> Enum.find(fn port ->
      case Port.info(port, :links) do
        {:links, links} ->
          pid in links

        _ ->
          false
      end
    end)
  end

  test "you can disconnect stubs" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

      %{adapter_payload: %{conn_pid: gun_conn_pid}} = channel

      gun_port = port_for(gun_conn_pid)
      # Using :erlang.monitor to be compatible with <= 1.5
      ref = :erlang.monitor(:port, gun_port)

      {:ok, channel} = GRPC.Stub.disconnect(channel)

      assert %{adapter_payload: %{conn_pid: nil}} = channel
      assert_receive {:DOWN, ^ref, :port, ^gun_port, _}
      assert port_for(gun_conn_pid) == nil
    end)
  end

  test "disconnecting a disconnected channel is a no-op" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      {:ok, channel} = GRPC.Stub.disconnect(channel)
      {:ok, _channel} = GRPC.Stub.disconnect(channel)
    end)
  end

  test "body larger than 2^14 works" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", interceptors: [GRPC.Logger.Client])
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

      assert {:error,
              %GRPC.RPCError{
                message: "Deadline expired",
                status: GRPC.Status.deadline_exceeded()
              }} == channel |> Helloworld.Greeter.Stub.say_hello(req, timeout: 500)
    end)
  end

  @tag :stream_deadline
  test "doesn't timeout on streaming reply" do
    run_server(SlowServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      req = Helloworld.HelloRequest.new(name: "Elixir")

      {:ok, stream} = channel |> Helloworld.Greeter.Stub.say_hello_stream(req, timeout: 500)

      response =
        Enum.map stream, &(&1)

      assert [{:ok, %Helloworld.HelloRequest{name: "Stream, 3"}}] == Enum.take(response, -1)
    end)
  end
end
