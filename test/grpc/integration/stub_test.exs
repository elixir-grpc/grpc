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

  error_table = [
    {401, GRPC.Status.unauthenticated()},
    {403, GRPC.Status.permission_denied()},
    {404, GRPC.Status.unimplemented()},
    {429, GRPC.Status.unavailable()},
    {502, GRPC.Status.unavailable()},
    {503, GRPC.Status.unavailable()},
    {504, GRPC.Status.unavailable()},
    # General case
    {518, GRPC.Status.internal()}
  ]

  client_adapters = [GRPC.Client.Adapters.Gun, GRPC.Client.Adapters.Mint]

  for {http_code, expected_error_code} <- error_table, client_adapter <- client_adapters do
    test "#{client_adapter} returns RPC Error when getting HTTP #{http_code}" do
      run_error_server(unquote(http_code), fn port ->
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter: unquote(client_adapter))
        req = %Helloworld.HelloRequest{name: "GRPC"}

        {:error, %GRPC.RPCError{status: unquote(expected_error_code)}} =
          Helloworld.Greeter.Stub.say_hello(channel, req)
      end)
    end
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
      {:ok, channel} =
        GRPC.Stub.connect("localhost:#{port}", interceptors: [GRPC.Client.Interceptors.Logger])

      name = String.duplicate("a", round(:math.pow(2, 15)))
      req = %Helloworld.HelloRequest{name: name}
      {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
      assert reply.message == "Hello, #{name}"
    end)
  end

  test "invalid channel function clause error" do
    req = Helloworld.HelloRequest.new(name: "GRPC")

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
