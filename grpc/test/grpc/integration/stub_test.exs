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

  test "you can disconnect stubs" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      Process.sleep(100)

      %{adapter_payload: %{conn_pid: connection_process_pid}} = channel
      %{gun_pid: gun_pid} = :sys.get_state(connection_process_pid)

      gun_port = port_for(gun_pid)
      # Using :erlang.monitor to be compatible with <= 1.5
      ref = :erlang.monitor(:port, gun_port)

      {:ok, channel} = GRPC.Stub.disconnect(channel)

      assert %{adapter_payload: %{conn_pid: nil}} = channel
      assert_receive {:DOWN, ^ref, :port, ^gun_port, _}
      assert port_for(gun_pid) == nil
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
      {:ok, _channel} =
        GRPC.Client.Connection.connect("localhost:#{port}",
          interceptors: [GRPC.Client.Interceptors.Logger],
          name: :my_channel
        )

      name = "GRPC user!"
      req = %Helloworld.HelloRequest{name: name}
      {:ok, reply} = %GRPC.Channel{ref: :my_channel} |> Helloworld.Greeter.Stub.say_hello(req)
      assert reply.message == "Hello, #{name}"
    end)
  end

  test "named Gun connections survive the original caller exiting" do
    run_server(HelloServer, fn port ->
      parent = self()
      channel_name = {:named_gun_connection, make_ref()}

      {caller_pid, caller_ref} =
        spawn_monitor(fn ->
          {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", name: channel_name)
          request = %Helloworld.HelloRequest{name: "first caller"}
          {:ok, reply} = Helloworld.Greeter.Stub.say_hello(channel, request)
          send(parent, {:initial_call_succeeded, channel, reply.message})
        end)

      assert_receive {:initial_call_succeeded, initial_channel, "Hello, first caller"}
      assert_receive {:DOWN, ^caller_ref, :process, ^caller_pid, :normal}

      named_channel = %GRPC.Channel{ref: channel_name}
      manager_pid = :global.whereis_name({GRPC.Client.Connection, channel_name})

      assert is_pid(manager_pid)
      assert Process.alive?(manager_pid)

      assert {:ok, picked_channel} = GRPC.Client.Connection.pick_channel(named_channel)
      connection_process_pid = picked_channel.adapter_payload.conn_pid

      assert is_pid(connection_process_pid)
      assert Process.alive?(connection_process_pid)

      %{gun_pid: initial_gun_pid} = :sys.get_state(connection_process_pid)
      assert is_pid(initial_gun_pid)
      assert Process.alive?(initial_gun_pid)

      assert {:ok, reused_channel} = GRPC.Stub.connect("localhost:#{port}", name: channel_name)
      assert reused_channel.adapter_payload.conn_pid == connection_process_pid

      %{gun_pid: reused_gun_pid} = :sys.get_state(connection_process_pid)
      assert reused_gun_pid == initial_gun_pid

      request = %Helloworld.HelloRequest{name: "second caller"}
      assert {:ok, reply} = Helloworld.Greeter.Stub.say_hello(named_channel, request)
      assert reply.message == "Hello, second caller"

      assert {:ok, disconnected_channel} = GRPC.Stub.disconnect(reused_channel)
      assert disconnected_channel.ref == initial_channel.ref
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
