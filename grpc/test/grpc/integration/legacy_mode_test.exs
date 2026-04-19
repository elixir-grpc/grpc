defmodule GRPC.Integration.LegacyModeTest do
  # async: false — toggles global Application env and uses global telemetry handlers
  use ExUnit.Case, async: false

  import GRPC.Integration.TestCase

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(%{name: "raise", duration: duration}, _stream) do
      Process.sleep(duration)
      raise ArgumentError, "exception raised"
    end

    def say_hello(%{name: "delay", duration: duration}, _stream) do
      Process.sleep(duration)
      %Helloworld.HelloReply{message: "Hello"}
    end

    def say_hello(req, _stream) do
      %Helloworld.HelloReply{message: "Hello, #{req.name}"}
    end
  end

  setup do
    Application.put_env(:grpc, :pool_enabled, false)
    on_exit(fn -> Application.delete_env(:grpc, :pool_enabled) end)
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

      %{adapter_payload: %{conn_pid: gun_conn_pid}} = channel

      gun_port = port_for(gun_conn_pid)
      ref = :erlang.monitor(:port, gun_port)

      {:ok, channel} = GRPC.Stub.disconnect(channel)

      assert %{adapter_payload: %{conn_pid: nil}} = channel
      assert_receive {:DOWN, ^ref, :port, ^gun_port, _}
      assert port_for(gun_conn_pid) == nil
    end)
  end

  describe "telemetry" do
    test "sends server start+stop events on success" do
      server_rpc_prefix = GRPC.Telemetry.server_rpc_prefix()
      start_server_name = server_rpc_prefix ++ [:start]
      stop_server_name = server_rpc_prefix ++ [:stop]
      exception_server_name = server_rpc_prefix ++ [:exception]

      client_rpc_prefix = GRPC.Telemetry.client_rpc_prefix()
      start_client_name = client_rpc_prefix ++ [:start]
      stop_client_name = client_rpc_prefix ++ [:stop]
      exception_client_name = client_rpc_prefix ++ [:exception]

      attach_events([
        start_server_name,
        stop_server_name,
        exception_server_name,
        start_client_name,
        stop_client_name,
        exception_client_name
      ])

      run_server([HelloServer], fn port ->
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

        req = %Helloworld.HelloRequest{name: "delay", duration: 1000}

        assert {:ok, _} = Helloworld.Greeter.Stub.say_hello(channel, req)
      end)

      assert_received {^start_server_name, measurements, metadata}
      assert %{monotonic_time: _, system_time: _} = measurements

      assert %{
               server: HelloServer,
               endpoint: nil,
               function_name: :say_hello,
               stream: %GRPC.Server.Stream{}
             } = metadata

      assert_received {^stop_server_name, measurements, metadata}
      assert %{duration: duration} = measurements
      assert duration > 1000

      assert %{
               server: HelloServer,
               endpoint: nil,
               function_name: :say_hello,
               stream: %GRPC.Server.Stream{}
             } = metadata

      assert_received {:gun_down, _, _, _, _}

      assert_received {^start_client_name, measurements, metadata}
      assert %{monotonic_time: _, system_time: _} = measurements

      assert %{
               stream: %GRPC.Client.Stream{
                 rpc:
                   {:SayHello, {Helloworld.HelloRequest, false}, {Helloworld.HelloReply, false},
                    %{}}
               }
             } = metadata

      assert_received {^stop_client_name, measurements, metadata}
      assert %{duration: duration} = measurements
      assert duration > 1100

      assert %{
               stream: %GRPC.Client.Stream{
                 rpc:
                   {:SayHello, {Helloworld.HelloRequest, false}, {Helloworld.HelloReply, false},
                    %{}}
               }
             } = metadata

      refute_receive _
    end

    test "sends server start+exception events on success" do
      server_rpc_prefix = GRPC.Telemetry.server_rpc_prefix()
      start_server_name = server_rpc_prefix ++ [:start]
      stop_server_name = server_rpc_prefix ++ [:stop]
      exception_server_name = server_rpc_prefix ++ [:exception]

      client_rpc_prefix = GRPC.Telemetry.client_rpc_prefix()
      start_client_name = client_rpc_prefix ++ [:start]
      stop_client_name = client_rpc_prefix ++ [:stop]
      exception_client_name = client_rpc_prefix ++ [:exception]

      attach_events([
        start_server_name,
        stop_server_name,
        exception_server_name,
        start_client_name,
        stop_client_name,
        exception_client_name
      ])

      run_server([HelloServer], fn port ->
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

        req = %Helloworld.HelloRequest{name: "raise", duration: 1100}

        assert {:error, %GRPC.RPCError{status: 2}} =
                 Helloworld.Greeter.Stub.say_hello(channel, req)
      end)

      assert_received {^start_server_name, measurements, metadata}
      assert %{monotonic_time: _, system_time: _} = measurements

      assert %{
               server: HelloServer,
               endpoint: nil,
               function_name: :say_hello,
               stream: %GRPC.Server.Stream{}
             } = metadata

      assert_received {^exception_server_name, measurements, metadata}
      assert %{duration: duration} = measurements
      assert duration > 1100

      assert %{
               server: HelloServer,
               endpoint: nil,
               function_name: :say_hello,
               stream: %GRPC.Server.Stream{},
               kind: :error,
               reason: %ArgumentError{message: "exception raised"},
               stacktrace: stacktrace
             } = metadata

      assert is_list(stacktrace)

      Enum.each(stacktrace, fn entry ->
        assert {mod, fun, arity, meta} = entry
        assert is_atom(mod)
        assert is_atom(fun)
        assert is_integer(arity)
        assert is_list(meta)
      end)

      assert_received {^start_client_name, measurements, metadata}
      assert %{monotonic_time: _, system_time: _} = measurements

      assert %{
               stream: %GRPC.Client.Stream{
                 rpc:
                   {:SayHello, {Helloworld.HelloRequest, false}, {Helloworld.HelloReply, false},
                    %{}}
               }
             } = metadata

      assert_received {^stop_client_name, measurements, metadata}
      assert %{duration: duration} = measurements
      assert duration > 1100

      assert %{
               stream: %GRPC.Client.Stream{
                 rpc:
                   {:SayHello, {Helloworld.HelloRequest, false}, {Helloworld.HelloReply, false},
                    %{}}
               }
             } = metadata

      assert_received {:gun_down, _, _, _, _}

      refute_receive _
    end
  end
end
