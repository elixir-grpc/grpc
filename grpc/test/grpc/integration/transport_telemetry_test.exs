defmodule GRPC.Integration.TransportTelemetryTest do
  # async: false — toggles global Application env and uses global telemetry handlers
  use ExUnit.Case, async: false

  import GRPC.Integration.TestCase

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream) do
      %Helloworld.HelloReply{message: "Hello, #{req.name}"}
    end
  end

  @events for event <- [:started, :connected, :streams],
              do: [:grpc, :client, :transport, event]

  setup do
    previous = Application.get_env(:grpc, :pool_enabled)
    on_exit(fn -> Application.put_env(:grpc, :pool_enabled, previous) end)

    test_pid = self()
    handler_id = {__MODULE__, make_ref()}

    :telemetry.attach_many(
      handler_id,
      @events,
      fn event, measurements, metadata, _config ->
        send(test_pid, {:telemetry, event, measurements, metadata})
      end,
      nil
    )

    on_exit(fn -> :telemetry.detach(handler_id) end)
  end

  for adapter <- [GRPC.Client.Adapters.Gun, GRPC.Client.Adapters.Mint] do
    @adapter adapter

    test "pooled connections report the logical connection ref (#{inspect(adapter)})" do
      Application.put_env(:grpc, :pool_enabled, true)
      logical_ref = make_ref()

      run_server(HelloServer, fn port ->
        {:ok, channel} =
          GRPC.Stub.connect("localhost:#{port}",
            name: logical_ref,
            adapter: @adapter,
            pool: %{size: 2}
          )

        assert channel.ref == logical_ref

        assert_receive {:telemetry, [:grpc, :client, :transport, :started], _,
                        %{logical_connection_ref: ^logical_ref, transport_ref: first}}

        assert_receive {:telemetry, [:grpc, :client, :transport, :started], _,
                        %{logical_connection_ref: ^logical_ref, transport_ref: second}}

        assert first != second

        for transport_ref <- [first, second] do
          assert_receive {:telemetry, [:grpc, :client, :transport, :connected], %{generation: 1},
                          %{
                            logical_connection_ref: ^logical_ref,
                            transport_ref: ^transport_ref,
                            reconnect: false
                          }}
        end

        assert {:ok, %Helloworld.HelloReply{message: "Hello, World"}} =
                 Helloworld.Greeter.Stub.say_hello(channel, %Helloworld.HelloRequest{
                   name: "World"
                 })

        assert_receive {:telemetry, [:grpc, :client, :transport, :streams], %{active: 1},
                        %{logical_connection_ref: ^logical_ref, transport_ref: used}}

        assert used in [first, second]

        assert_receive {:telemetry, [:grpc, :client, :transport, :streams], %{active: 0},
                        %{logical_connection_ref: ^logical_ref, transport_ref: ^used}}

        GRPC.Stub.disconnect(channel)
      end)
    end
  end

  test "legacy connections report the logical connection ref" do
    Application.put_env(:grpc, :pool_enabled, false)
    logical_ref = make_ref()

    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", name: logical_ref)

      assert_receive {:telemetry, [:grpc, :client, :transport, :connected], %{generation: 1},
                      %{logical_connection_ref: ^logical_ref, transport_ref: transport_ref}}

      assert {:ok, %Helloworld.HelloReply{}} =
               Helloworld.Greeter.Stub.say_hello(channel, %Helloworld.HelloRequest{name: "World"})

      assert_receive {:telemetry, [:grpc, :client, :transport, :streams], %{active: 1},
                      %{logical_connection_ref: ^logical_ref, transport_ref: ^transport_ref}}

      assert_receive {:telemetry, [:grpc, :client, :transport, :streams], %{active: 0},
                      %{logical_connection_ref: ^logical_ref, transport_ref: ^transport_ref}}

      GRPC.Stub.disconnect(channel)
    end)
  end
end
