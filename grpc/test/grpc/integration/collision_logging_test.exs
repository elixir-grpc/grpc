if Application.get_env(:grpc, :run_warning_tests) do
  defmodule GRPC.Integration.CollisionLoggingTest do
    use GRPC.Integration.TestCase
    import ExUnit.CaptureLog

    @moduletag :warning_test

    defmodule CollisionServer do
      use GRPC.Server, service: CollisionTest.CollisionService.Service

      def process_data(req, stream) do
        # Both :process_data and :ProcessData RPCs dispatch to this function
        # We can check stream.rpc to see which one was called
        rpc_name = elem(stream.rpc, 0)
        %CollisionTest.Response{result: "#{rpc_name}: #{req.data}"}
      end

      def get_user(req, _stream) do
        %CollisionTest.Response{result: "user: #{req.data}"}
      end
    end

    defmodule CollisionEndpoint do
      use GRPC.Endpoint

      intercept(GRPC.Server.Interceptors.Logger)
      run(CollisionServer)
    end

    test "logs colliding lowercase RPC with call(:Name, ...) format" do
      log =
        capture_log(fn ->
          run_endpoint(CollisionEndpoint, fn port ->
            {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

            # Test lowercase process_data (colliding)
            req = %CollisionTest.Request{data: "test1"}

            {:ok, reply} =
              CollisionTest.CollisionService.Stub.call(:process_data, channel, req, [])

            assert reply.result == "process_data: test1"
          end)
        end)

      assert log =~
               "GRPC.Integration.CollisionLoggingTest.CollisionServer.call(:process_data, ...)"
    end

    test "logs colliding PascalCase RPC with call(:Name, ...) format" do
      log =
        capture_log(fn ->
          run_endpoint(CollisionEndpoint, fn port ->
            {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

            # Test PascalCase ProcessData (colliding)
            req = %CollisionTest.Request{data: "test2"}

            {:ok, reply} =
              CollisionTest.CollisionService.Stub.call(:ProcessData, channel, req, [])

            assert reply.result == "ProcessData: test2"
          end)
        end)

      assert log =~
               "GRPC.Integration.CollisionLoggingTest.CollisionServer.call(:ProcessData, ...)"
    end

    test "logs non-colliding functions with underscored name" do
      log =
        capture_log(fn ->
          run_endpoint(CollisionEndpoint, fn port ->
            {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

            # Test GetUser (non-colliding)
            req = %CollisionTest.Request{data: "john"}
            {:ok, reply} = channel |> CollisionTest.CollisionService.Stub.get_user(req)
            assert reply.result == "user: john"
          end)
        end)

      assert log =~ "GRPC.Integration.CollisionLoggingTest.CollisionServer.get_user"
      refute log =~ "call(:GetUser"
    end
  end
end
