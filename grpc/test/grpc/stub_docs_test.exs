defmodule GRPC.StubDocsTest do
  use ExUnit.Case, async: true

  defp function_doc(module, name, arity) do
    {:docs_v1, _, _, _, _, _, docs} = Code.fetch_docs(module)

    case Enum.find(docs, &match?({{:function, ^name, ^arity}, _, _, _, _}, &1)) do
      {_, _, _, %{"en" => doc}, _} -> doc
      {_, _, _, other, _} -> other
      nil -> nil
    end
  end

  describe "generated function docs" do
    test "unary RPC" do
      # The default `opts` argument makes the arity 3.
      doc = function_doc(Helloworld.Greeter.Stub, :say_hello, 3)

      assert doc =~ "`SayHello` RPC"
      assert doc =~ "/helloworld.Greeter/SayHello"
      assert doc =~ "unary"
      assert doc =~ "Helloworld.HelloRequest"
      assert doc =~ "Helloworld.HelloReply"
      assert doc =~ "Helloworld.Greeter.Stub.say_hello(channel, request)"
      assert doc =~ "return_headers: true"
    end

    test "server streaming RPC" do
      doc = function_doc(Routeguide.RouteGuide.Stub, :list_features, 3)

      assert doc =~ "`ListFeatures` RPC"
      assert doc =~ "server streaming"
      assert doc =~ "Routeguide.Rectangle"
      assert doc =~ "Routeguide.Feature"
      assert doc =~ "{:trailers, trailers}"
    end

    test "client streaming RPC" do
      doc = function_doc(Routeguide.RouteGuide.Stub, :record_route, 2)

      assert doc =~ "`RecordRoute` RPC"
      assert doc =~ "client streaming"
      assert doc =~ "Routeguide.Point"
      assert doc =~ "Routeguide.RouteSummary"
      assert doc =~ "GRPC.Stub.send_request/3"
      assert doc =~ "GRPC.Stub.end_stream/1"
      assert doc =~ "GRPC.Stub.recv/2"
    end

    test "bidirectional streaming RPC" do
      doc = function_doc(Routeguide.RouteGuide.Stub, :route_chat, 2)

      assert doc =~ "`RouteChat` RPC"
      assert doc =~ "bidirectional streaming"
      assert doc =~ "Routeguide.RouteNote"
      assert doc =~ "GRPC.Stub.recv/2"
    end

    test "each function doc is specific to its RPC" do
      docs =
        for {stub, name, arity} <- [
              {Helloworld.Greeter.Stub, :say_hello, 3},
              {Helloworld.Greeter.Stub, :check_headers, 3},
              {Helloworld.Greeter.Stub, :loop_back, 3},
              {Routeguide.RouteGuide.Stub, :get_feature, 3},
              {Routeguide.RouteGuide.Stub, :list_features, 3},
              {Routeguide.RouteGuide.Stub, :record_route, 2},
              {Routeguide.RouteGuide.Stub, :route_chat, 2}
            ] do
          doc = function_doc(stub, name, arity)
          assert is_binary(doc), "#{inspect(stub)}.#{name}/#{arity} has no doc"
          doc
        end

      assert length(Enum.uniq(docs)) == length(docs)
    end

    test "functions generated for the health service are documented" do
      assert function_doc(GRPC.Health.V1.Health.Stub, :check, 3) =~
               "GRPC.Health.V1.HealthCheckRequest"
    end

    test "colliding RPCs get no convenience function and no doc" do
      stub = CollisionTest.CollisionService.Stub

      refute function_exported?(stub, :process_data, 2)
      refute function_exported?(stub, :process_data, 3)
      assert function_doc(stub, :process_data, 3) == nil

      doc = function_doc(stub, :get_user, 3)
      assert doc =~ "`GetUser` RPC"
      assert doc =~ "CollisionTest.Request"
      assert doc =~ "CollisionTest.Response"
    end
  end
end
