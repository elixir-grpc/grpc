defmodule GRPC.Integration.ServerTest do
  use GRPC.Integration.TestCase, async: true

  defmodule FeatureServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def get_feature(point, _stream) do
      Routeguide.Feature.new(location: point, name: "#{point.latitude},#{point.longitude}")
    end
  end

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream) do
      Helloworld.HelloReply.new(message: "Hello, #{req.name}")
    end
  end

  defmodule HelloErrorServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(_req, _stream) do
      raise GRPC.RPCError, status: GRPC.Status.unauthenticated, message: "Please authenticate"
    end
  end

  defmodule FeatureErrorServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service
    alias GRPC.Server

    def list_features(rectangle, stream) do
      raise GRPC.RPCError, status: GRPC.Status.unauthenticated, message: "Please authenticate"
      Enum.each [rectangle.lo, rectangle.hi], fn (point)->
        feature = simple_feature(point)
        Server.stream_send(stream, feature)
      end
    end

    defp simple_feature(point) do
      Routeguide.Feature.new(location: point, name: "#{point.latitude},#{point.longitude}")
    end
  end

  test "multiple servers works" do
    run_server [FeatureServer, HelloServer], fn(port) ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      point = Routeguide.Point.new(latitude: 409_146_138, longitude: -746_188_906)
      {:ok, feature} = channel |> Routeguide.RouteGuide.Stub.get_feature(point)
      assert feature == Routeguide.Feature.new(location: point, name: "409146138,-746188906")

      req = Helloworld.HelloRequest.new(name: "Elixir")
      {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
      assert reply.message == "Hello, Elixir"
    end
  end

  test "returns appropriate error for unary requests" do
    run_server [HelloErrorServer], fn(port) ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      req = Helloworld.HelloRequest.new(name: "Elixir")
      {:error, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
      assert reply == %GRPC.RPCError{status: GRPC.Status.unauthenticated, message: "Please authenticate"}
    end
  end

  test "returns appropriate error for stream requests" do
    run_server [FeatureErrorServer], fn(port) ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      low = Routeguide.Point.new(latitude: 400000000, longitude: -750000000)
      high = Routeguide.Point.new(latitude: 420000000, longitude: -730000000)
      rect = Routeguide.Rectangle.new(lo: low, hi: high)
      stream = channel |> Routeguide.RouteGuide.Stub.list_features(rect)
      Enum.each(stream, fn(thing) -> 
        IO.inspect thing 
      end)
    end
  end
end
