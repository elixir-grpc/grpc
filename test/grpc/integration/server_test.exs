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

  test "multiple servers works" do
    run_server [FeatureServer, HelloServer], fn(port) ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      point = Routeguide.Point.new(latitude: 409_146_138, longitude: -746_188_906)
      feature = channel |> Routeguide.RouteGuide.Stub.get_feature(point)
      assert feature == Routeguide.Feature.new(location: point, name: "409146138,-746188906")

      req = Helloworld.HelloRequest.new(name: "Elixir")
      reply = channel |> Helloworld.Greeter.Stub.say_hello(req)
      assert reply.message == "Hello, Elixir"
    end
  end
end
