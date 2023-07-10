defmodule GRPC.Integration.EndpointTest do
  use GRPC.Integration.TestCase
  import ExUnit.CaptureLog

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream) do
      Helloworld.HelloReply.new(message: "Hello, #{req.name}")
    end
  end

  defmodule HelloEndpoint do
    use GRPC.Endpoint

    intercept GRPC.Server.Interceptors.Logger
    run HelloServer
  end

  defmodule HelloHaltInterceptor do
    def init(_), do: []

    def call(_, stream, _next, _) do
      {:ok, stream, Helloworld.HelloReply.new(message: "Hello by interceptor")}
    end
  end

  defmodule FeatureServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def get_feature(point, _stream) do
      Routeguide.Feature.new(location: point, name: "#{point.latitude},#{point.longitude}")
    end

    def list_features(rectangle, stream) do
      Enum.each([rectangle.lo, rectangle.hi], fn point ->
        feature = simple_feature(point)
        GRPC.Server.send_reply(stream, feature)
      end)
    end

    def record_route(enum, _stream) do
      Routeguide.RouteSummary.new(point_count: Enum.count(enum))
    end

    defp simple_feature(point) do
      Routeguide.Feature.new(location: point, name: "#{point.latitude},#{point.longitude}")
    end
  end

  defmodule FeatureEndpoint do
    use GRPC.Endpoint

    intercept GRPC.Server.Interceptors.Logger
    run FeatureServer
  end

  defmodule FeatureAndHelloHaltEndpoint do
    use GRPC.Endpoint

    intercept GRPC.Server.Interceptors.Logger
    run HelloServer, interceptors: [HelloHaltInterceptor]
    run FeatureServer
  end

  test "endpoint uses Logger interceptor for unary" do
    assert capture_log(fn ->
             run_endpoint(HelloEndpoint, fn port ->
               {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

               req = Helloworld.HelloRequest.new(name: "Elixir")
               {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
               assert reply.message == "Hello, Elixir"
             end)
           end) =~ "GRPC.Integration.EndpointTest.HelloServer.say_hello"
  end

  test "endpoint uses Logger interceptor for streaming server" do
    assert capture_log(fn ->
             run_endpoint(FeatureEndpoint, fn port ->
               {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

               point = Routeguide.Point.new(latitude: 409_146_138, longitude: -746_188_906)
               rect = Routeguide.Rectangle.new(hi: point, lo: point)
               {:ok, enum} = channel |> Routeguide.RouteGuide.Stub.list_features(rect)
               loc = Routeguide.Feature.new(location: point, name: "409146138,-746188906")
               assert [{:ok, loc}, {:ok, loc}] == Enum.to_list(enum)
             end)
           end) =~ "GRPC.Integration.EndpointTest.FeatureServer.list_features"
  end

  test "endpoint uses Logger interceptor for streaming client" do
    assert capture_log(fn ->
             run_endpoint(FeatureEndpoint, fn port ->
               {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

               point0 = Routeguide.Point.new(latitude: 0, longitude: -1)
               point1 = Routeguide.Point.new(latitude: 1, longitude: 1)
               stream = channel |> Routeguide.RouteGuide.Stub.record_route()
               GRPC.Stub.send_request(stream, point0)
               GRPC.Stub.send_request(stream, point1, end_stream: true)
               reply = GRPC.Stub.recv(stream)
               assert {:ok, Routeguide.RouteSummary.new(point_count: 2)} == reply
             end)
           end) =~ "GRPC.Integration.EndpointTest.FeatureServer.record_route"
  end

  test "endpoint uses Logger and custom interceptor" do
    assert capture_log(fn ->
             run_endpoint(FeatureAndHelloHaltEndpoint, fn port ->
               {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

               point = Routeguide.Point.new(latitude: 409_146_138, longitude: -746_188_906)
               {:ok, feature} = channel |> Routeguide.RouteGuide.Stub.get_feature(point)

               assert feature ==
                        Routeguide.Feature.new(location: point, name: "409146138,-746188906")

               req = Helloworld.HelloRequest.new(name: "Elixir")
               {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
               assert reply.message == "Hello by interceptor"
             end)
           end) =~ "GRPC.Integration.EndpointTest.HelloServer.say_hello"
  end
end
