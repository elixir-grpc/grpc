defmodule GRPC.ServiceTest do
  use ExUnit.Case, async: true

  defmodule Routeguide do
    @external_resource Path.expand("../../priv/protos/route_guide.proto", __DIR__)
    use Protobuf, from: Path.expand("../../priv/protos/route_guide.proto", __DIR__)

    defmodule RouteGuide.Service do
      use GRPC.Service, name: "routeguide.RouteGuide"

      rpc :GetFeature, Routeguide.Point, Routeguide.Feature
      rpc :ListFeatures, Routeguide.Rectangle, stream(Routeguide.Feature)
      rpc :RecordRoute, stream(Routeguide.Point), Routeguide.RouteSummary
      rpc :RouteChat, stream(Routeguide.RouteNote), stream(Routeguide.RouteNote)
    end

    defmodule RouteGuide.Stub do
      use GRPC.Stub, service: RouteGuide.Service
    end

    defmodule RouteGuide.Server do
      use GRPC.Server, service: RouteGuide.Service
      alias GRPC.Server

      def get_feature(point, _conn) do
        simple_feature(point)
      end

      def list_features(rectangle, conn) do
        Enum.each [rectangle.lo, rectangle.hi], fn (point)->
          feature = simple_feature(point)
          Server.stream_send(conn, feature)
        end
      end

      defp simple_feature(point) do
        Routeguide.Feature.new(location: point, name: "#{point.latitude},#{point.longitude}")
      end
    end
  end

  test "Unary RPC works" do
    GRPC.Server.start(Routeguide.RouteGuide.Server, "localhost:50051", insecure: true)

    {:ok, channel} = GRPC.Channel.connect("localhost:50051", insecure: true)
    point = Routeguide.Point.new(latitude: 409_146_138, longitude: -746_188_906)
    feature = channel |> Routeguide.RouteGuide.Stub.get_feature(point)
    assert feature == Routeguide.Feature.new(location: point, name: "409146138,-746188906")
    :ok = GRPC.Server.stop(Routeguide.RouteGuide.Server)
  end

  test "unary-stream works" do
    GRPC.Server.start(Routeguide.RouteGuide.Server, "localhost:10000", insecure: true)

    {:ok, channel} = GRPC.Channel.connect("localhost:10000", insecure: true)
    low = Routeguide.Point.new(latitude: 400000000, longitude: -750000000)
    high = Routeguide.Point.new(latitude: 420000000, longitude: -730000000)
    rect = Routeguide.Rectangle.new(lo: low, hi: high)
    stream = channel |> Routeguide.RouteGuide.Stub.list_features(rect)
    assert Enum.to_list(stream) == [
      Routeguide.Feature.new(location: low, name: "400000000,-750000000"),
      Routeguide.Feature.new(location: high, name: "420000000,-730000000")
    ]
    :ok = GRPC.Server.stop(Routeguide.RouteGuide.Server)
  end
end
