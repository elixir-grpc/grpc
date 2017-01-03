defmodule GRPC.Integration.NamespaceTest do
  use GRPC.Integration.TestCase, async: true

  defmodule Foo do
    @external_resource Path.expand("./protos/route_guide.proto", :code.priv_dir(:grpc))
    use Protobuf, from: Path.expand("./protos/route_guide.proto", :code.priv_dir(:grpc))
  end
  defmodule Foo.Foo.Service do
    use GRPC.Service, name: "routeguide.RouteGuide"
    rpc :GetFeature, Foo.Point, Foo.Feature
  end
  defmodule Foo.RouteGuide.Stub do
    use GRPC.Stub, service: Foo.Foo.Service
  end
  defmodule Foo.RouteGuide.Server do
    use GRPC.Server, service: Foo.Foo.Service

    def get_feature(point, _stream) do
      Foo.Feature.new(location: point, name: "#{point.latitude},#{point.longitude}")
    end
  end

  test "it works when outer namespace is same with inner's" do
    run_server Foo.RouteGuide.Server, fn(port) ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      point = Foo.Point.new(latitude: 409_146_138, longitude: -746_188_906)
      feature = channel |> Foo.RouteGuide.Stub.get_feature(point)
      assert feature == Foo.Feature.new(location: point, name: "409146138,-746188906")
    end
  end
end
