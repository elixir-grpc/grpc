defmodule GRPC.Integration.ConnectionTest do
  use GRPC.Integration.TestCase, async: true

  @cert_path Path.expand("./tls/server1.pem", :code.priv_dir(:grpc))
  @key_path Path.expand("./tls/server1.key", :code.priv_dir(:grpc))

  defmodule Routeguide do
    @external_resource Path.expand("protos/route_guide.proto", :code.priv_dir(:grpc))
    use Protobuf, from: Path.expand("protos/route_guide.proto", :code.priv_dir(:grpc))
  end
  defmodule Routeguide.RouteGuide.Service do
    use GRPC.Service, name: "routeguide.RouteGuide"

    rpc :GetFeature, Routeguide.Point, Routeguide.Feature
  end
  defmodule Routeguide.RouteGuide.Stub do
    use GRPC.Stub, service: Routeguide.RouteGuide.Service
  end

  defmodule Routeguide.RouteGuide.Server do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def get_feature(point, _stream) do
      Routeguide.Feature.new(location: point, name: "#{point.latitude},#{point.longitude}")
    end
  end

  test "reconnection works" do
    server = Routeguide.RouteGuide.Server
    {:ok, _, port} = GRPC.Server.start(server, 0, insecure: true)
    point = Routeguide.Point.new(latitude: 409_146_138, longitude: -746_188_906)
    {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", insecure: true)
    assert channel |> Routeguide.RouteGuide.Stub.get_feature(point)
    :ok = GRPC.Server.stop(server)
    {:ok, _, _} = GRPC.Server.start(server, port, insecure: true)
    assert channel |> Routeguide.RouteGuide.Stub.get_feature(point)
    :ok = GRPC.Server.stop(server)
  end
end
