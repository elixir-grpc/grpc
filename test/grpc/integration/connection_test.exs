defmodule GRPC.Integration.ConnectionTest do
  use GRPC.Integration.TestCase, async: true

  @cert_path Path.expand("./tls/server1.pem", :code.priv_dir(:grpc))
  @key_path Path.expand("./tls/server1.key", :code.priv_dir(:grpc))
  @ca_path Path.expand("./tls/ca.pem", :code.priv_dir(:grpc))

  defmodule FeatureServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def get_feature(point, _stream) do
      Routeguide.Feature.new(location: point, name: "#{point.latitude},#{point.longitude}")
    end
  end

  test "reconnection works" do
    server = FeatureServer
    {:ok, _, port} = GRPC.Server.start(server, 0)
    point = Routeguide.Point.new(latitude: 409_146_138, longitude: -746_188_906)
    {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
    assert channel |> Routeguide.RouteGuide.Stub.get_feature(point)
    :ok = GRPC.Server.stop(server)
    {:ok, _, _} = GRPC.Server.start(server, port)
    assert channel |> Routeguide.RouteGuide.Stub.get_feature(point)
    :ok = GRPC.Server.stop(server)
  end

  test "authentication works" do
    server = FeatureServer
    cred = GRPC.Credential.server_tls(@cert_path, @key_path)
    {:ok, _, port} = GRPC.Server.start(server, 0, cred: cred)
    try do
      point = Routeguide.Point.new(latitude: 409_146_138, longitude: -746_188_906)
      client_cred = GRPC.Credential.client_tls(@ca_path)
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", cred: client_cred)
      assert channel |> Routeguide.RouteGuide.Stub.get_feature(point)
    after
      :ok = GRPC.Server.stop(server)
    end
  end
end
