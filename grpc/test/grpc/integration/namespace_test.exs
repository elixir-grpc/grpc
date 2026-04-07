defmodule GRPC.Integration.NamespaceTest do
  use GRPC.Integration.TestCase

  defmodule FeatureServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def get_feature(point, _stream) do
      %Routeguide.Feature{location: point, name: "#{point.latitude},#{point.longitude}"}
    end
  end

  test "it works when outer namespace is same with inner's" do
    run_server(FeatureServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      point = %Routeguide.Point{latitude: 409_146_138, longitude: -746_188_906}
      {:ok, feature} = channel |> Routeguide.RouteGuide.Stub.get_feature(point)
      assert feature == %Routeguide.Feature{location: point, name: "409146138,-746188906"}
    end)
  end
end
