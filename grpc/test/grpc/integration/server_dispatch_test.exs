defmodule GRPC.Integration.ServerDispatchTest do
  use GRPC.Integration.TestCase

  defmodule FeatureServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def get_feature(point, _stream), do: %Routeguide.Feature{location: point, name: "feature"}
  end

  # Serves the same service as `FeatureServer`, but has no `get_feature`.
  defmodule ListServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def list_features(_rectangle, _materializer), do: :ok
  end

  test "a server keeps its own routes when another server starts later" do
    run_server(FeatureServer, fn port ->
      run_server(ListServer, fn _other_port ->
        {:ok, channel} = GRPC.Stub.connect("127.0.0.1:#{port}")
        point = %Routeguide.Point{latitude: 1, longitude: 0}

        assert {:ok, %Routeguide.Feature{name: "feature"}} =
                 Routeguide.RouteGuide.Stub.get_feature(channel, point)

        GRPC.Stub.disconnect(channel)
      end)
    end)
  end
end
