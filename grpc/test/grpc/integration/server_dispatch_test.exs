defmodule GRPC.Integration.ServerDispatchTest do
  use GRPC.Integration.TestCase

  defmodule FeatureServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def get_feature(point, _stream), do: %Routeguide.Feature{location: point, name: "feature"}
  end

  # Serves the same service as `FeatureServer`, but has no `get_feature`.
  defmodule ListServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def list_features(rectangle, stream) do
      GRPC.Server.send_reply(stream, %Routeguide.Feature{location: rectangle.lo, name: "listed"})
    end
  end

  test "a server keeps its own routes when another server starts later" do
    run_server(FeatureServer, fn port ->
      run_server(ListServer, fn other_port ->
        {:ok, channel} = GRPC.Stub.connect("127.0.0.1:#{port}")
        {:ok, other_channel} = GRPC.Stub.connect("127.0.0.1:#{other_port}")
        point = %Routeguide.Point{latitude: 1, longitude: 0}
        rectangle = %Routeguide.Rectangle{lo: point, hi: point}

        assert {:ok, %Routeguide.Feature{name: "feature"}} =
                 Routeguide.RouteGuide.Stub.get_feature(channel, point)

        assert {:ok, stream} = Routeguide.RouteGuide.Stub.list_features(other_channel, rectangle)
        assert [{:ok, %Routeguide.Feature{name: "listed"}}] = Enum.to_list(stream)

        GRPC.Stub.disconnect(channel)
        GRPC.Stub.disconnect(other_channel)
      end)
    end)
  end
end
