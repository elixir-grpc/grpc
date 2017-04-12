defmodule GRPC.Integration.ServiceTest do
  use GRPC.Integration.TestCase, async: true

  defmodule Routeguide do
    @external_resource Path.expand("./protos/route_guide.proto", :code.priv_dir(:grpc))
    use Protobuf, from: Path.expand("./protos/route_guide.proto", :code.priv_dir(:grpc))
  end
  defmodule Routeguide.RouteGuide.Service do
    use GRPC.Service, name: "routeguide.RouteGuide"

    rpc :GetFeature, Routeguide.Point, Routeguide.Feature
    rpc :ListFeatures, Routeguide.Rectangle, stream(Routeguide.Feature)
    rpc :RecordRoute, stream(Routeguide.Point), Routeguide.RouteSummary
    rpc :RouteChat, stream(Routeguide.RouteNote), stream(Routeguide.RouteNote)
  end
  defmodule Routeguide.RouteGuide.Stub do
    use GRPC.Stub, service: Routeguide.RouteGuide.Service
  end

  defmodule Routeguide.RouteGuide.Server do
    use GRPC.Server, service: Routeguide.RouteGuide.Service
    alias GRPC.Server

    def get_feature(point, _stream) do
      simple_feature(point)
    end

    def list_features(rectangle, stream) do
      Enum.each [rectangle.lo, rectangle.hi], fn (point)->
        feature = simple_feature(point)
        Server.stream_send(stream, feature)
      end
    end

    def record_route(req_enum, _stream) do
      points = Enum.reduce req_enum, [], fn (point, acc) ->
        [point|acc]
      end
      fake_num = length(points)
      Routeguide.RouteSummary.new(point_count: fake_num, feature_count: fake_num,
                                  distance: fake_num, elapsed_time: fake_num)
    end

    def route_chat(req_enum, stream) do
      Enum.each req_enum, fn note ->
        note = %{note | message: "Reply: #{note.message}"}
        Server.stream_send(stream, note)
      end
    end

    defp simple_feature(point) do
      Routeguide.Feature.new(location: point, name: "#{point.latitude},#{point.longitude}")
    end
  end

  test "Unary RPC works" do
    run_server Routeguide.RouteGuide.Server, fn(port) ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      point = Routeguide.Point.new(latitude: 409_146_138, longitude: -746_188_906)
      feature = channel |> Routeguide.RouteGuide.Stub.get_feature(point)
      assert feature == Routeguide.Feature.new(location: point, name: "409146138,-746188906")
    end
  end

  test "Server streaming RPC works" do
    run_server Routeguide.RouteGuide.Server, fn(port) ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      low = Routeguide.Point.new(latitude: 400000000, longitude: -750000000)
      high = Routeguide.Point.new(latitude: 420000000, longitude: -730000000)
      rect = Routeguide.Rectangle.new(lo: low, hi: high)
      stream = channel |> Routeguide.RouteGuide.Stub.list_features(rect)
      assert Enum.to_list(stream) == [
        Routeguide.Feature.new(location: low, name: "400000000,-750000000"),
        Routeguide.Feature.new(location: high, name: "420000000,-730000000")
      ]
    end
  end

  test "Client streaming RPC works" do
    run_server Routeguide.RouteGuide.Server, fn(port) ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      point1 = Routeguide.Point.new(latitude: 400000000, longitude: -750000000)
      point2 = Routeguide.Point.new(latitude: 420000000, longitude: -730000000)
      stream = channel |> Routeguide.RouteGuide.Stub.record_route
      GRPC.Stub.stream_send(stream, point1)
      GRPC.Stub.stream_send(stream, point2, end_stream: true)
      res = GRPC.Stub.recv(stream)
      assert %GRPC.Integration.ServiceTest.Routeguide.RouteSummary{point_count: 2} = res
    end
  end

  test "Bidirectional streaming RPC works" do
    run_server Routeguide.RouteGuide.Server, fn(port) ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      stream = channel |> Routeguide.RouteGuide.Stub.route_chat
      task = Task.async(fn ->
        Enum.each(1..6, fn (i) ->
          point = Routeguide.Point.new(latitude: 0, longitude: rem(i, 3) + 1)
          note = Routeguide.RouteNote.new(location: point, message: "Message #{i}")
          opts = if i == 6, do: [end_stream: true], else: []
          GRPC.Stub.stream_send(stream, note, opts)
        end)
      end)
      result_enum = GRPC.Stub.recv(stream)
      Task.await(task)
      notes = Enum.map result_enum, fn (note)->
        assert "Reply: " <> _msg = note.message
        note
      end
      assert length(notes) == 6
    end
  end
end
