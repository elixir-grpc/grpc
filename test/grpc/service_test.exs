defmodule GRPC.ServiceTest do
  use ExUnit.Case, async: true

  defmodule Routeguide do
    @external_resource Path.expand("../../priv/protos/route_guide.proto", __DIR__)
    use Protobuf, from: Path.expand("../../priv/protos/route_guide.proto", __DIR__)
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

    def record_route(req_stream, _stream) do
      points = Enum.reduce req_stream, [], fn (point, acc) ->
        [point|acc]
      end
      fake_num = length(points)
      Routeguide.RouteSummary.new(point_count: fake_num, feature_count: fake_num,
                                  distance: fake_num, elapsed_time: fake_num)
    end

    def route_chat(req_stream, stream) do
      Enum.each req_stream, fn note ->
        note = %{note | message: "Reply: #{note.message}"}
        Server.stream_send(stream, note)
      end
    end

    defp simple_feature(point) do
      Routeguide.Feature.new(location: point, name: "#{point.latitude},#{point.longitude}")
    end
  end

  # For the test case "it works when outer namespace is same with inner's"
  defmodule Foo do
    @external_resource Path.expand("../../priv/protos/route_guide.proto", __DIR__)
    use Protobuf, from: Path.expand("../../priv/protos/route_guide.proto", __DIR__)
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

    def get_feature(point, _conn) do
      Foo.Feature.new(location: point, name: "#{point.latitude},#{point.longitude}")
    end
  end

  def run_server(server, func) do
    {:ok, _, port} = GRPC.Server.start(server, "localhost:0", insecure: true)
    try do
      func.(port)
    after
      :ok = GRPC.Server.stop(server)
    end
  end

  test "Unary RPC works" do
    run_server Routeguide.RouteGuide.Server, fn(port) ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", insecure: true)
      point = Routeguide.Point.new(latitude: 409_146_138, longitude: -746_188_906)
      feature = channel |> Routeguide.RouteGuide.Stub.get_feature(point)
      assert feature == Routeguide.Feature.new(location: point, name: "409146138,-746188906")
    end
  end

  test "it works when outer namespace is same with inner's" do
    run_server Foo.RouteGuide.Server, fn(port) ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", insecure: true)
      point = Foo.Point.new(latitude: 409_146_138, longitude: -746_188_906)
      feature = channel |> Foo.RouteGuide.Stub.get_feature(point)
      assert feature == Foo.Feature.new(location: point, name: "409146138,-746188906")
    end
  end

  test "Server streaming RPC works" do
    run_server Routeguide.RouteGuide.Server, fn(port) ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", insecure: true)
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
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", insecure: true)
      point1 = Routeguide.Point.new(latitude: 400000000, longitude: -750000000)
      point2 = Routeguide.Point.new(latitude: 420000000, longitude: -730000000)
      stream = channel |> Routeguide.RouteGuide.Stub.record_route
      GRPC.Stub.stream_send(stream, point1)
      GRPC.Stub.stream_send(stream, point2, end_stream: true)
      res = GRPC.Stub.recv(stream)
      assert %GRPC.ServiceTest.Routeguide.RouteSummary{point_count: 2} = res
    end
  end

  test "Bidirectional streaming RPC works" do
    run_server Routeguide.RouteGuide.Server, fn(port) ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", insecure: true)
      stream = channel |> Routeguide.RouteGuide.Stub.route_chat
      task = Task.async(fn ->
        Enum.each(1..6, fn (i) ->
          point = Routeguide.Point.new(latitude: 0, longitude: rem(i, 3) + 1)
          note = Routeguide.RouteNote.new(location: point, message: "Message #{i}")
          opts = if i == 6, do: [end_stream: true], else: []
          GRPC.Stub.stream_send(stream, note, opts)
        end)
      end)
      stream_result = GRPC.Stub.recv(stream)
      Task.await(task)
      notes = Enum.map stream_result, fn (note)->
        assert "Reply: " <> _msg = note.message
        note
      end
      assert length(notes) == 6
    end
  end
end
