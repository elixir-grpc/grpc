defmodule GRPC.Integration.ThousandIslandAdapterTest do
  @moduledoc """
  Integration tests for the ThousandIsland adapter.
  """

  use GRPC.Integration.TestCase

  setup do
    {:ok, adapter_opts: [adapter: GRPC.Server.Adapters.ThousandIsland]}
  end

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(request, materializer) do
      GRPC.Stream.unary(request, materializer: materializer)
      |> GRPC.Stream.map(fn req ->
        %Helloworld.HelloReply{message: "Hello #{req.name}!"}
      end)
      |> GRPC.Stream.run()
    end
  end

  defmodule RouteServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service
    require Logger

    def get_feature(point, materializer) do
      GRPC.Stream.unary(point, materializer: materializer)
      |> GRPC.Stream.map(fn point ->
        %Routeguide.Feature{
          location: point,
          name: "Feature at #{point.latitude},#{point.longitude}"
        }
      end)
      |> GRPC.Stream.run()
    end

    def list_features(_rectangle, materializer) do
      features =
        Enum.map(1..5, fn i ->
          %Routeguide.Feature{
            location: %Routeguide.Point{latitude: i * 10, longitude: i * 20},
            name: "Feature #{i}"
          }
        end)

      features
      |> GRPC.Stream.from()
      |> GRPC.Stream.run_with(materializer)
    end

    def record_route(point_stream, _materializer) do
      # For client streaming, process input and return single response
      count = Enum.reduce(point_stream, 0, fn _point, acc -> acc + 1 end)

      %Routeguide.RouteSummary{
        point_count: count,
        feature_count: count,
        distance: count * 100,
        elapsed_time: count * 10
      }
    end

    def route_chat(note_stream, materializer) do
      GRPC.Stream.from(note_stream)
      |> GRPC.Stream.map(fn note ->
        %Routeguide.RouteNote{
          # location: note.location,
          message: "Echo: #{note.message}"
        }
      end)
      |> GRPC.Stream.run_with(materializer)
    end
  end

  describe "ThousandIsland adapter - unary RPC" do
    test "handles simple unary request/response", %{adapter_opts: adapter_opts} do
      run_server(
        [HelloServer],
        fn port ->
          {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

          request = %Helloworld.HelloRequest{name: "ThousandIsland"}
          {:ok, response} = channel |> Helloworld.Greeter.Stub.say_hello(request)

          assert response.message == "Hello ThousandIsland!"

          GRPC.Stub.disconnect(channel)
        end,
        0,
        adapter_opts
      )
    end

    test "handles multiple sequential unary calls", %{adapter_opts: adapter_opts} do
      run_server(
        [HelloServer],
        fn port ->
          {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

          for i <- 1..10 do
            request = %Helloworld.HelloRequest{name: "User#{i}"}
            {:ok, response} = channel |> Helloworld.Greeter.Stub.say_hello(request)
            assert response.message == "Hello User#{i}!"
          end

          GRPC.Stub.disconnect(channel)
        end,
        0,
        adapter_opts
      )
    end
  end

  describe "ThousandIsland adapter - server streaming RPC" do
    test "receives multiple responses from server", %{adapter_opts: adapter_opts} do
      run_server(
        [RouteServer],
        fn port ->
          {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

          rectangle = %Routeguide.Rectangle{
            lo: %Routeguide.Point{latitude: 0, longitude: 0},
            hi: %Routeguide.Point{latitude: 100, longitude: 100}
          }

          {:ok, stream} = channel |> Routeguide.RouteGuide.Stub.list_features(rectangle)

          features = stream |> Enum.map(fn {:ok, f} -> f end) |> Enum.to_list()

          assert length(features) == 5

          Enum.each(1..5, fn i ->
            feature = Enum.at(features, i - 1)
            assert feature.name == "Feature #{i}"
            assert feature.location.latitude == i * 10
            assert feature.location.longitude == i * 20
          end)

          GRPC.Stub.disconnect(channel)
        end,
        0,
        adapter_opts
      )
    end
  end

  describe "ThousandIsland adapter - client streaming RPC" do
    test "sends multiple requests and receives single response", %{adapter_opts: adapter_opts} do
      run_server(
        [RouteServer],
        fn port ->
          {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

          points = [
            %Routeguide.Point{latitude: 10, longitude: 20},
            %Routeguide.Point{latitude: 30, longitude: 40},
            %Routeguide.Point{latitude: 50, longitude: 60}
          ]

          stream = channel |> Routeguide.RouteGuide.Stub.record_route()

          Enum.each(points, fn point ->
            GRPC.Stub.send_request(stream, point)
          end)

          GRPC.Stub.end_stream(stream)

          {:ok, summary} = GRPC.Stub.recv(stream)

          assert summary.point_count == 3
          assert summary.feature_count == 3
          assert summary.distance == 300

          GRPC.Stub.disconnect(channel)
        end,
        0,
        adapter_opts
      )
    end
  end

  describe "ThousandIsland adapter - bidirectional streaming RPC" do
    test "exchanges messages bidirectionally", %{adapter_opts: adapter_opts} do
      run_server(
        [RouteServer],
        fn port ->
          {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

          notes = [
            %Routeguide.RouteNote{
              location: %Routeguide.Point{latitude: 1, longitude: 2},
              message: "First note"
            },
            %Routeguide.RouteNote{
              location: %Routeguide.Point{latitude: 3, longitude: 4},
              message: "Second note"
            },
            %Routeguide.RouteNote{
              location: %Routeguide.Point{latitude: 5, longitude: 6},
              message: "Third note"
            }
          ]

          bidi_stream = channel |> Routeguide.RouteGuide.Stub.route_chat()

          Enum.each(notes, fn note ->
            GRPC.Stub.send_request(bidi_stream, note)
          end)

          GRPC.Stub.end_stream(bidi_stream)

          {:ok, response_stream} = GRPC.Stub.recv(bidi_stream)
          responses = response_stream |> Enum.map(fn {:ok, r} -> r end) |> Enum.to_list()

          assert length(responses) == 3
          assert Enum.at(responses, 0).message == "Echo: First note"
          assert Enum.at(responses, 1).message == "Echo: Second note"
          assert Enum.at(responses, 2).message == "Echo: Third note"

          GRPC.Stub.disconnect(channel)
        end,
        0,
        adapter_opts
      )
    end
  end

  describe "ThousandIsland adapter - HTTP/2 protocol validation" do
    test "handles multiple concurrent unary calls on same connection", %{
      adapter_opts: adapter_opts
    } do
      run_server(
        [HelloServer],
        fn port ->
          {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

          tasks =
            1..10
            |> Enum.map(fn i ->
              Task.async(fn ->
                request = %Helloworld.HelloRequest{name: "Concurrent#{i}"}
                {:ok, response} = channel |> Helloworld.Greeter.Stub.say_hello(request)
                response
              end)
            end)

          responses = Task.await_many(tasks, 5000)

          assert length(responses) == 10

          Enum.each(1..10, fn i ->
            response = Enum.find(responses, fn r -> r.message == "Hello Concurrent#{i}!" end)
            assert response != nil
          end)

          GRPC.Stub.disconnect(channel)
        end,
        0,
        adapter_opts
      )
    end
  end
end
