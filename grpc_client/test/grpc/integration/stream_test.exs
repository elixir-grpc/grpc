defmodule GRPC.StreamTest do
  use GRPC.Integration.TestCase
  doctest GRPC.Stream

  describe "run/1" do
    defmodule MyGRPCService do
      use GRPC.Server, service: Routeguide.RouteGuide.Service

      def get_feature(input, materializer) do
        GRPC.Stream.unary(input, materializer: materializer)
        |> GRPC.Stream.map(fn point ->
          %Routeguide.Feature{location: point, name: "#{point.latitude},#{point.longitude}"}
        end)
        |> GRPC.Stream.run()
      end
    end

    test "runs a unary stream" do
      run_server([MyGRPCService], fn port ->
        point = %Routeguide.Point{latitude: 409_146_138, longitude: -746_188_906}
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter_opts: [retry_timeout: 10])

        expected_response = %Routeguide.Feature{
          location: point,
          name: "#{point.latitude},#{point.longitude}"
        }

        assert {:ok, response, %{trailers: trailers}} =
                 Routeguide.RouteGuide.Stub.get_feature(channel, point, return_headers: true)

        assert response == expected_response
        assert trailers == GRPC.Transport.HTTP2.server_trailers()
      end)
    end
  end
end
