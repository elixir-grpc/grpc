defmodule FeatureServer do
  use GRPC.Server, service: Routeguide.RouteGuide.Service

  def get_feature(point, _stream) do
    if point.latitude != 0 do
      %Routeguide.Feature{location: point, name: "#{point.latitude},#{point.longitude}"}
    else
      {:error, "server error"}
    end
  end

  # Client-streaming: drain the enum so the HTTP/2 stream stays open until the
  # client sends eof. Mint ConnectionProcess unit tests open RecordRoute and
  # need the request_ref to remain in state.requests while they exercise
  # stream_body/cancel — an unimplemented handler replies immediately and
  # pops the ref (flake under --repeat-until-failure).
  def record_route(req_enum, _stream) do
    points = Enum.to_list(req_enum)

    %Routeguide.RouteSummary{
      point_count: length(points),
      feature_count: 0,
      distance: 0,
      elapsed_time: 0
    }
  end
end
