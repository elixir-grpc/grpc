defmodule FeatureServer do
  use GRPC.Server, service: Routeguide.RouteGuide.Service

  def get_feature(point, _stream) do
    if point.latitude != 0 do
      Routeguide.Feature.new(location: point, name: "#{point.latitude},#{point.longitude}")
    else
      {:error, "server error"}
    end
  end
end
