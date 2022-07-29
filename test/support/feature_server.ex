defmodule FeatureServer do
  use GRPC.Server, service: Routeguide.RouteGuide.Service

  def get_feature(point, _stream) do
    Routeguide.Feature.new(location: point, name: "#{point.latitude},#{point.longitude}")
  end
end
