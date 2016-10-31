defmodule Routeguide.RouteGuide.Server do
  use GRPC.Server, service: Routeguide.RouteGuide.Service
  alias GRPC.Server
  @json_path Path.expand("../priv/route_guide_db.json", __DIR__)

  def get_feature(point, %{state: features} = stream) do
    default_feature = Routeguide.Feature.new(location: point)
    Enum.find features, default_feature, fn (feature) ->
      feature.location == point
    end
  end

  def list_features(rect, %{state: features} = stream) do
    features
    |> Enum.filter(fn %{location: loc} -> in_range?(loc, rect) end)
    |> Enum.each(fn feature -> Server.stream_send(stream, feature) end)
  end

  def load_features do
    data = File.read!(@json_path)
    items = Poison.Parser.parse!(data)
    Enum.map items, fn (%{"location" => location, "name" => name}) ->
      point = Routeguide.Point.new(latitude: location["latitude"], longitude: location["longitude"])
      Routeguide.Feature.new(name: name, location: point)
    end
  end

  def in_range?(%{longitude: long, latitude: lat} = point, %{lo: low, hi: high} = rect) do
    left = min(low.longitude, high.longitude)
    right = max(low.longitude, high.longitude)
    bottom = min(low.latitude, high.latitude)
    top = max(low.latitude, high.latitude)

    long >= left && long <= right && lat >= bottom && lat <= top
  end
end
