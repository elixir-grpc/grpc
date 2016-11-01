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

  def record_route(req_stream, %{state: features} = stream) do
    start_time = now_ts
    {_, distance, point_count, feature_count} =
      Enum.reduce req_stream, {nil, 0, 0, 0}, fn (point, {last, distance, point_count, feature_count}) ->
        point_count = point_count + 1
        found_feature = Enum.find features, fn (f) -> f.location == point end
        feature_count = if found_feature, do: feature_count + 1, else: feature_count
        distance = if last, do: distance + calc_distance(last, point), else: distance
        {point, distance, point_count, feature_count}
      end
    Routeguide.RouteSummary.new(point_count: point_count, feature_count: feature_count,
                                distance: distance, elapsed_time: now_ts - start_time)
  end

  def load_features do
    data = File.read!(@json_path)
    items = Poison.Parser.parse!(data)
    Enum.map items, fn (%{"location" => location, "name" => name}) ->
      point = Routeguide.Point.new(latitude: location["latitude"], longitude: location["longitude"])
      Routeguide.Feature.new(name: name, location: point)
    end
  end

  defp in_range?(%{longitude: long, latitude: lat} = point, %{lo: low, hi: high} = rect) do
    left = min(low.longitude, high.longitude)
    right = max(low.longitude, high.longitude)
    bottom = min(low.latitude, high.latitude)
    top = max(low.latitude, high.latitude)

    long >= left && long <= right && lat >= bottom && lat <= top
  end

  defp now_ts do
    DateTime.utc_now() |> DateTime.to_unix()
  end

  # calcDistance calculates the distance between two points using the "haversine" formula.
  # This code was taken from http://www.movable-type.co.uk/scripts/latlong.html.
  defp calc_distance(p1, p2) do
    cord_factor = 1.0e7
    r = 6371000.0
    lat1 = p1.latitude / cord_factor
    lat2 = p2.latitude / cord_factor
    lng1 = p1.longitude / cord_factor
    lng2 = p2.longitude / cord_factor
    phi1 = to_radians(lat1)
    phi2 = to_radians(lat2)
    delta_phi = to_radians(lat2 - lat1)
    delta_lambda = to_radians(lng2 - lng1)

    a = sqr(:math.sin(delta_phi/2)) + :math.cos(phi1) * :math.cos(phi2) * sqr(:math.sin(delta_lambda/2))
    c = 2 * :math.atan2(:math.sqrt(a), :math.sqrt(1 - a))
    round(r * c)
  end

  defp to_radians(num) do
    num * :math.pi / 180
  end

  defp sqr(num) do
    num * num
  end
end
