defmodule Routeguide.RouteGuide.Server do
  use GRPC.Server, service: Routeguide.RouteGuide.Service
  alias GRPC.Server
  alias RouteGuide.Data

  def get_feature(point, _stream) do
    features = Data.fetch_features
    default_feature = Routeguide.Feature.new(location: point)
    Enum.find features, default_feature, fn (feature) ->
      feature.location == point
    end
  end

  def list_features(rect, stream) do
    features = Data.fetch_features
    features
    |> Enum.filter(fn %{location: loc} -> in_range?(loc, rect) end)
    |> Enum.each(fn feature -> Server.stream_send(stream, feature) end)
  end

  def record_route(req_enum, _stream) do
    features = Data.fetch_features
    start_time = now_ts
    {_, distance, point_count, feature_count} =
      Enum.reduce req_enum, {nil, 0, 0, 0}, fn (point, {last, distance, point_count, feature_count}) ->
        point_count = point_count + 1
        found_feature = Enum.find features, fn (f) -> f.location == point end
        feature_count = if found_feature, do: feature_count + 1, else: feature_count
        distance = if last, do: distance + calc_distance(last, point), else: distance
        {point, distance, point_count, feature_count}
      end
    Routeguide.RouteSummary.new(point_count: point_count, feature_count: feature_count,
                                distance: distance, elapsed_time: now_ts - start_time)
  end

  def route_chat(req_enum, stream) do
    notes = Enum.reduce req_enum, Data.fetch_notes, fn (note, notes) ->
      key = serialize_location(note.location)
      new_notes = Map.update(notes, key, [note], &(&1 ++ [note]))
      Enum.each new_notes[key], fn note ->
        IO.inspect note
        Server.stream_send(stream, note)
      end
      new_notes
    end
    Data.update_notes(notes)
  end

  defp in_range?(%{longitude: long, latitude: lat}, %{lo: low, hi: high}) do
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
    lat1 = (p1.latitude || 0) / cord_factor
    lat2 = (p2.latitude || 0) / cord_factor
    lng1 = (p1.longitude || 0) / cord_factor
    lng2 = (p2.longitude || 0) / cord_factor
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

  def serialize_location(p) do
    "#{p.latitude} #{p.longitude}"
  end
end
