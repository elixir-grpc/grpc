defmodule RouteGuide.Data do
  use Agent

  @json_path Path.expand("../priv/route_guide_db.json", __DIR__)

  def start_link(_) do
    features = load_features()
    Agent.start_link(fn -> %{features: features, notes: %{}} end, name: __MODULE__)
  end

  def fetch_features do
    Agent.get(__MODULE__, &Map.get(&1, :features))
  end

  def fetch_notes do
    Agent.get(__MODULE__, &Map.get(&1, :notes))
  end

  def update_notes(notes) do
    Agent.update(__MODULE__, &Map.put(&1, :notes, notes))
  end

  defp load_features(path \\ @json_path) do
    data = File.read!(path)
    items = Jason.decode!(data)

    for %{"location" => location, "name" => name} <- items do
      point =
        Routeguide.Point.new(latitude: location["latitude"], longitude: location["longitude"])

      Routeguide.Feature.new(name: name, location: point)
    end
  end
end
