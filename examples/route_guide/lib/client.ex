defmodule RouteGuide.Client do
  def main(channel) do
    print_feature(channel, Routeguide.Point.new(latitude: 409_146_138, longitude: -746_188_906))
    print_feature(channel, Routeguide.Point.new(latitude: 0, longitude: 0))

    # Looking for features between 40, -75 and 42, -73.
    print_features(
      channel,
      Routeguide.Rectangle.new(
        lo: Routeguide.Point.new(latitude: 400_000_000, longitude: -750_000_000),
        hi: Routeguide.Point.new(latitude: 420_000_000, longitude: -730_000_000)
      )
    )

    run_record_route(channel)

    run_route_chat(channel)
  end

  def print_feature(channel, point) do
    IO.puts("Getting feature for point (#{point.latitude}, #{point.longitude})")
    {:ok, reply} = channel |> Routeguide.RouteGuide.Stub.get_feature(point)
    IO.inspect(reply)
  end

  def print_features(channel, rect) do
    IO.puts("Looking for features within #{inspect(rect)}")
    {:ok, stream} = channel |> Routeguide.RouteGuide.Stub.list_features(rect)

    Enum.each(stream, fn {:ok, feature} ->
      IO.inspect(feature)
    end)
  end

  def run_record_route(channel) do
    ts = :os.timestamp()
    seed = :rand.seed(:exs64, ts)
    {count, seed} = :rand.uniform_s(seed)
    count = trunc(count * 100 + 2)

    {points, _seed} =
      Enum.reduce(1..count, {[], seed}, fn _, {acc, seed} ->
        {point, seed} = random_point(seed)
        {[point | acc], seed}
      end)

    IO.puts("Traversing #{length(points)} points.")
    stream = channel |> Routeguide.RouteGuide.Stub.record_route()

    Enum.reduce(points, points, fn _, [point | tail] ->
      opts = if length(tail) == 0, do: [end_stream: true], else: []
      GRPC.Stub.send_request(stream, point, opts)
      tail
    end)

    res = GRPC.Stub.recv(stream)
    IO.puts("Route summary: #{inspect(res)}")
  end

  def run_route_chat(channel) do
    data = [
      %{lat: 0, long: 1, msg: "First message"},
      %{lat: 0, long: 2, msg: "Second message"},
      %{lat: 0, long: 3, msg: "Third message"},
      %{lat: 0, long: 1, msg: "Fourth message"},
      %{lat: 0, long: 2, msg: "Fifth message"},
      %{lat: 0, long: 3, msg: "Sixth message"}
    ]

    stream = channel |> Routeguide.RouteGuide.Stub.route_chat()

    notes =
      Enum.map(data, fn %{lat: lat, long: long, msg: msg} ->
        point = Routeguide.Point.new(latitude: lat, longitude: long)
        Routeguide.RouteNote.new(location: point, message: msg)
      end)

    task =
      Task.async(fn ->
        Enum.reduce(notes, notes, fn _, [note | tail] ->
          opts = if length(tail) == 0, do: [end_stream: true], else: []
          GRPC.Stub.send_request(stream, note, opts)
          tail
        end)
      end)

    {:ok, result_enum} = GRPC.Stub.recv(stream)
    Task.await(task)

    Enum.each(result_enum, fn {:ok, note} ->
      IO.puts(
        "Got message #{note.message} at point(#{note.location.latitude}, #{note.location.longitude})"
      )
    end)
  end

  defp random_point(seed) do
    {lat, seed} = :rand.uniform_s(seed)
    {long, seed} = :rand.uniform_s(seed)
    lat = trunc((trunc(lat * 180) - 90) * 1.0e7)
    long = trunc((trunc(long * 360) - 180) * 1.0e7)
    {Routeguide.Point.new(latitude: lat, longitude: long), seed}
  end
end
