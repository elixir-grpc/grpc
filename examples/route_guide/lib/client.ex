defmodule RouteGuide.Client do
  defmodule Foo do
    def call(service_mod, rpc, path, channel, request, opts, next) do
      IO.puts("Foo.call")
      next.(service_mod, rpc, path, channel, request, opts)
    end

    def stream_send(stream, request, opts, next) do
      IO.puts("Foo.stream_send")
      next.(stream, request, opts)
    end

    def recv(stream, opts, next) do
      IO.puts("Foo.recv")
      next.(stream, opts)
    end
  end

  @middlewares [Foo]

  def main(channel) do
    print_feature(channel, Routeguide.Point.new(latitude: 409146138, longitude: -746188906))
    print_feature(channel, Routeguide.Point.new(latitude: 0, longitude: 0))

    # Looking for features between 40, -75 and 42, -73.
    print_features(channel, Routeguide.Rectangle.new(
      lo: Routeguide.Point.new(latitude: 400000000, longitude: -750000000),
      hi: Routeguide.Point.new(latitude: 420000000, longitude: -730000000)
    ))

    run_record_route(channel)

    run_route_chat(channel)
  end

  def print_feature(channel, point) do
    IO.puts "Getting feature for point (#{point.latitude}, #{point.longitude})"
    {:ok, reply} = channel |> Routeguide.RouteGuide.Stub.get_feature(point, middlewares: @middlewares)
    IO.inspect reply
  end

  def print_features(channel, rect) do
    IO.puts "Looking for features within #{inspect rect}"
    stream = channel |> Routeguide.RouteGuide.Stub.list_features(rect, middlewares: @middlewares)
    Enum.each stream, fn (feature)->
      IO.inspect feature
    end
  end

  def run_record_route(channel) do
    ts = :os.timestamp
    seed = :rand.seed(:exs64, ts)
    {count, seed} = :rand.uniform_s(seed)
    count = trunc((count * 100) + 2)
    {points, _seed} = Enum.reduce 1..count, {[], seed}, fn (_, {acc, seed}) ->
      {point, seed} = random_point(seed)
      {[point|acc], seed}
    end
    IO.puts "Traversing #{length(points)} points."
    stream = channel |> Routeguide.RouteGuide.Stub.record_route(middlewares: @middlewares)
    Enum.reduce points, points, fn (_, [point|tail]) ->
      opts = if length(tail) == 0, do: [end_stream: true], else: []
      Routeguide.RouteGuide.Stub.stream_send(stream, point, Keyword.put_new(opts, :middlewares, @middlewares))
      tail
    end
    res = Routeguide.RouteGuide.Stub.recv(stream, middlewares: @middlewares)
    IO.puts "Route summary: #{inspect res}"
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
    stream = channel |> Routeguide.RouteGuide.Stub.route_chat
    notes = Enum.map data, fn (%{lat: lat, long: long, msg: msg}) ->
      point = Routeguide.Point.new(latitude: lat, longitude: long)
      Routeguide.RouteNote.new(location: point, message: msg)
    end
    task = Task.async(fn ->
      Enum.reduce(notes, notes, fn (_, [note|tail]) ->
        opts = if length(tail) == 0, do: [end_stream: true], else: []
        Routeguide.RouteGuide.Stub.stream_send(stream, note, Keyword.put_new(opts, :middlewares, @middlewares))
        tail
      end)
    end)
    result_enum = Routeguide.RouteGuide.Stub.recv(stream, middlewares: @middlewares)
    Task.await(task)
    Enum.each result_enum, fn (note) ->
      IO.puts "Got message #{note.message} at point(#{note.location.latitude}, #{note.location.longitude})"
    end
  end

  defp random_point(seed) do
    {lat, seed} = :rand.uniform_s(seed)
    {long, seed} = :rand.uniform_s(seed)
    lat = trunc((trunc(lat * 180) - 90) * 1.0e7)
    long = trunc((trunc(long * 360) - 180) * 1.0e7)
    {Routeguide.Point.new(latitude: lat, longitude: long), seed}
  end
end
