defmodule GRPC.Client.Adapters.GunAbandonedStreamTest do
  # A server stream is a lazy enumerable. A caller that stops enumerating one
  # never resets it, so its response process keeps the rest of the reply queued
  # until the connection closes.
  use GRPC.Client.DataCase, async: true

  alias GRPC.Client.Adapters.Gun
  alias Routeguide.RouteGuide.Stub

  @messages 200

  defmodule Server do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def get_feature(_point, _stream), do: %Routeguide.Feature{name: "ok"}

    def list_features(%{hi: %{latitude: count}}, stream) do
      Enum.each(1..count, fn index ->
        GRPC.Server.send_reply(stream, %Routeguide.Feature{name: "feature-#{index}"})
      end)
    end
  end

  setup do
    {:ok, _, port} = GRPC.Server.start(Server, 0)
    on_exit(fn -> GRPC.Server.stop(Server) end)
    %{port: port}
  end

  test "a stream the caller stops reading is released", %{port: port} do
    channel = build(:channel, port: port, host: "localhost")
    {:ok, channel} = Gun.connect(channel, [])
    on_exit(fn -> Gun.disconnect(channel) end)

    {:ok, replies} = Stub.list_features(channel, rectangle(@messages), timeout: 30_000)

    assert [{:ok, %Routeguide.Feature{}}] = Enum.take(replies, 1)

    assert :sys.get_state(channel.adapter_payload.conn_pid).response_processes == %{}
  end

  defp rectangle(count) do
    %Routeguide.Rectangle{
      lo: %Routeguide.Point{latitude: 1, longitude: 1},
      hi: %Routeguide.Point{latitude: count, longitude: 2}
    }
  end
end
