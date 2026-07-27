defmodule GRPC.Integration.ConnectionTest do
  use GRPC.Integration.TestCase

  # Gun's adapter installs its own retry_fun (≈1s base); override for a fast reconnect.
  def fast_retry_fun(retries, _opts), do: %{retries: retries - 1, timeout: 10}

  test "reconnection works" do
    server = FeatureServer
    {:ok, _, port} = GRPC.Server.start(server, 0)
    point = %Routeguide.Point{latitude: 409_146_138, longitude: -746_188_906}

    {:ok, channel} =
      GRPC.Stub.connect("localhost:#{port}",
        adapter_opts: [retry_fun: &__MODULE__.fast_retry_fun/2]
      )

    assert {:ok, _} = channel |> Routeguide.RouteGuide.Stub.get_feature(point)
    :ok = GRPC.Server.stop(server)
    {:ok, _, _} = reconnect_server(server, port)
    assert {:ok, _} = channel |> Routeguide.RouteGuide.Stub.get_feature(point)
    :ok = GRPC.Server.stop(server)
  end

  test "connecting with a domain socket works" do
    socket_path = "/tmp/grpc.sock"
    server = FeatureServer
    File.rm(socket_path)

    {:ok, _, _} = GRPC.Server.start(server, 0, adapter_opts: [ip: {:local, socket_path}])

    {:ok, channel} =
      GRPC.Stub.connect(socket_path, adapter_opts: [retry_fun: &__MODULE__.fast_retry_fun/2])

    point = %Routeguide.Point{latitude: 409_146_138, longitude: -746_188_906}
    assert {:ok, _} = channel |> Routeguide.RouteGuide.Stub.get_feature(point)
    :ok = GRPC.Server.stop(server)
  end

  test "authentication works" do
    server = FeatureServer

    cred = GRPC.Factory.build(:credential, verify: :verify_peer)

    {:ok, _, port} = GRPC.Server.start(server, 0, adapter_opts: [cred: cred])

    try do
      point = %Routeguide.Point{latitude: 409_146_138, longitude: -746_188_906}

      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", cred: cred)
      assert {:ok, _} = Routeguide.RouteGuide.Stub.get_feature(channel, point)
    catch
      error ->
        refute "Caught #{inspect(error)}"
    after
      :ok = GRPC.Server.stop(server)
    end
  end

  test "disconnecting a channel works" do
    server = FeatureServer
    {:ok, _, port} = GRPC.Server.start(server, 0)
    {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

    assert {:ok, _} =
             Routeguide.RouteGuide.Stub.get_feature(channel, %Routeguide.Point{
               latitude: 409_146_138,
               longitude: -746_188_906
             })

    assert {:ok, _} = GRPC.Stub.disconnect(channel)
    :ok = GRPC.Server.stop(server)
  end
end
