defmodule GRPC.Integration.IPv6Test do
  # Changes the global Erlang resolver settings, so it can't run async.
  use GRPC.Integration.TestCase, async: false

  @moduletag :tmp_dir

  unless GRPC.Test.IPv6.loopback_available?() do
    @moduletag skip: "no IPv6 loopback on this machine"
  end

  @point %Routeguide.Point{latitude: 409_146_138, longitude: -746_188_906}

  setup %{tmp_dir: tmp_dir} do
    host = GRPC.Test.IPv6.aaaa_only_host(tmp_dir)

    # What `dns://` finds for the same name: an AAAA record and no A record.
    Mox.stub(GRPC.Client.Resolver.DNS.MockAdapter, :lookup, fn
      ^host, :a -> {:ok, []}
      ^host, :aaaa -> {:ok, [GRPC.Test.IPv6.loopback()]}
      _name, :txt -> []
    end)

    {:ok, _pid, port} =
      GRPC.Server.start(FeatureServer, 0,
        adapter_opts: [net: :inet6, ip: GRPC.Test.IPv6.loopback()]
      )

    on_exit(fn -> GRPC.Server.stop(FeatureServer) end)

    %{host: host, port: port}
  end

  for adapter <- [GRPC.Client.Adapters.Gun, GRPC.Client.Adapters.Mint] do
    describe "#{inspect(adapter)}" do
      @describetag adapter: adapter

      test "connects to a bracketed IPv6 address", %{adapter: adapter, port: port} do
        assert_unary_call("[::1]:#{port}", adapter)
      end

      test "connects to an ipv6: target", %{adapter: adapter, port: port} do
        assert_unary_call("ipv6:[::1]:#{port}", adapter)
      end

      test "connects to an IPv6 address given as host and port", %{adapter: adapter, port: port} do
        # connect/3 is deprecated, but still takes a bare IPv6 host.
        {:ok, channel} = apply(GRPC.Stub, :connect, ["::1", port, [adapter: adapter]])
        assert {:ok, _} = Routeguide.RouteGuide.Stub.get_feature(channel, @point)
        {:ok, _} = GRPC.Stub.disconnect(channel)
      end

      test "connects to a name that has only AAAA records", %{
        adapter: adapter,
        host: host,
        port: port
      } do
        assert_unary_call("#{host}:#{port}", adapter)
      end

      test "connects to a dns:// name that has only AAAA records", %{
        adapter: adapter,
        host: host,
        port: port
      } do
        assert_unary_call("dns://#{host}:#{port}", adapter)
      end
    end
  end

  defp assert_unary_call(target, adapter) do
    {:ok, channel} = GRPC.Stub.connect(target, adapter: adapter)
    assert {:ok, _} = Routeguide.RouteGuide.Stub.get_feature(channel, @point)
    {:ok, _} = GRPC.Stub.disconnect(channel)
  end
end
