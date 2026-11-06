defmodule GRPC.Client.Adapters.NodelayTest do
  # TCP_NODELAY stops Nagle's algorithm from holding a small write until the peer
  # ACKs the previous one, which adds tens of milliseconds to sequential calls.
  # See https://github.com/elixir-grpc/grpc/issues/323
  use GRPC.Client.DataCase, async: false

  alias GRPC.Client.Adapters.Gun
  alias GRPC.Client.Adapters.Mint

  defmodule PlainEndpoint do
    use GRPC.Endpoint
    run(FeatureServer)
  end

  defmodule TLSEndpoint do
    use GRPC.Endpoint
    run(FeatureServer)
  end

  setup do
    {:ok, _, port} = GRPC.Server.start_endpoint(PlainEndpoint, 0)

    {:ok, _, tls_port} =
      GRPC.Server.start_endpoint(TLSEndpoint, 0, adapter_opts: [cred: build(:credential)])

    on_exit(fn ->
      :ok = GRPC.Server.stop_endpoint(PlainEndpoint)
      :ok = GRPC.Server.stop_endpoint(TLSEndpoint)
    end)

    %{ports: %{"http" => port, "https" => tls_port}}
  end

  defp channel(adapter, scheme, ports) do
    build(:channel, adapter: adapter, scheme: scheme, port: ports[scheme], host: "127.0.0.1")
  end

  defp connect(adapter, channel, opts) do
    {:ok, connected} = adapter.connect(channel, opts)
    # The connection process is linked to the test and may be gone by now
    on_exit(fn ->
      try do
        adapter.disconnect(connected)
      catch
        :exit, _ -> :ok
      end
    end)

    connected
  end

  defp getopts(socket, :tcp), do: :inet.getopts(socket, [:nodelay])
  defp getopts(socket, :tls), do: :ssl.getopts(socket, [:nodelay])

  defp mint_nodelay(%{adapter_payload: %{conn_pid: pid}}, transport) do
    %{conn: conn} = :sys.get_state(pid)
    {:ok, [nodelay: nodelay]} = getopts(Elixir.Mint.HTTP.get_socket(conn), transport)
    nodelay
  end

  defp gun_nodelay(%{adapter_payload: %{conn_pid: pid}}, transport) do
    %{gun_pid: gun_pid} = :sys.get_state(pid)
    %{socket: socket} = :gun.info(gun_pid)
    {:ok, [nodelay: nodelay]} = getopts(socket, transport)
    nodelay
  end

  describe "Mint client" do
    test "has nodelay on by default over tcp", %{ports: ports} do
      connected = connect(Mint, channel(Mint, "http", ports), [])

      assert mint_nodelay(connected, :tcp) == true
    end

    test "has nodelay on by default over tls", %{ports: ports} do
      connected = connect(Mint, channel(Mint, "https", ports), [])

      assert mint_nodelay(connected, :tls) == true
    end

    test "lets transport_opts turn it off over tcp", %{ports: ports} do
      connected =
        connect(Mint, channel(Mint, "http", ports), transport_opts: [nodelay: false])

      assert mint_nodelay(connected, :tcp) == false
    end

    test "lets transport_opts turn it off over tls", %{ports: ports} do
      connected =
        connect(Mint, channel(Mint, "https", ports), transport_opts: [nodelay: false])

      assert mint_nodelay(connected, :tls) == false
    end
  end

  describe "Gun client" do
    test "has nodelay on by default over tcp", %{ports: ports} do
      connected = connect(Gun, channel(Gun, "http", ports), [])

      assert gun_nodelay(connected, :tcp) == true
    end

    test "has nodelay on by default over tls", %{ports: ports} do
      connected = connect(Gun, channel(Gun, "https", ports), [])

      assert gun_nodelay(connected, :tls) == true
    end

    test "lets transport_opts turn it off over tcp", %{ports: ports} do
      connected =
        connect(Gun, channel(Gun, "http", ports), transport_opts: [nodelay: false])

      assert gun_nodelay(connected, :tcp) == false
    end
  end
end
