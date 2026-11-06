defmodule GRPC.Server.Adapters.CowboyNodelayTest do
  # Ranch turns TCP_NODELAY on by default, and the adapter passes no option that
  # would turn it off. This test fails if either of those changes.
  # See https://github.com/elixir-grpc/grpc/issues/323
  use ExUnit.Case, async: false

  alias GRPC.Credential

  defmodule PlainEndpoint do
    use GRPC.Endpoint
    run(FeatureServer)
  end

  defmodule TLSEndpoint do
    use GRPC.Endpoint
    run(FeatureServer)
  end

  setup do
    cred = %Credential{
      ssl: [
        certfile: Path.expand("../../../../priv/tls/server1.pem", __DIR__),
        keyfile: Path.expand("../../../../priv/tls/server1.key", __DIR__)
      ]
    }

    {:ok, _, port} = GRPC.Server.start_endpoint(PlainEndpoint, 0)
    {:ok, _, tls_port} = GRPC.Server.start_endpoint(TLSEndpoint, 0, adapter_opts: [cred: cred])

    on_exit(fn ->
      :ok = GRPC.Server.stop_endpoint(PlainEndpoint)
      :ok = GRPC.Server.stop_endpoint(TLSEndpoint)
    end)

    %{port: port, tls_port: tls_port}
  end

  # Cowboy's connection process holds the accepted socket in its state record.
  # Over TLS, the state is `{record, extra}`, so unwrap it first.
  defp accepted_socket(endpoint, matcher) do
    [conn_pid] = :ranch.procs(inspect(endpoint), :connections)

    state =
      case :sys.get_state(conn_pid) do
        {record, _extra} when is_tuple(record) -> record
        record -> record
      end

    state |> Tuple.to_list() |> Enum.find(matcher)
  end

  test "accepted tcp sockets have nodelay on", %{port: port} do
    {:ok, client} = :gen_tcp.connect(~c"127.0.0.1", port, active: false)

    socket = wait_for_socket(PlainEndpoint, &is_port/1)

    assert {:ok, [nodelay: true]} = :inet.getopts(socket, [:nodelay])
    :gen_tcp.close(client)
  end

  test "accepted tls sockets have nodelay on", %{tls_port: port} do
    {:ok, client} =
      :ssl.connect(~c"127.0.0.1", port,
        active: false,
        verify: :verify_none,
        alpn_advertised_protocols: ["h2"]
      )

    socket = wait_for_socket(TLSEndpoint, &ssl_socket?/1)

    assert {:ok, [nodelay: true]} = :ssl.getopts(socket, [:nodelay])
    :ssl.close(client)
  end

  # The tuple size of an :sslsocket differs between OTP versions
  defp ssl_socket?(term),
    do: is_tuple(term) and tuple_size(term) > 0 and elem(term, 0) == :sslsocket

  defp wait_for_socket(endpoint, matcher, attempts \\ 100) do
    with [_] <- :ranch.procs(inspect(endpoint), :connections),
         socket when not is_nil(socket) <- accepted_socket(endpoint, matcher) do
      socket
    else
      _ when attempts > 0 ->
        Process.sleep(10)
        wait_for_socket(endpoint, matcher, attempts - 1)

      _ ->
        flunk("no accepted socket for #{inspect(endpoint)}")
    end
  end
end
