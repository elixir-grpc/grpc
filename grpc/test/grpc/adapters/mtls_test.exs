defmodule GRPC.Client.Adapters.MTLSTest do
  # Runs the configuration shown in guides/advanced/tls.md against a server that
  # requires client certificates. See https://github.com/elixir-grpc/grpc/issues/393
  use GRPC.Client.DataCase, async: false

  alias GRPC.Client.Adapters.Gun
  alias GRPC.Client.Adapters.Mint

  @moduletag :tmp_dir

  defmodule Endpoint do
    use GRPC.Endpoint
    run(FeatureServer)
  end

  setup %{tmp_dir: tmp_dir} do
    certs = write_certs(tmp_dir)

    server_cred =
      GRPC.Credential.new(
        ssl: [
          certfile: certs.server_cert,
          keyfile: certs.server_key,
          cacertfile: certs.client_ca,
          verify: :verify_peer,
          fail_if_no_peer_cert: true
        ]
      )

    {:ok, _, port} = GRPC.Server.start_endpoint(Endpoint, 0, adapter_opts: [cred: server_cred])
    on_exit(fn -> :ok = GRPC.Server.stop_endpoint(Endpoint) end)

    %{port: port, certs: certs}
  end

  # The options below are the ones the guide shows.
  defp mutual_cred(certs) do
    GRPC.Credential.new(
      ssl: [
        certfile: certs.client_cert,
        keyfile: certs.client_key,
        cacertfile: certs.server_ca,
        verify: :verify_peer,
        server_name_indication: ~c"localhost"
      ]
    )
  end

  defp server_only_cred(certs) do
    GRPC.Credential.new(
      ssl: [
        cacertfile: certs.server_ca,
        verify: :verify_peer,
        server_name_indication: ~c"localhost"
      ]
    )
  end

  defp get_feature(channel, opts \\ []) do
    point = %Routeguide.Point{latitude: 409_146_138, longitude: -746_188_906}
    Routeguide.RouteGuide.Stub.get_feature(channel, point, opts)
  end

  # A connection the server drops can fail a call in more than one way: an error
  # tuple, or an exit from the connection pool. Both mean the call did not go through.
  defp failed_call(channel) do
    case get_feature(channel, timeout: 2_000) do
      {:error, _reason} -> :failed
      {:ok, _feature} -> :succeeded
    end
  catch
    :exit, _reason -> :failed
  end

  for {name, adapter} <- [gun: Gun, mint: Mint] do
    describe "#{name} adapter" do
      test "makes a call with a client certificate", %{port: port, certs: certs} do
        {:ok, channel} =
          GRPC.Stub.connect("https://localhost:#{port}",
            adapter: unquote(adapter),
            cred: mutual_cred(certs)
          )

        on_exit(fn -> GRPC.Stub.disconnect(channel) end)

        assert {:ok, %Routeguide.Feature{name: "409146138,-746188906"}} = get_feature(channel)
      end

      test "a bare host:port with :cred also uses TLS", %{port: port, certs: certs} do
        {:ok, channel} =
          GRPC.Stub.connect("localhost:#{port}",
            adapter: unquote(adapter),
            cred: mutual_cred(certs)
          )

        on_exit(fn -> GRPC.Stub.disconnect(channel) end)

        assert {:ok, %Routeguide.Feature{}} = get_feature(channel)
      end

      test "server_name_indication is optional when dialing a host name", %{
        port: port,
        certs: certs
      } do
        cred = update_in(mutual_cred(certs).ssl, &Keyword.delete(&1, :server_name_indication))

        {:ok, channel} =
          GRPC.Stub.connect("https://localhost:#{port}", adapter: unquote(adapter), cred: cred)

        on_exit(fn -> GRPC.Stub.disconnect(channel) end)

        assert {:ok, %Routeguide.Feature{}} = get_feature(channel)
      end

      test "dialing an IP address works with server_name_indication", %{port: port, certs: certs} do
        {:ok, channel} =
          GRPC.Stub.connect("https://127.0.0.1:#{port}",
            adapter: unquote(adapter),
            cred: mutual_cred(certs)
          )

        on_exit(fn -> GRPC.Stub.disconnect(channel) end)

        assert {:ok, %Routeguide.Feature{}} = get_feature(channel)
      end

      test "a client without a certificate is rejected", %{port: port, certs: certs} do
        {:ok, channel} =
          GRPC.Stub.connect("https://localhost:#{port}",
            adapter: unquote(adapter),
            cred: server_only_cred(certs)
          )

        on_exit(fn -> GRPC.Stub.disconnect(channel) end)

        assert failed_call(channel) == :failed
      end

      test "a bare host:port without :cred connects, then calls fail", %{port: port} do
        # The client speaks plaintext to a TLS server. Connecting is lazy, so
        # this returns {:ok, channel} and the failure shows up on the first call.
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter: unquote(adapter))
        on_exit(fn -> GRPC.Stub.disconnect(channel) end)

        assert failed_call(channel) == :failed
      end
    end
  end

  # Builds a CA and a leaf for the server, and another pair for the client, and
  # writes them as PEM files like the ones a real deployment has.
  defp write_certs(dir) do
    # The server cert needs a subject alternative name for `localhost`, or
    # `verify: :verify_peer` rejects it.
    san = {:Extension, {2, 5, 29, 17}, false, [dNSName: ~c"localhost"]}

    %{server_config: server, client_config: client} =
      :public_key.pkix_test_data(%{
        server_chain: %{
          root: [key: {:rsa, 2048, 17}],
          peer: [key: {:rsa, 2048, 17}, extensions: [san]]
        },
        client_chain: %{root: [key: {:rsa, 2048, 17}], peer: [key: {:rsa, 2048, 17}]}
      })

    files = %{
      server_cert: write_cert(dir, "server.pem", server[:cert]),
      server_key: write_key(dir, "server.key", server[:key]),
      # The CA that signed the client's cert, which the server trusts.
      client_ca: write_cert(dir, "client_ca.pem", hd(server[:cacerts])),
      client_cert: write_cert(dir, "client.pem", client[:cert]),
      client_key: write_key(dir, "client.key", client[:key]),
      # The CA that signed the server's cert, which the client trusts.
      server_ca: write_cert(dir, "server_ca.pem", hd(client[:cacerts]))
    }

    files
  end

  defp write_cert(dir, name, der) do
    path = Path.join(dir, name)
    File.write!(path, :public_key.pem_encode([{:Certificate, der, :not_encrypted}]))
    path
  end

  defp write_key(dir, name, {type, der}) do
    path = Path.join(dir, name)
    File.write!(path, :public_key.pem_encode([{type, der, :not_encrypted}]))
    path
  end
end
