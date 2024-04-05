defmodule GRPC.ChannelTest do
  use ExUnit.Case
  alias GRPC.Test.ClientAdapter
  alias GRPC.Channel

  for {kind, addr} <- [{"ip", "10.0.0.1"}, {"hostname", "example.com"}] do
    describe "connect/2 with http and #{kind}" do
      test "works" do
        {:ok, channel} =
          GRPC.Stub.connect("http://#{unquote(addr)}:50051", adapter: ClientAdapter)

        assert %Channel{host: unquote(addr), port: 50051, scheme: "http", cred: nil} = channel
      end

      test "errors if credential is provided" do
        cred = %GRPC.Credential{ssl: []}

        assert_raise ArgumentError, "invalid option for insecure (http) address: :cred", fn ->
          GRPC.Stub.connect("http://#{unquote(addr)}:50051", adapter: ClientAdapter, cred: cred)
        end
      end
    end

    describe "connect/2 with https and #{kind}" do
      test "sets default credential" do
        {:ok, channel} =
          GRPC.Stub.connect("https://#{unquote(addr)}:50051", adapter: ClientAdapter)

        assert %Channel{host: unquote(addr), port: 50051, scheme: "https", cred: cred} = channel

        assert Keyword.has_key?(cred.ssl, :verify)
        assert Keyword.has_key?(cred.ssl, :depth)
        assert Keyword.has_key?(cred.ssl, :cacert_file)
      end

      test "allows overriding default credentials" do
        cred = %GRPC.Credential{ssl: []}

        {:ok, channel} =
          GRPC.Stub.connect("https://#{unquote(addr)}:50051", adapter: ClientAdapter, cred: cred)

        assert %Channel{host: unquote(addr), port: 50051, scheme: "https", cred: ^cred} = channel
      end
    end

    describe "connect/2 with no scheme, #{kind} and" do
      test "no cred uses http" do
        {:ok, channel} = GRPC.Stub.connect("#{unquote(addr)}:50051", adapter: ClientAdapter)
        assert %Channel{host: unquote(addr), port: 50051, scheme: "http", cred: nil} = channel
      end

      test "cred uses https" do
        cred = %{ssl: []}

        {:ok, channel} =
          GRPC.Stub.connect("#{unquote(addr)}:50051", adapter: ClientAdapter, cred: cred)

        assert %Channel{host: unquote(addr), port: 50051, scheme: "https", cred: ^cred} = channel
      end
    end
  end

  test "connect/2 allows setting default headers" do
    headers = [{"authorization", "Bearer TOKEN"}]

    {:ok, channel} =
      GRPC.Stub.connect("http://10.0.0.1:50051", adapter: ClientAdapter, headers: headers)

    assert %Channel{host: "10.0.0.1", port: 50051, headers: ^headers} = channel
  end
end
