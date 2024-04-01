defmodule GRPC.ChannelTest do
  use ExUnit.Case
  alias GRPC.Test.ClientAdapter
  alias GRPC.Channel

  describe "connect/2 with scheme" do
    test "works for insecure" do
      {:ok, channel} = GRPC.Stub.connect("http://10.1.0.0:50051", adapter: ClientAdapter)
      assert %Channel{host: "10.1.0.0", port: 50051, scheme: "http", cred: nil} = channel
    end

    test "default ssl credentials" do
      {:ok, channel} = GRPC.Stub.connect("https://10.1.0.0:50051", adapter: ClientAdapter)

      assert %Channel{host: "10.1.0.0", port: 50051, scheme: "https", cred: cred} = channel

      assert Keyword.has_key?(cred.ssl, :verify)
      assert Keyword.has_key?(cred.ssl, :depth)
      assert Keyword.has_key?(cred.ssl, :cacert_file)
    end

    test "allows overriding ssl credentials" do
      cred = %GRPC.Credential{ssl: []}

      {:ok, channel} =
        GRPC.Stub.connect("https://10.1.0.0:50051", adapter: ClientAdapter, cred: cred)

      assert %Channel{host: "10.1.0.0", port: 50051, scheme: "https", cred: ^cred} = channel
    end
  end

  describe "connect/2 without scheme" do
    test "works for insecure" do
      {:ok, channel} = GRPC.Stub.connect("10.1.0.0:50051", adapter: ClientAdapter)
      assert %Channel{host: "10.1.0.0", port: 50051, scheme: "http", cred: nil} = channel
    end

    test "works for ssl" do
      cred = %{ssl: []}
      {:ok, channel} = GRPC.Stub.connect("10.1.0.0:50051", adapter: ClientAdapter, cred: cred)
      assert %Channel{host: "10.1.0.0", port: 50051, scheme: "https", cred: ^cred} = channel
    end
  end

  test "connect/2 allows setting default headers" do
    headers = [{"authorization", "Bearer TOKEN"}]

    {:ok, channel} =
      GRPC.Stub.connect("http://10.1.0.0:50051", adapter: ClientAdapter, headers: headers)

    assert %Channel{host: "10.1.0.0", port: 50051, headers: ^headers} = channel
  end
end
