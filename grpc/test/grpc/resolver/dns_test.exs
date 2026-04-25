defmodule GRPC.Client.Resolver.DNSTest do
  use ExUnit.Case, async: true
  import Mox
  import ExUnit.CaptureLog

  alias GRPC.Client.Resolver.DNS

  setup :verify_on_exit!

  setup do
    Mox.set_mox_global()
    :ok
  end

  test "resolves A record and parses service config from TXT via GenServer" do
    host = "my-service.local"
    config_name = "_grpc_config." <> host

    DNS.MockAdapter
    |> expect(:lookup, fn ^host, :a ->
      {:ok, [{127, 0, 0, 1}]}
    end)
    |> expect(:lookup, fn ^config_name, :txt ->
      {:ok,
       [
         ~c'grpc_config={
           "loadBalancingConfig":[{"round_robin":{}}],
           "methodConfig":[
             {
               "name":[
                 {"service":"foo","method":"bar"},
                 {"service":"baz"}
               ],
               "timeout":"1.000000001s"
             }
           ]
         }'
       ]}
    end)

    assert {:ok, %{addresses: addrs, service_config: config}} = DNS.resolve(host)
    assert [%{address: "127.0.0.1", port: 50051}] = addrs
    assert config.load_balancing_policy == :round_robin

    method_names =
      Enum.flat_map(config.method_configs, fn mc ->
        Enum.map(mc["name"], fn n -> {n["service"], Map.get(n, "method")} end)
      end)

    assert {"foo", "bar"} in method_names
    assert {"baz", nil} in method_names
  end

  test "resolves AAAA record as fallback when A record returns empty list" do
    host = "my-service.local"
    config_name = "_grpc_config." <> host

    DNS.MockAdapter
    |> expect(:lookup, fn ^host, :a ->
      {:ok, []}
    end)
    |> expect(:lookup, fn ^host, :aaaa ->
      {:ok, [{0, 0, 0, 0, 0, 0, 0, 1}]}
    end)
    |> expect(:lookup, fn ^config_name, :txt ->
      {:ok, []}
    end)

    assert {:ok, %{addresses: addrs}} = DNS.resolve(host)
    assert [%{address: "::1", port: 50051}] = addrs
  end

  test "handles multiple identical grpc_config records silently" do
    host = "my-service.local"
    config_name = "_grpc_config." <> host

    DNS.MockAdapter
    |> expect(:lookup, fn ^host, :a ->
      {:ok, [{127, 0, 0, 1}]}
    end)
    |> expect(:lookup, fn ^config_name, :txt ->
      {:ok,
       [
         ~c'grpc_config={"loadBalancingConfig":[{"round_robin":{}}]}',
         ~c'grpc_config={"loadBalancingConfig":[{"round_robin":{}}]}'
       ]}
    end)

    log =
      capture_log(fn ->
        assert {:ok, %{addresses: addrs, service_config: config}} = DNS.resolve(host)
        assert [%{address: "127.0.0.1", port: 50051}] = addrs
        assert config.load_balancing_policy == :round_robin
      end)

    assert log == ""
  end

  test "logs warning on multiple conflicting grpc_config records" do
    host = "my-service.local"
    config_name = "_grpc_config." <> host

    DNS.MockAdapter
    |> expect(:lookup, fn ^host, :a ->
      {:ok, [{127, 0, 0, 1}]}
    end)
    |> expect(:lookup, fn ^config_name, :txt ->
      {:ok,
       [
         ~c'grpc_config={"loadBalancingConfig":[{"round_robin":{}}]}',
         ~c'grpc_config={"loadBalancingConfig":[{"least_request":{}}]}'
       ]}
    end)

    log =
      capture_log(fn ->
        assert {:ok, %{addresses: _addrs, service_config: config}} = DNS.resolve(host)
        assert config.load_balancing_policy == :round_robin
      end)

    assert log =~ "DNS: found multiple conflicting grpc_config records, using first"
  end

  test "trims whitespace from grpc_config values" do
    host = "my-service.local"
    config_name = "_grpc_config." <> host

    DNS.MockAdapter
    |> expect(:lookup, fn ^host, :a ->
      {:ok, [{127, 0, 0, 1}]}
    end)
    |> expect(:lookup, fn ^config_name, :txt ->
      {:ok,
       [
         ~c'grpc_config=  {"loadBalancingConfig":[{"round_robin":{}}]}'
       ]}
    end)

    assert {:ok, %{addresses: _addrs, service_config: config}} = DNS.resolve(host)
    assert config.load_balancing_policy == :round_robin
  end

  test "falls back to nil when no valid grpc_config records found" do
    host = "my-service.local"
    config_name = "_grpc_config." <> host

    DNS.MockAdapter
    |> expect(:lookup, fn ^host, :a ->
      {:ok, [{127, 0, 0, 1}]}
    end)
    |> expect(:lookup, fn ^config_name, :txt ->
      {:ok,
       [
         ~c'other_config=value'
       ]}
    end)

    assert {:ok, %{addresses: addrs, service_config: config}} = DNS.resolve(host)
    assert [%{address: "127.0.0.1", port: 50051}] = addrs
    assert {:ok, %{load_balancing_policy: :pick_first}} = config
  end
end
