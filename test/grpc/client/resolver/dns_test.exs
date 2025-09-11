defmodule GRPC.Client.Resolver.DNSTest do
  use ExUnit.Case, async: true
  import Mox

  alias GRPC.Client.Resolver.DNS

  setup :verify_on_exit!

  setup do
    Mox.set_mox_global()
    {:ok, _pid} = start_supervised(DNS)
    :ok
  end

  test "resolves A record and parses service config from TXT via GenServer" do
    host = "my-service.local"
    host_charlist = String.to_charlist(host)

    DNS.MockAdapter
    |> expect(:lookup, fn ^host_charlist, :a ->
      {:ok, [{127, 0, 0, 1}]}
    end)
    |> expect(:lookup, fn ~c"_grpc_config." ++ ^host_charlist, :txt ->
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

  test "returns cached value on subsequent resolve" do
    host = "my-service.local"
    host_charlist = String.to_charlist(host)

    DNS.MockAdapter
    |> expect(:lookup, fn ^host_charlist, :a ->
      {:ok, [{127, 0, 0, 1}]}
    end)
    |> expect(:lookup, fn ~c"_grpc_config." ++ ^host_charlist, :txt ->
      {:ok,
       [
         ~c'grpc_config={
           "loadBalancingConfig":[{"pick_first":{}}],
           "methodConfig":[{"name":[{"service":"test"}]}]
         }'
       ]}
    end)

    {:ok, first} = DNS.resolve(host)
    {:ok, second} = DNS.resolve(host)

    assert first == second
  end

  test "refreshes DNS addresses and service config automatically" do
    host = "my-service.local"
    host_charlist = String.to_charlist(host)

    DNS.MockAdapter
    |> expect(:lookup, fn ^host_charlist, :a ->
      {:ok, [{127, 0, 0, 1}]}
    end)
    |> expect(:lookup, fn ~c"_grpc_config." ++ ^host_charlist, :txt ->
      {:ok,
       [
         ~c'grpc_config={"loadBalancingConfig":[{"pick_first":{}}],"methodConfig":[{"name":[{"service":"foo"}]}]}'
       ]}
    end)

    {:ok, %{addresses: addrs1, service_config: config1}} = DNS.resolve(host)

    assert [%{address: "127.0.0.1", port: 50051}] = addrs1
    assert config1.load_balancing_policy == :pick_first

    DNS.MockAdapter
    |> expect(:lookup, fn ^host_charlist, :a ->
      {:ok, [{10, 0, 0, 1}, {10, 0, 0, 2}]}
    end)
    |> expect(:lookup, fn ~c"_grpc_config." ++ ^host_charlist, :txt ->
      {:ok,
       [
         ~c'grpc_config={"loadBalancingConfig":[{"round_robin":{}}],"methodConfig":[{"name":[{"service":"bar"}]}]}'
       ]}
    end)

    # Force refresh immediately
    send(DNS, {:refresh, host})

    # Small delay to allow the GenServer to process
    :timer.sleep(50)

    {:ok, %{addresses: addrs2, service_config: config2}} = DNS.resolve(host)

    assert [%{address: "10.0.0.1", port: 50051}, %{address: "10.0.0.2", port: 50051}] = addrs2
    assert config2.load_balancing_policy == :round_robin
  end
end
