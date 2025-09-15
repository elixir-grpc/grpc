defmodule GRPC.Client.Resolver.DNSTest do
  use ExUnit.Case, async: true
  import Mox

  alias GRPC.Client.Resolver.DNS

  setup :verify_on_exit!

  setup do
    Mox.set_mox_global()
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
end
