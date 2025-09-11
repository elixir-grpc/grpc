defmodule GRPC.Client.Resolver.DNS do
  @behaviour GRPC.Client.Resolver

  alias GRPC.Client.ServiceConfig

  @impl GRPC.Client.Resolver
  def resolve(target) do
    uri = URI.parse(target)
    host = uri.host || target
    port = uri.port || 50051

    # resolve A/AAAA
    {:ok, addresses} = :inet_res.lookup(String.to_charlist(host), :in, :a)

    addrs =
      Enum.map(addresses, fn ip ->
        %{address: :inet.ntoa(ip) |> to_string(), port: port}
      end)

    # tries to resolve TXT to service config
    service_config_json =
      case :inet_res.lookup(~c"_grpc_config." ++ String.to_charlist(host), :in, :txt) do
        [txt | _] ->
          str = List.to_string(txt)

          # TXT may have prefix "grpc_config="
          case String.split(str, "grpc_config=") do
            [_, json] -> json
            _ -> nil
          end

        _ ->
          nil
      end

    {:ok, %{addresses: addrs, service_config: ServiceConfig.parse(service_config_json)}}
  end
end
