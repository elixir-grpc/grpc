defmodule GRPC.Client.Resolver.IPv6 do
  @behaviour GRPC.Client.Resolver

  @impl GRPC.Client.Resolver
  def resolve(target) do
    # target example: "ipv6:[2607:f8b0:400e:c00::ef]:443,[::1]:50051"
    [_scheme, addrs_str] = String.split(target, ":", parts: 2)

    addresses =
      String.split(addrs_str, ",")
      |> Enum.map(fn entry ->
        [ip, port] =
          case Regex.run(~r/\[(.*?)\]:(\d+)/, entry) do
            [_, ip, port] -> [ip, port]
            _ -> [entry, "443"]
          end

        %{address: ip, port: String.to_integer(port)}
      end)

    {:ok, %{addresses: addresses, service_config: nil}}
  end
end
