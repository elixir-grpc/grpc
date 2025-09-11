defmodule GRPC.Client.Resolver.IPv4 do
  @behaviour GRPC.Client.Resolver

  @impl GRPC.Client.Resolver
  def resolve(target) do
    # target exemplo: "ipv4:10.0.0.1:50051,10.0.0.2:50052"
    [_scheme, addrs_str] = String.split(target, ":", parts: 2)

    addresses =
      String.split(addrs_str, ",")
      |> Enum.map(fn addr ->
        [ip, port] = String.split(addr, ":")
        %{address: ip, port: String.to_integer(port)}
      end)

    {:ok, %{addresses: addresses, service_config: nil}}
  end
end
