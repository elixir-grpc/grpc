defmodule GRPC.Client.Resolver.IPv6 do
  @moduledoc """
  Resolver for gRPC clients connecting to one or more IPv6 addresses.

  This resolver handles target strings using the `ipv6` URI scheme, which
  allows specifying one or multiple IPv6 addresses with optional ports.

  ## Target format

      ipv6:[addr][:port][,[addr][:port],...]

  - IPv6 addresses **must** be enclosed in square brackets (`[...]`).
  - The port is optional; if not provided, the default port is `443`.
  - Multiple addresses can be comma-separated.
  - `service_config` is always `nil` as IPv6 literals do not support DNS TXT or xDS.
  """

  @behaviour GRPC.Client.Resolver

  @default_port 443

  @impl GRPC.Client.Resolver
  def resolve(target) do
    uri = URI.parse(target)
    addresses_str = uri.path || ""

    with {:ok, addresses} <- parse_entries(addresses_str) do
      {:ok, %{addresses: addresses, service_config: nil}}
    end
  end

  ## Helpers

  defp parse_entries(entries_str) do
    entries =
      String.split(entries_str, ",", trim: true)
      |> Enum.map(&parse_entry/1)

    case Enum.find(entries, &match?({:error, _}, &1)) do
      {:error, reason} -> {:error, reason}
      _ -> {:ok, entries}
    end
  end

  defp parse_entry("[" <> rest) do
    case String.split(rest, "]", parts: 2) do
      [addr, port_str] ->
        case :inet.parse_address(String.to_charlist(addr)) do
          {:ok, _tuple} ->
            port =
              port_str
              |> String.trim_leading(":")
              |> case do
                "" ->
                  @default_port

                s ->
                  case Integer.parse(s) do
                    {int, ""} -> int
                    _ -> return_error(:invalid_port)
                  end
              end

            %{address: addr, port: port}

          _ ->
            return_error(:invalid_ipv6)
        end

      _ ->
        return_error(:invalid_format)
    end
  end

  defp parse_entry(_), do: return_error(:invalid_format)

  defp return_error(reason), do: {:error, reason}
end
