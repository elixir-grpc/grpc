defmodule GRPC.Client.ServiceConfig do
  @moduledoc """
  Represents the gRPC ServiceConfig parsed from JSON (from DNS TXT or xDS).
  """

  defstruct load_balancing_policy: :pick_first,
            method_configs: [],
            raw: %{}

  @type t :: %__MODULE__{
          load_balancing_policy: atom(),
          method_configs: list(),
          raw: map()
        }

  def parse(nil), do: {:ok, %__MODULE__{}}

  def parse(json) when is_binary(json) do
    case Jason.decode(json) do
      {:ok, map} -> {:ok, from_map(map)}
      error -> error
    end
  end

  defp from_map(map) do
    lb =
      map
      |> Map.get("loadBalancingPolicy", "pick_first")
      |> String.downcase()
      |> String.to_existing_atom()

    %__MODULE__{
      load_balancing_policy: lb,
      method_configs: Map.get(map, "methodConfig", []),
      raw: map
    }
  end
end
