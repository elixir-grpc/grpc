defmodule GRPC.Client.ServiceConfig do
  @moduledoc """
  Represents the gRPC `ServiceConfig` parsed from JSON, which can come from DNS TXT records or xDS.

  The gRPC `ServiceConfig` allows a client to configure per-service and per-method
  behaviors such as load balancing, timeouts, and retry policies.

  ## Spec

  According to the gRPC specification ([service_config.md](https://github.com/grpc/grpc/blob/master/doc/service_config.md)):

  - **loadBalancingConfig**: a list of load balancing policies.  
    The client should pick the first policy it supports. Common values are:
      - `"pick_first"`: always pick the first server.
      - `"round_robin"`: distribute calls across servers in round-robin.

  - **methodConfig**: a list of configurations applied to specific methods or services.  
    Each entry can include:
      - `"name"`: a list of `{ "service": "<service_name>", "method": "<method_name>" }`
        or `{ "service": "<service_name>" }` to match all methods in the service.
      - `"timeout"`: RPC timeout as a string (e.g., `"1.000000001s"`).
      - `"retryPolicy"`: optional retry policy map.
      - Other optional method-level settings.

  ## Example TXT record

  A DNS TXT record for a service `my-service.local` might look like this:

      _grpc_config.my-service.local 3600 TXT
      "grpc_config={
        \"loadBalancingConfig\":[{\"round_robin\":{}}],
        \"methodConfig\":[
          {
            \"name\":[
              {\"service\":\"foo\",\"method\":\"bar\"},
              {\"service\":\"baz\"}
            ],
            \"timeout\":\"1.000000001s\"
          }
        ]
      }"

  This JSON will be parsed into a `%GRPC.Client.ServiceConfig{}` struct with:

      %GRPC.Client.ServiceConfig{
        load_balancing_policy: :round_robin,
        method_configs: [
          %{
            "name" => [
              %{"service" => "foo", "method" => "bar"},
              %{"service" => "baz"}
            ],
            "timeout" => "1.000000001s"
          }
        ],
        raw: <original JSON map>
      }

  ## Usage

  ```elixir
  {:ok, config} = GRPC.Client.ServiceConfig.parse(txt_json)
  IO.inspect(config.load_balancing_policy)
  IO.inspect(config.method_configs)
  ```
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
      {:ok, map} -> from_map(map)
      error -> error
    end
  end

  defp from_map(map) do
    lb_policy =
      map
      |> Map.get("loadBalancingConfig", [%{"pick_first" => %{}}])
      |> List.first()
      |> Map.keys()
      |> case do
        [key] -> String.to_existing_atom(key)
        _ -> :pick_first
      end

    %__MODULE__{
      load_balancing_policy: lb_policy,
      method_configs: Map.get(map, "methodConfig", []),
      raw: map
    }
  end
end
