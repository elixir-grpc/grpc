defmodule GRPC.Server.Transcode do
  @moduledoc false
  alias GRPC.Server.Router.Query
  alias GRPC.Server.Router.FieldPath

  @type t :: map()
  # The request mapping follow the following rules:
  #
  # 1. Fields referred by the path template. They are passed via the URL path.
  # 2. Fields referred by the HttpRule.body. They are passed via the HTTP request body.
  # 3. All other fields are passed via the URL query parameters, and the parameter name is the field path in the request message. A repeated field can be represented as multiple query parameters under the same name.
  # If HttpRule.body is "*", there is no URL query parameter, all fields are passed via URL path and HTTP request body.
  # If HttpRule.body is omitted, there is no HTTP request body, all fields are passed via URL path and URL query parameters.
  @spec map_request(t(), map(), map(), String.t(), module()) ::
          {:ok, struct()} | {:error, term()}
  def map_request(
        %{body: ""},
        _body_request,
        path_bindings,
        query_string,
        req_mod
      ) do
    path_bindings = map_path_bindings(path_bindings)
    query = Query.decode(query_string)
    request = Map.merge(path_bindings, query)

    Protobuf.JSON.from_decoded(request, req_mod)
  end

  def map_request(
        %{body: "*"} = rule,
        body_request,
        path_bindings,
        _query_string,
        req_mod
      ) do
    path_bindings = map_path_bindings(path_bindings)
    body_request = map_request_body(rule, body_request)
    request = Map.merge(path_bindings, body_request)

    Protobuf.JSON.from_decoded(request, req_mod)
  end

  def map_request(
        %{} = rule,
        body_request,
        path_bindings,
        query_string,
        req_mod
      ) do
    path_bindings = map_path_bindings(path_bindings)
    query = Query.decode(query_string)
    body_request = map_request_body(rule, body_request)
    request = Enum.reduce([query, body_request], path_bindings, &Map.merge(&2, &1))

    Protobuf.JSON.from_decoded(request, req_mod)
  end

  defp map_request_body(%{body: "*"}, request_body), do: request_body
  defp map_request_body(%{body: ""}, request_body), do: request_body

  defp map_request_body(%{body: field}, request_body),
    do: %{field => request_body}

  @spec map_response_body(t() | map(), map()) :: map()
  def map_response_body(%{response_body: ""}, response_body), do: response_body

  def map_response_body(%{response_body: field}, response_body) do
    key = String.to_existing_atom(field)
    Map.get(response_body, key)
  end

  @spec map_path_bindings(map()) :: map()
  def map_path_bindings(bindings) when bindings == %{}, do: bindings

  def map_path_bindings(bindings) do
    for {k, v} <- bindings, reduce: %{} do
      acc -> FieldPath.decode_pair({to_string(k), v}, acc)
    end
  end
end
