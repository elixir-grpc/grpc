defmodule GRPC.Server.Transcode do
  alias __MODULE__.{Query, Template, FieldPath}

  @doc """
  Leaf request fields (recursive expansion nested messages in the request message) are classified into three categories:

  1. Fields referred by the path template. They are passed via the URL path.
  2. Fields referred by the HttpRule.body. They are passed via the HTTP request body.
  3. All other fields are passed via the URL query parameters, and the parameter name is the field path in the request message. A repeated field can be represented as multiple query parameters under the same name.

  If HttpRule.body is "*", there is no URL query parameter, all fields are passed via URL path and HTTP request body.

  If HttpRule.body is omitted, there is no HTTP request body, all fields are passed via URL path and URL query parameters.
  """
  @spec map_request(Google.Api.HttpRule.t(), map(), map(), String.t(), module()) ::
          {:ok, struct()} | {:error, term()}
  def map_request(
        %Google.Api.HttpRule{body: ""},
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
        %Google.Api.HttpRule{} = rule,
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

  defp map_request_body(%Google.Api.HttpRule{body: "*"}, request_body), do: request_body
  defp map_request_body(%Google.Api.HttpRule{body: ""}, request_body), do: request_body

  defp map_request_body(%Google.Api.HttpRule{body: field}, request_body),
    do: %{field => request_body}

  @spec map_response_body(Google.Api.HttpRule.t() | map(), map()) :: map()
  def map_response_body(%Google.Api.HttpRule{response_body: ""}, response_body), do: response_body

  # TODO The field is required to be present on the toplevel response message
  def map_response_body(%Google.Api.HttpRule{response_body: field}, response_body) do
    key = String.to_existing_atom(field)
    Map.get(response_body, key)
  end

  def map_response_body(%{}, response_body), do: response_body

  @spec to_path({atom(), Template.route()}) :: String.t()
  def to_path({_method, {_bindings, segments}} = _spec) do
    match =
      segments
      |> Enum.map(&segment_to_string/1)
      |> Enum.join("/")

    "/" <> match
  end

  defp segment_to_string({binding, _}) when is_atom(binding), do: ":#{Atom.to_string(binding)}"
  defp segment_to_string(segment), do: segment

  @spec build_route(Google.Api.HttpRule.t()) :: {atom(), Template.route()}
  def build_route(%Google.Api.HttpRule{pattern: {method, path}}) do
    route =
      path
      |> Template.tokenize([])
      |> Template.parse([], [])

    {method, route}
  end

  @spec map_path_bindings(map()) :: map()
  def map_path_bindings(bindings) when bindings == %{}, do: bindings

  def map_path_bindings(bindings) do
    for {k, v} <- bindings, reduce: %{} do
      acc -> FieldPath.decode_pair({to_string(k), v}, acc)
    end
  end
end
