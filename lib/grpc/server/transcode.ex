defmodule GRPC.Server.Transcode do
  alias __MODULE__.Query

  @spec map_request(map(), map(), String.t(), module()) :: {:ok, struct()} | {:error, term()}
  def map_request(body_request, path_bindings, query_string, req_mod) do
    path_bindings = Map.new(path_bindings, fn {k, v} -> {to_string(k), v} end)
    query = Query.decode(query_string)
    request = Enum.reduce([query, body_request], path_bindings, &Map.merge(&2, &1))

    Protobuf.JSON.from_decoded(request, req_mod)
  end

  @spec to_path(term()) :: String.t()
  def to_path({_method, {_bindings, segments}} = _spec) do
    match =
      segments
      |> Enum.map(&segment_to_string/1)
      |> Enum.join("/")

    "/" <> match
  end

  defp segment_to_string({binding, _}) when is_atom(binding), do: ":#{Atom.to_string(binding)}"
  defp segment_to_string(segment), do: segment

  # https://cloud.google.com/endpoints/docs/grpc-service-config/reference/rpc/google.api#google.api.HttpRule

  # Template = "/" Segments [ Verb ] ;
  # Segments = Segment { "/" Segment } ;
  # Segment  = "*" | "**" | LITERAL | Variable ;
  # Variable = "{" FieldPath [ "=" Segments ] "}" ;
  # FieldPath = IDENT { "." IDENT } ;
  # Verb     = ":" LITERAL ;
  #
  @spec build_route(term()) :: tuple()
  def build_route(%Google.Api.HttpRule{pattern: {method, path}}) do
    route =
      path
      |> tokenize([])
      |> parse([], [])

    {method, route}
  end

  @spec tokenize(binary(), list()) :: list()
  def tokenize(path, tokens \\ [])

  def tokenize(<<>>, tokens) do
    Enum.reverse(tokens)
  end

  def tokenize(segments, tokens) do
    {token, rest} = do_tokenize(segments, <<>>)
    tokenize(rest, [token | tokens])
  end

  @terminals [?/, ?{, ?}, ?=, ?*]
  defp do_tokenize(<<h, t::binary>>, <<>>) when h in @terminals do
    # parse(t, acc)
    {{List.to_atom([h]), []}, t}
  end

  defp do_tokenize(<<h, _::binary>> = rest, acc) when h in @terminals do
    {{:identifier, acc, []}, rest}
  end

  defp do_tokenize(<<h, t::binary>>, acc)
       when h in ?a..?z or h in ?A..?Z or h in ?0..?9 or h == ?_ or h == ?. do
    do_tokenize(t, <<acc::binary, h>>)
  end

  defp do_tokenize(<<>>, acc) do
    {{:identifier, acc, []}, <<>>}
  end

  @spec parse(list(tuple()), list(), list()) :: {list(), list()}
  def parse([], params, segments) do
    {Enum.reverse(params), Enum.reverse(segments)}
  end

  def parse([{:/, _} | rest], params, segments) do
    parse(rest, params, segments)
  end

  def parse([{:*, _} | rest], params, segments) do
    parse(rest, params, [{:_, []} | segments])
  end

  def parse([{:identifier, identifier, _} | rest], params, segments) do
    parse(rest, params, [identifier | segments])
  end

  def parse([{:"{", _} | rest], params, segments) do
    {params, segments, rest} = parse_binding(rest, params, segments)
    parse(rest, params, segments)
  end

  defp parse_binding([{:"}", []} | rest], params, segments) do
    {params, segments, rest}
  end

  defp parse_binding(
         [{:identifier, id, _}, {:=, _}, {:identifier, assign, _} | rest],
         params,
         segments
       ) do
    {variable, _} = param = field_path(id)
    # assign = field_path(assign)

    parse_binding(rest, [param | params], [{variable, [assign]} | segments])
  end

  defp parse_binding([{:identifier, id, []} | rest], params, segments) do
    {variable, _} = param = field_path(id)
    parse_binding(rest, [param | params], [{variable, []} | segments])
  end

  def field_path(identifier) do
    [root | path] = String.split(identifier, ".")
    {String.to_atom(root), path}
  end
end
