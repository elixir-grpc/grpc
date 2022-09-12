defmodule GRPC.Server.Transcode do
  @spec path(term()) :: String.t()
  def path(%{pattern: {_method, path}}) do
    path
  end

  @spec method(term()) :: String.t()
  def method(%{pattern: {method, _path}}) do
    method
  end

  @doc """
  https://cloud.google.com/endpoints/docs/grpc-service-config/reference/rpc/google.api#google.api.HttpRule

  Template = "/" Segments [ Verb ] ;
  Segments = Segment { "/" Segment } ;
  Segment  = "*" | "**" | LITERAL | Variable ;
  Variable = "{" FieldPath [ "=" Segments ] "}" ;
  FieldPath = IDENT { "." IDENT } ;
  Verb     = ":" LITERAL ;
  """
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
    {{:literal, acc, []}, rest}
  end

  defp do_tokenize(<<h, t::binary>>, acc)
       when h in ?a..?z or h in ?A..?Z or h in ?0..?9 or h == ?_ or h == ?. do
    do_tokenize(t, <<acc::binary, h>>)
  end

  defp do_tokenize(<<>>, acc) do
    {{:literal, acc, []}, <<>>}
  end

  @spec parse(list(tuple()), list(), list()) :: list()
  def parse([], params, segments) do
    {Enum.reverse(params), Enum.reverse(segments)}
  end

  def parse([{:/, _} | rest], params, segments) do
    parse(rest, params, segments)
  end

  def parse([{:*, _} | rest], params, segments) do
    parse(rest, params, [{:_, []} | segments])
  end

  def parse([{:literal, literal, _} | rest], params, segments) do
    parse(rest, params, [literal | segments])
  end

  def parse([{:"{", _} | rest], params, segments) do
    {params, segments, rest} = parse_binding(rest, params, segments)
    parse(rest, params, segments)
  end

  defp parse_binding([{:"}", []} | rest], params, segments) do
    {params, segments, rest}
  end

  defp parse_binding(
         [{:literal, lit, _}, {:=, _}, {:literal, assign, _} = a | rest],
         params,
         segments
       ) do

    {variable, _} = param = field_path(lit)
    # assign = field_path(assign)

    parse_binding(rest, [param | params], [{variable, [assign]} | segments])
  end

  defp parse_binding([{:literal, lit, []} | rest], params, segments) do
    {variable, _} = param = field_path(lit)
    parse_binding(rest, [param | params], [{variable, []} | segments])
  end

  def field_path(identifier) do
    [root | path] = String.split(identifier, ".")
    {String.to_atom(root), path}
  end

end
