defmodule GRPC.Server.Router.Template do
  @moduledoc false
  # https://cloud.google.com/endpoints/docs/grpc-service-config/reference/rpc/google.api#google.api.HttpRule
  # Template = "/" Segments [ Verb ] ;
  # Segments = Segment { "/" Segment } ;
  # Segment  = "*" | "**" | LITERAL | Variable ;
  # Variable = "{" FieldPath [ "=" Segments ] "}" ;
  # FieldPath = IDENT { "." IDENT } ;
  # Verb     = ":" LITERAL ;
  @type segment_match :: String.t() | {atom(), [segment_match]}
  @type matchers :: [segment_match]

  @spec tokenize(binary(), [tuple()]) :: [tuple()]
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

  @spec parse(tokens :: [tuple()], matchers()) :: matchers() | {matchers, tokens :: [tuple()]}
  def parse([], matchers) do
    Enum.reverse(matchers)
  end

  def parse([{:/, _} | rest], matchers) do
    parse(rest, matchers)
  end

  def parse([{:*, _}, {:*, _} | rest], matchers) do
    parse(rest, [{:__, []} | matchers])
  end

  def parse([{:*, _} | rest], matchers) do
    parse(rest, [{:_, []} | matchers])
  end

  def parse([{:identifier, identifier, _} | rest], matchers) do
    parse(rest, [identifier | matchers])
  end

  def parse([{:"{", _} | rest], matchers) do
    {matchers, rest} = parse_binding(rest, matchers)
    parse(rest, matchers)
  end

  def parse([{:"}", _} | _rest] = acc, matchers) do
    {matchers, acc}
  end

  defp parse_binding([], matchers) do
    {matchers, []}
  end

  defp parse_binding([{:"}", []} | rest], matchers) do
    {matchers, rest}
  end

  defp parse_binding(
         [{:identifier, id, _}, {:=, _} | rest],
         matchers
       ) do
    variable = field_path(id)
    {assign, rest} = parse(rest, [])

    parse_binding(rest, [{variable, Enum.reverse(assign)} | matchers])
  end

  defp parse_binding([{:identifier, id, []} | rest], matchers) do
    variable = field_path(id)
    parse_binding(rest, [{variable, []} | matchers])
  end

  defp field_path(identifier) do
    String.to_existing_atom(identifier)
  rescue
    _e ->
      String.to_atom(identifier)
  end
end
