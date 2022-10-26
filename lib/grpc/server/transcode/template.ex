defmodule GRPC.Server.Transcode.Template do
  @moduledoc false
  # https://cloud.google.com/endpoints/docs/grpc-service-config/reference/rpc/google.api#google.api.HttpRule
  # Template = "/" Segments [ Verb ] ;
  # Segments = Segment { "/" Segment } ;
  # Segment  = "*" | "**" | LITERAL | Variable ;
  # Variable = "{" FieldPath [ "=" Segments ] "}" ;
  # FieldPath = IDENT { "." IDENT } ;
  # Verb     = ":" LITERAL ;
  @type segments :: list(atom | String.t())
  @type route :: {atom(), segments()}

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

  @spec parse(list(tuple()), list()) :: route() | {list(), list()}
  def parse([], segments) do
    Enum.reverse(segments)
  end

  def parse([{:/, _} | rest], segments) do
    parse(rest, segments)
  end

  def parse([{:*, _}, {:*, _} | rest], segments) do
    parse(rest, [{:__, []} | segments])
  end

  def parse([{:*, _} | rest], segments) do
    parse(rest, [{:_, []} | segments])
  end

  def parse([{:identifier, identifier, _} | rest], segments) do
    parse(rest, [identifier | segments])
  end

  def parse([{:"{", _} | rest], segments) do
    {segments, rest} = parse_binding(rest, segments)
    parse(rest, segments)
  end

  def parse([{:"}", _} | _rest] = acc, segments) do
    {segments, acc}
  end

  defp parse_binding([], segments) do
    {segments, []}
  end

  defp parse_binding([{:"}", []} | rest], segments) do
    {segments, rest}
  end

  defp parse_binding(
         [{:identifier, id, _}, {:=, _} | rest],
         segments
       ) do
    variable = field_path(id)
    {assign, rest} = parse(rest, [])

    parse_binding(rest, [{variable, Enum.reverse(assign)} | segments])
  end

  defp parse_binding([{:identifier, id, []} | rest], segments) do
    variable = field_path(id)
    parse_binding(rest, [{variable, []} | segments])
  end

  defp field_path(identifier) do
    String.to_atom(identifier)
  end
end
