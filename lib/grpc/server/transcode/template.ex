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
  @type bindings :: list(atom)
  @type route :: {bindings(), segments()}

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

  @spec parse(list(tuple()), list(), list()) :: route() | {list(), list(), list()}
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

  def parse([{:"}", _} | _rest] = acc, params, segments) do
    {params, segments, acc}
  end

  defp parse_binding([], params, segments) do
    {params, segments, []}
  end

  defp parse_binding([{:"}", []} | rest], params, segments) do
    {params, segments, rest}
  end

  defp parse_binding(
         [{:identifier, id, _}, {:=, _} | rest],
         params,
         segments
       ) do
    {variable, _} = param = field_path(id)
    {_, assign, rest} = parse(rest, [], [])

    parse_binding(rest, [param | params], [{variable, Enum.reverse(assign)} | segments])
  end

  defp parse_binding([{:identifier, id, []} | rest], params, segments) do
    {variable, _} = param = field_path(id)
    parse_binding(rest, [param | params], [{variable, []} | segments])
  end

  defp field_path(identifier) do
    id_key = String.to_atom(identifier)

    case String.split(identifier, ".") do
      [_root] -> {id_key, []}
      [_root | _] = path -> {id_key, path}
    end
  end
end
