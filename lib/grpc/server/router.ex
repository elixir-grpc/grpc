defmodule GRPC.Server.Router do
  @moduledoc """
  """
  alias __MODULE__.Template

  @type http_method :: :get | :put | :post | :patch | :delete
  @type route :: {http_method(), String.t(), Template.matchers()}

  @wildcards [:_, :__]

  @spec build_route(binary() | map()) :: route()
  def build_route(path) when is_binary(path), do: build_route(:post, path)
  def build_route(%{pattern: {method, path}}), do: build_route(method, path)

  @doc """
  Builds a t:route/0 from a URL path or `t:Google.Api.Http.t/0`.

  The matcher part in the route can be used in `match/3` to match on a URL path or a list of segments.

  ## Examples

      {:get, "/v1/messages/{message_id}", match} = GRPC.Server.Router.build_route(:get, "/v1/messages/{message_id}")

      {:get, path, match} = GRPC.Server.Router.build_route(:get, "/v1/{book.location=shelves/*}/books/{book.name=*}")
      {true, %{"book.location": "shelves/example-shelf", "book.name": "example-book"}} = GRPC.Server.Router.match("/v1/shelves/example-shelf/books/example-book", match, [])
  """
  @spec build_route(atom(), binary()) :: route()
  def build_route(method, path) when is_binary(path) do
    match =
      path
      |> Template.tokenize([])
      |> Template.parse([])

    {method, path, match}
  end

  @doc """
  Split URL path into segments, removing the leading and trailing slash.

  ## Examples

       ["v1", "messages"] = GRPC.Server.Router.split_path("/v1/messages")
  """
  @spec split_path(String.t()) :: iolist()
  def split_path(bin) do
    for segment <- String.split(bin, "/"), segment != "", do: segment
  end

  @doc """
  Matches a URL path or URL segements against a compiled route matcher. Matched bindings from the segments are extracted
  into a map. If the same variable name is used in multiple bindings, the value must match otherwise the route is not considered a match.

  ## Examples

      {_, _, match} = GRPC.Server.Router.build_route(:get, "/v1/{name=messages}")
      {true, %{name: "messages"}} = GRPC.Server.Router.match("/v1/messages", match)
      false = GRPC.Server.Router.match("/v1/messages/foobar", match)


      {_, _, match} = GRPC.Server.Router.build_route(:get, "/v1/{name=shelves/*/books/*)
      {true, %{name: "shelves/books/book"}} = GRPC.Server.Router.match("/v1/shelves/example-shelf/books/book", match)

      false = GRPC.Server.Router.match("/v1/shelves/example-shelf/something-els/books/book", match)

  """
  @spec match(String.t() | [String.t()], Template.matchers()) :: {true, map()} | false
  def match(path, match) do
    match(path, match, %{})
  end

  @spec match(String.t() | [String.t()], Template.matchers(), map()) :: {true, map()} | false
  def match(path, match, bindings) when is_binary(path) do
    path
    |> split_path()
    |> match(match, bindings)
  end

  # The last matcher is a 'catch all' matcher, so the rest of segments are matching.
  def match(_segments, [{:__, []}], bindings) do
    {true, bindings}
  end

  # 'Any' matcher matches a single segment, cont. recursion.
  def match([_s | segments], [{:_, _} | matchers], bindings) do
    match(segments, matchers, bindings)
  end

  # Matching against a 'literal' match, cont. recursion
  def match([segment | segments], [_literal = segment | matchers], bindings) do
    match(segments, matchers, bindings)
  end

  # /v1/{a=*} is the same as /v1/{a}. Matching and binding the segment to `binding`
  def match([segment | tail], [{binding, [{:_, _}]} | matchers], bindings) do
    put_binding(bindings, binding, segment, tail, matchers)
  end

  # /v1/{a=messages} binding a matching literal
  def match([segment | segments], [{binding, [segment]} | matchers], bindings) do
    put_binding(bindings, binding, segment, segments, matchers)
  end

  # /v1/{a=*} /v1/{a=**} theres no more matchers after the wildcard, bind
  # the rest of the segments to `binding`
  def match(rest, [{binding, [{any, _}]}], bindings) when any in @wildcards do
    value = Enum.join(rest, "/")

    match([], [], Map.put(bindings, binding, value))
  end

  # /v1/{a=messages/*} /v1/{a=messages/**} theres no more matchers after the wildcard, bind
  # the rest of the segments including the current segment to `binding`
  def match([segment | _] = segments, [{binding, [segment, {any, _}]}], bindings)
      when any in @wildcards do
    value = Enum.join(segments, "/")

    match([], [], Map.put(bindings, binding, value))
  end

  # /v1/{a=users/*/messages/*}/suffix. There are sub-matches inside the capture
  # so the segments are matched with match submatches until an end-condition
  # is reached
  def match(
        [segment | tail],
        [{binding, [segment | sub_matches]} | matches],
        bindings
      ) do
    end_condition =
      case matches do
        [next | _] -> next
        [] -> :undefined
      end

    with {matched_segments, tail} <- match_until(tail, end_condition, sub_matches, []) do
      value = Enum.join([segment | matched_segments], "/")
      bindings = Map.put(bindings, binding, value)

      match(tail, matches, bindings)
    end
  end

  # /v1/messages/{message_id} simple binding
  def match([segment | segments], [{binding, []} | matchers], bindings) when is_atom(binding) do
    put_binding(bindings, binding, segment, segments, matchers)
  end

  def match([], [], bindings) do
    {true, bindings}
  end

  # no match
  def match(_segments, _matches, _bindings) do
    false
  end

  # End recursion, since there's no "outside" matches we should iterate to end of segments
  defp match_until([], :undefined, [], acc) do
    {Enum.reverse(acc), []}
  end

  # End recursion, end condition is a binding with a matching complex start segment
  defp match_until(
         [segment | _] = segments,
         _end_condition = {binding, [segment | _]},
         [],
         acc
       )
       when is_atom(binding) do
    {Enum.reverse(acc), segments}
  end

  # End recursion since the submatch contains a trailing wildcard but we have more matches "outside" this sub-segment
  defp match_until([segment | _] = segments, _end_condition = segment, [], acc) do
    {Enum.reverse(acc), segments}
  end

  # Reached the "end" of this wildcard, so we proceed with the next match
  defp match_until([_segment | _] = segments, end_condition, [{:__, []}, match | matches], acc) do
    match_until(segments, end_condition, [match | matches], acc)
  end

  # Segment is matching the wildcard and have not reached "end" of wildcard
  defp match_until([segment | segments], end_condition, [{:__, []} | _] = matches, acc) do
    match_until(segments, end_condition, matches, [segment | acc])
  end

  # Current match is matching segment, add to accumulator and set next match as the current one
  defp match_until([segment | segments], end_condition, [segment | matches], acc) do
    match_until(segments, end_condition, matches, [segment | acc])
  end

  # 'Any' match is matching first segment, add to accumulator and set next match as the current one
  defp match_until([segment | segments], end_condition, [{:_, []} | matches], acc) do
    match_until(segments, end_condition, matches, [segment | acc])
  end

  # No match
  defp match_until(_segments, _end_condition, _matches, _acc) do
    false
  end

  defp put_binding(bindings, binding, value, segments, matchers) do
    case bindings do
      %{^binding => ^value} ->
        match(segments, matchers, bindings)

      %{^binding => _} ->
        false

      _ ->
        match(segments, matchers, Map.put(bindings, binding, value))
    end
  end
end
