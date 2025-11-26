%% Copyright (c) Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(cow_link).
-compile({no_auto_import, [link/1]}).

-export([parse_link/1]).
-export([resolve_link/2]).
-export([resolve_link/3]).
-export([link/1]).

-include("cow_inline.hrl").
-include("cow_parse.hrl").

-type link() :: #{
	target := binary(),
	rel := binary(),
	attributes := [{binary(), binary()}]
}.
-export_type([link/0]).

-type resolve_opts() :: #{
	allow_anchor => boolean()
}.

-type uri() :: uri_string:uri_map() | uri_string:uri_string() | undefined.

%% Parse a link header.

%% This function returns the URI target from the header directly.
%% Relative URIs must then be resolved as per RFC3986 5. In some
%% cases it might not be possible to resolve URIs, for example when
%% the link header is returned with a 404 status code.
-spec parse_link(binary()) -> [link()].
parse_link(Link) ->
	before_target(Link, []).

before_target(<<>>, Acc) -> lists:reverse(Acc);
before_target(<<$<,R/bits>>, Acc) -> target(R, Acc, <<>>);
before_target(<<C,R/bits>>, Acc) when ?IS_WS(C) -> before_target(R, Acc).

target(<<$>,R/bits>>, Acc, T) -> param_sep(R, Acc, T, []);
target(<<C,R/bits>>, Acc, T) -> target(R, Acc, <<T/binary, C>>).

param_sep(<<>>, Acc, T, P) -> lists:reverse(acc_link(Acc, T, P));
param_sep(<<$,,R/bits>>, Acc, T, P) -> before_target(R, acc_link(Acc, T, P));
param_sep(<<$;,R/bits>>, Acc, T, P) -> before_param(R, Acc, T, P);
param_sep(<<C,R/bits>>, Acc, T, P) when ?IS_WS(C) -> param_sep(R, Acc, T, P).

before_param(<<C,R/bits>>, Acc, T, P) when ?IS_WS(C) -> before_param(R, Acc, T, P);
before_param(<<C,R/bits>>, Acc, T, P) when ?IS_TOKEN(C) -> ?LOWER(param, R, Acc, T, P, <<>>).

param(<<$=,$",R/bits>>, Acc, T, P, K) -> quoted(R, Acc, T, P, K, <<>>);
param(<<$=,C,R/bits>>, Acc, T, P, K) when ?IS_TOKEN(C) -> value(R, Acc, T, P, K, <<C>>);
param(<<C,R/bits>>, Acc, T, P, K) when ?IS_TOKEN(C) -> ?LOWER(param, R, Acc, T, P, K).

quoted(<<$",R/bits>>, Acc, T, P, K, V) -> param_sep(R, Acc, T, [{K, V}|P]);
quoted(<<$\\,C,R/bits>>, Acc, T, P, K, V) when ?IS_VCHAR_OBS(C) -> quoted(R, Acc, T, P, K, <<V/binary,C>>);
quoted(<<C,R/bits>>, Acc, T, P, K, V) when ?IS_VCHAR_OBS(C) -> quoted(R, Acc, T, P, K, <<V/binary,C>>).

value(<<C,R/bits>>, Acc, T, P, K, V) when ?IS_TOKEN(C) -> value(R, Acc, T, P, K, <<V/binary,C>>);
value(R, Acc, T, P, K, V) -> param_sep(R, Acc, T, [{K, V}|P]).

acc_link(Acc, Target, Params0) ->
	Params1 = lists:reverse(Params0),
	%% The rel parameter MUST be present. (RFC8288 3.3)
	{value, {_, Rel}, Params2} = lists:keytake(<<"rel">>, 1, Params1),
	%% Occurrences after the first MUST be ignored by parsers.
	Params = filter_out_duplicates(Params2, #{}),
	[#{
		target => Target,
		rel => ?LOWER(Rel),
		attributes => Params
	}|Acc].

%% This function removes duplicates for attributes that don't allow them.
filter_out_duplicates([], _) ->
	[];
%% The "rel" is mandatory and was already removed from params.
filter_out_duplicates([{<<"rel">>, _}|Tail], State) ->
	filter_out_duplicates(Tail, State);
filter_out_duplicates([{<<"anchor">>, _}|Tail], State=#{anchor := true}) ->
	filter_out_duplicates(Tail, State);
filter_out_duplicates([{<<"media">>, _}|Tail], State=#{media := true}) ->
	filter_out_duplicates(Tail, State);
filter_out_duplicates([{<<"title">>, _}|Tail], State=#{title := true}) ->
	filter_out_duplicates(Tail, State);
filter_out_duplicates([{<<"title*">>, _}|Tail], State=#{title_star := true}) ->
	filter_out_duplicates(Tail, State);
filter_out_duplicates([{<<"type">>, _}|Tail], State=#{type := true}) ->
	filter_out_duplicates(Tail, State);
filter_out_duplicates([Tuple={<<"anchor">>, _}|Tail], State) ->
	[Tuple|filter_out_duplicates(Tail, State#{anchor => true})];
filter_out_duplicates([Tuple={<<"media">>, _}|Tail], State) ->
	[Tuple|filter_out_duplicates(Tail, State#{media => true})];
filter_out_duplicates([Tuple={<<"title">>, _}|Tail], State) ->
	[Tuple|filter_out_duplicates(Tail, State#{title => true})];
filter_out_duplicates([Tuple={<<"title*">>, _}|Tail], State) ->
	[Tuple|filter_out_duplicates(Tail, State#{title_star => true})];
filter_out_duplicates([Tuple={<<"type">>, _}|Tail], State) ->
	[Tuple|filter_out_duplicates(Tail, State#{type => true})];
filter_out_duplicates([Tuple|Tail], State) ->
	[Tuple|filter_out_duplicates(Tail, State)].

-ifdef(TEST).
parse_link_test_() ->
	Tests = [
		{<<>>, []},
		{<<"   ">>, []},
		%% Examples from the RFC.
		{<<"<http://example.com/TheBook/chapter2>; rel=\"previous\"; title=\"previous chapter\"">>, [
			#{
				target => <<"http://example.com/TheBook/chapter2">>,
				rel => <<"previous">>,
				attributes => [
					{<<"title">>, <<"previous chapter">>}
				]
			}
		]},
		{<<"</>; rel=\"http://example.net/foo\"">>, [
			#{
				target => <<"/">>,
				rel => <<"http://example.net/foo">>,
				attributes => []
			}
		]},
		{<<"</terms>; rel=\"copyright\"; anchor=\"#foo\"">>, [
			#{
				target => <<"/terms">>,
				rel => <<"copyright">>,
				attributes => [
					{<<"anchor">>, <<"#foo">>}
				]
			}
		]},
%		{<<"</TheBook/chapter2>; rel=\"previous\"; title*=UTF-8'de'letztes%20Kapitel, "
%			"</TheBook/chapter4>; rel=\"next\"; title*=UTF-8'de'n%c3%a4chstes%20Kapitel">>, [
%			%% @todo
%		]}
		{<<"<http://example.org/>; rel=\"start http://example.net/relation/other\"">>, [
			#{
				target => <<"http://example.org/">>,
				rel => <<"start http://example.net/relation/other">>,
				attributes => []
			}
		]},
		{<<"<https://example.org/>; rel=\"start\", "
			"<https://example.org/index>; rel=\"index\"">>, [
			#{
				target => <<"https://example.org/">>,
				rel => <<"start">>,
				attributes => []
			},
			#{
				target => <<"https://example.org/index">>,
				rel => <<"index">>,
				attributes => []
			}
		]},
		%% Relation types are case insensitive.
		{<<"</>; rel=\"SELF\"">>, [
			#{
				target => <<"/">>,
				rel => <<"self">>,
				attributes => []
			}
		]},
		{<<"</>; rel=\"HTTP://EXAMPLE.NET/FOO\"">>, [
			#{
				target => <<"/">>,
				rel => <<"http://example.net/foo">>,
				attributes => []
			}
		]},
		%% Attribute names are case insensitive.
		{<<"</terms>; REL=\"copyright\"; ANCHOR=\"#foo\"">>, [
			#{
				target => <<"/terms">>,
				rel => <<"copyright">>,
				attributes => [
					{<<"anchor">>, <<"#foo">>}
				]
			}
		]}
	],
	[{V, fun() -> R = parse_link(V) end} || {V, R} <- Tests].
-endif.

%% Resolve a link based on the context URI and options.

-spec resolve_link(Link, uri()) -> Link | false when Link::link().
resolve_link(Link, ContextURI) ->
	resolve_link(Link, ContextURI, #{}).

-spec resolve_link(Link, uri(), resolve_opts()) -> Link | false when Link::link().
%% When we do not have a context URI we only succeed when the target URI is absolute.
%% The target URI will only be normalized in that case.
resolve_link(Link=#{target := TargetURI}, undefined, _) ->
	case uri_string:parse(TargetURI) of
		URIMap = #{scheme := _} ->
			Link#{target => uri_string:normalize(URIMap)};
		_ ->
			false
	end;
resolve_link(Link=#{attributes := Params}, ContextURI, Opts) ->
	AllowAnchor = maps:get(allow_anchor, Opts, true),
	case lists:keyfind(<<"anchor">>, 1, Params) of
		false ->
			do_resolve_link(Link, ContextURI);
		{_, Anchor} when AllowAnchor ->
			do_resolve_link(Link, resolve(Anchor, ContextURI));
		_ ->
			false
	end.

do_resolve_link(Link=#{target := TargetURI}, ContextURI) ->
	Link#{target => uri_string:recompose(resolve(TargetURI, ContextURI))}.

-ifdef(TEST).
resolve_link_test_() ->
	Tests = [
		%% No context URI available.
		{#{target => <<"http://a/b/./c">>}, undefined, #{},
			#{target => <<"http://a/b/c">>}},
		{#{target => <<"a/b/./c">>}, undefined, #{},
			false},
		%% Context URI available, allow_anchor => true.
		{#{target => <<"http://a/b">>, attributes => []}, <<"http://a/c">>, #{},
			#{target => <<"http://a/b">>, attributes => []}},
		{#{target => <<"b">>, attributes => []}, <<"http://a/c">>, #{},
			#{target => <<"http://a/b">>, attributes => []}},
		{#{target => <<"b">>, attributes => [{<<"anchor">>, <<"#frag">>}]}, <<"http://a/c">>, #{},
			#{target => <<"http://a/b">>, attributes => [{<<"anchor">>, <<"#frag">>}]}},
		{#{target => <<"b">>, attributes => [{<<"anchor">>, <<"d/e">>}]}, <<"http://a/c">>, #{},
			#{target => <<"http://a/d/b">>, attributes => [{<<"anchor">>, <<"d/e">>}]}},
		%% Context URI available, allow_anchor => false.
		{#{target => <<"http://a/b">>, attributes => []}, <<"http://a/c">>, #{allow_anchor => false},
			#{target => <<"http://a/b">>, attributes => []}},
		{#{target => <<"b">>, attributes => []}, <<"http://a/c">>, #{allow_anchor => false},
			#{target => <<"http://a/b">>, attributes => []}},
		{#{target => <<"b">>, attributes => [{<<"anchor">>, <<"#frag">>}]},
			<<"http://a/c">>, #{allow_anchor => false}, false},
		{#{target => <<"b">>, attributes => [{<<"anchor">>, <<"d/e">>}]},
			<<"http://a/c">>, #{allow_anchor => false}, false}
	],
	[{iolist_to_binary(io_lib:format("~0p", [L])),
		fun() -> R = resolve_link(L, C, O) end} || {L, C, O, R} <- Tests].
-endif.

%% @todo This function has been added to Erlang/OTP 22.3 as uri_string:resolve/2,3.
resolve(URI, BaseURI) ->
	case resolve1(ensure_map_uri(URI), BaseURI) of
		TargetURI = #{path := Path0} ->
			%% We remove dot segments. Normalizing the entire URI
			%% will sometimes add an extra slash we don't want.
			#{path := Path} = uri_string:normalize(#{path => Path0}, [return_map]),
			TargetURI#{path => Path};
		TargetURI ->
			TargetURI
	end.

resolve1(URI=#{scheme := _}, _) ->
	URI;
resolve1(URI=#{host := _}, BaseURI) ->
	#{scheme := Scheme} = ensure_map_uri(BaseURI),
	URI#{scheme => Scheme};
resolve1(URI=#{path := <<>>}, BaseURI0) ->
	BaseURI = ensure_map_uri(BaseURI0),
	Keys = case maps:is_key(query, URI) of
		true -> [scheme, host, port, path];
		false -> [scheme, host, port, path, query]
	end,
	maps:merge(URI, maps:with(Keys, BaseURI));
resolve1(URI=#{path := <<"/",_/bits>>}, BaseURI0) ->
	BaseURI = ensure_map_uri(BaseURI0),
	maps:merge(URI, maps:with([scheme, host, port], BaseURI));
resolve1(URI=#{path := Path}, BaseURI0) ->
	BaseURI = ensure_map_uri(BaseURI0),
	maps:merge(
		URI#{path := merge_paths(Path, BaseURI)},
		maps:with([scheme, host, port], BaseURI)).

merge_paths(Path, #{host := _, path := <<>>}) ->
	<<$/, Path/binary>>;
merge_paths(Path, #{path := BasePath0}) ->
	case string:split(BasePath0, <<$/>>, trailing) of
		[BasePath, _] -> <<BasePath/binary, $/, Path/binary>>;
		[_] -> <<$/, Path/binary>>
	end.

ensure_map_uri(URI) when is_map(URI) -> URI;
ensure_map_uri(URI) -> uri_string:parse(iolist_to_binary(URI)).

-ifdef(TEST).
resolve_test_() ->
	Tests = [
		%% 5.4.1.  Normal Examples
		{<<"g:h">>, <<"g:h">>},
		{<<"g">>, <<"http://a/b/c/g">>},
		{<<"./g">>, <<"http://a/b/c/g">>},
		{<<"g/">>, <<"http://a/b/c/g/">>},
		{<<"/g">>, <<"http://a/g">>},
		{<<"//g">>, <<"http://g">>},
		{<<"?y">>, <<"http://a/b/c/d;p?y">>},
		{<<"g?y">>, <<"http://a/b/c/g?y">>},
		{<<"#s">>, <<"http://a/b/c/d;p?q#s">>},
		{<<"g#s">>, <<"http://a/b/c/g#s">>},
		{<<"g?y#s">>, <<"http://a/b/c/g?y#s">>},
		{<<";x">>, <<"http://a/b/c/;x">>},
		{<<"g;x">>, <<"http://a/b/c/g;x">>},
		{<<"g;x?y#s">>, <<"http://a/b/c/g;x?y#s">>},
		{<<"">>, <<"http://a/b/c/d;p?q">>},
		{<<".">>, <<"http://a/b/c/">>},
		{<<"./">>, <<"http://a/b/c/">>},
		{<<"..">>, <<"http://a/b/">>},
		{<<"../">>, <<"http://a/b/">>},
		{<<"../g">>, <<"http://a/b/g">>},
		{<<"../..">>, <<"http://a/">>},
		{<<"../../">>, <<"http://a/">>},
		{<<"../../g">>, <<"http://a/g">>},
		%% 5.4.2.  Abnormal Examples
		{<<"../../../g">>, <<"http://a/g">>},
		{<<"../../../../g">>, <<"http://a/g">>},
		{<<"/./g">>, <<"http://a/g">>},
		{<<"/../g">>, <<"http://a/g">>},
		{<<"g.">>, <<"http://a/b/c/g.">>},
		{<<".g">>, <<"http://a/b/c/.g">>},
		{<<"g..">>, <<"http://a/b/c/g..">>},
		{<<"..g">>, <<"http://a/b/c/..g">>},
		{<<"./../g">>, <<"http://a/b/g">>},
		{<<"./g/.">>, <<"http://a/b/c/g/">>},
		{<<"g/./h">>, <<"http://a/b/c/g/h">>},
		{<<"g/../h">>, <<"http://a/b/c/h">>},
		{<<"g;x=1/./y">>, <<"http://a/b/c/g;x=1/y">>},
		{<<"g;x=1/../y">>, <<"http://a/b/c/y">>},
		{<<"g?y/./x">>, <<"http://a/b/c/g?y/./x">>},
		{<<"g?y/../x">>, <<"http://a/b/c/g?y/../x">>},
		{<<"g#s/./x">>, <<"http://a/b/c/g#s/./x">>},
		{<<"g#s/../x">>, <<"http://a/b/c/g#s/../x">>},
		{<<"http:g">>, <<"http:g">>} %% for strict parsers
	],
	[{V, fun() -> R = uri_string:recompose(resolve(V, <<"http://a/b/c/d;p?q">>)) end} || {V, R} <- Tests].
-endif.

%% Build a link header.

-spec link([#{
	target := binary(),
	rel := binary(),
	attributes := [{binary(), binary()}]
}]) -> iodata().
link(Links) ->
	lists:join(<<", ">>, [do_link(Link) || Link <- Links]).

do_link(#{target := TargetURI, rel := Rel, attributes := Params}) ->
	[
		$<, TargetURI, <<">"
		"; rel=\"">>, Rel, $",
		[[<<"; ">>, Key, <<"=\"">>, escape(iolist_to_binary(Value), <<>>), $"]
			|| {Key, Value} <- Params]
	].

escape(<<>>, Acc) -> Acc;
escape(<<$\\,R/bits>>, Acc) -> escape(R, <<Acc/binary,$\\,$\\>>);
escape(<<$\",R/bits>>, Acc) -> escape(R, <<Acc/binary,$\\,$\">>);
escape(<<C,R/bits>>, Acc) -> escape(R, <<Acc/binary,C>>).

-ifdef(TEST).
link_test_() ->
	Tests = [
		{<<>>, []},
		%% Examples from the RFC.
		{<<"<http://example.com/TheBook/chapter2>; rel=\"previous\"; title=\"previous chapter\"">>, [
			#{
				target => <<"http://example.com/TheBook/chapter2">>,
				rel => <<"previous">>,
				attributes => [
					{<<"title">>, <<"previous chapter">>}
				]
			}
		]},
		{<<"</>; rel=\"http://example.net/foo\"">>, [
			#{
				target => <<"/">>,
				rel => <<"http://example.net/foo">>,
				attributes => []
			}
		]},
		{<<"</terms>; rel=\"copyright\"; anchor=\"#foo\"">>, [
			#{
				target => <<"/terms">>,
				rel => <<"copyright">>,
				attributes => [
					{<<"anchor">>, <<"#foo">>}
				]
			}
		]},
%		{<<"</TheBook/chapter2>; rel=\"previous\"; title*=UTF-8'de'letztes%20Kapitel, "
%			"</TheBook/chapter4>; rel=\"next\"; title*=UTF-8'de'n%c3%a4chstes%20Kapitel">>, [
%			%% @todo
%		]}
		{<<"<http://example.org/>; rel=\"start http://example.net/relation/other\"">>, [
			#{
				target => <<"http://example.org/">>,
				rel => <<"start http://example.net/relation/other">>,
				attributes => []
			}
		]},
		{<<"<https://example.org/>; rel=\"start\", "
			"<https://example.org/index>; rel=\"index\"">>, [
			#{
				target => <<"https://example.org/">>,
				rel => <<"start">>,
				attributes => []
			},
			#{
				target => <<"https://example.org/index">>,
				rel => <<"index">>,
				attributes => []
			}
		]},
		{<<"</>; rel=\"previous\"; quoted=\"name=\\\"value\\\"\"">>, [
			#{
				target => <<"/">>,
				rel => <<"previous">>,
				attributes => [
					{<<"quoted">>, <<"name=\"value\"">>}
				]
			}
		]}
	],
	[{iolist_to_binary(io_lib:format("~0p", [V])),
		fun() -> R = iolist_to_binary(link(V)) end} || {R, V} <- Tests].
-endif.
