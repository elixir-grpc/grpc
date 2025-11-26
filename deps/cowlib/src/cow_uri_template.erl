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

%% This is a full level 4 implementation of URI Templates
%% as defined by RFC6570.

-module(cow_uri_template).

-export([parse/1]).
-export([expand/2]).

-type op() :: simple_string_expansion
	| reserved_expansion
	| fragment_expansion
	| label_expansion_with_dot_prefix
	| path_segment_expansion
	| path_style_parameter_expansion
	| form_style_query_expansion
	| form_style_query_continuation.

-type var_list() :: [
	{no_modifier, binary()}
	| {{prefix_modifier, pos_integer()}, binary()}
	| {explode_modifier, binary()}
].

-type uri_template() :: [
	binary() | {expr, op(), var_list()}
].
-export_type([uri_template/0]).

-type variables() :: #{
	binary() => binary()
		| integer()
		| float()
		| [binary()]
		| #{binary() => binary()}
}.

-include("cow_inline.hrl").
-include("cow_parse.hrl").

%% Parse a URI template.

-spec parse(binary()) -> uri_template().
parse(URITemplate) ->
	parse(URITemplate, <<>>).

parse(<<>>, <<>>) ->
	[];
parse(<<>>, Acc) ->
	[Acc];
parse(<<${,R/bits>>, <<>>) ->
	parse_expr(R);
parse(<<${,R/bits>>, Acc) ->
	[Acc|parse_expr(R)];
%% @todo Probably should reject unallowed characters so that
%% we don't produce invalid URIs.
parse(<<C,R/bits>>, Acc) when C =/= $} ->
	parse(R, <<Acc/binary, C>>).

parse_expr(<<$+,R/bits>>) ->
	parse_var_list(R, reserved_expansion, []);
parse_expr(<<$#,R/bits>>) ->
	parse_var_list(R, fragment_expansion, []);
parse_expr(<<$.,R/bits>>) ->
	parse_var_list(R, label_expansion_with_dot_prefix, []);
parse_expr(<<$/,R/bits>>) ->
	parse_var_list(R, path_segment_expansion, []);
parse_expr(<<$;,R/bits>>) ->
	parse_var_list(R, path_style_parameter_expansion, []);
parse_expr(<<$?,R/bits>>) ->
	parse_var_list(R, form_style_query_expansion, []);
parse_expr(<<$&,R/bits>>) ->
	parse_var_list(R, form_style_query_continuation, []);
parse_expr(R) ->
	parse_var_list(R, simple_string_expansion, []).

parse_var_list(<<C,R/bits>>, Op, List)
		when ?IS_ALPHANUM(C) or (C =:= $_) ->
	parse_varname(R, Op, List, <<C>>).

parse_varname(<<C,R/bits>>, Op, List, Name)
		when ?IS_ALPHANUM(C) or (C =:= $_) or (C =:= $.) or (C =:= $%) ->
	parse_varname(R, Op, List, <<Name/binary,C>>);
parse_varname(<<$:,C,R/bits>>, Op, List, Name)
		when (C =:= $1) or (C =:= $2) or (C =:= $3) or (C =:= $4) or (C =:= $5)
			or (C =:= $6) or (C =:= $7) or (C =:= $8) or (C =:= $9) ->
	parse_prefix_modifier(R, Op, List, Name, <<C>>);
parse_varname(<<$*,$,,R/bits>>, Op, List, Name) ->
	parse_var_list(R, Op, [{explode_modifier, Name}|List]);
parse_varname(<<$*,$},R/bits>>, Op, List, Name) ->
	[{expr, Op, lists:reverse([{explode_modifier, Name}|List])}|parse(R, <<>>)];
parse_varname(<<$,,R/bits>>, Op, List, Name) ->
	parse_var_list(R, Op, [{no_modifier, Name}|List]);
parse_varname(<<$},R/bits>>, Op, List, Name) ->
	[{expr, Op, lists:reverse([{no_modifier, Name}|List])}|parse(R, <<>>)].

parse_prefix_modifier(<<C,R/bits>>, Op, List, Name, Acc)
		when ?IS_DIGIT(C), byte_size(Acc) < 4 ->
	parse_prefix_modifier(R, Op, List, Name, <<Acc/binary,C>>);
parse_prefix_modifier(<<$,,R/bits>>, Op, List, Name, Acc) ->
	parse_var_list(R, Op, [{{prefix_modifier, binary_to_integer(Acc)}, Name}|List]);
parse_prefix_modifier(<<$},R/bits>>, Op, List, Name, Acc) ->
	[{expr, Op, lists:reverse([{{prefix_modifier, binary_to_integer(Acc)}, Name}|List])}|parse(R, <<>>)].

%% Expand a URI template (after parsing it if necessary).

-spec expand(binary() | uri_template(), variables()) -> iodata().
expand(URITemplate, Vars) when is_binary(URITemplate) ->
	expand(parse(URITemplate), Vars);
expand(URITemplate, Vars) ->
	expand1(URITemplate, Vars).

expand1([], _) ->
	[];
expand1([Literal|Tail], Vars) when is_binary(Literal) ->
	[Literal|expand1(Tail, Vars)];
expand1([{expr, simple_string_expansion, VarList}|Tail], Vars) ->
	[simple_string_expansion(VarList, Vars)|expand1(Tail, Vars)];
expand1([{expr, reserved_expansion, VarList}|Tail], Vars) ->
	[reserved_expansion(VarList, Vars)|expand1(Tail, Vars)];
expand1([{expr, fragment_expansion, VarList}|Tail], Vars) ->
	[fragment_expansion(VarList, Vars)|expand1(Tail, Vars)];
expand1([{expr, label_expansion_with_dot_prefix, VarList}|Tail], Vars) ->
	[label_expansion_with_dot_prefix(VarList, Vars)|expand1(Tail, Vars)];
expand1([{expr, path_segment_expansion, VarList}|Tail], Vars) ->
	[path_segment_expansion(VarList, Vars)|expand1(Tail, Vars)];
expand1([{expr, path_style_parameter_expansion, VarList}|Tail], Vars) ->
	[path_style_parameter_expansion(VarList, Vars)|expand1(Tail, Vars)];
expand1([{expr, form_style_query_expansion, VarList}|Tail], Vars) ->
	[form_style_query_expansion(VarList, Vars)|expand1(Tail, Vars)];
expand1([{expr, form_style_query_continuation, VarList}|Tail], Vars) ->
	[form_style_query_continuation(VarList, Vars)|expand1(Tail, Vars)].

simple_string_expansion(VarList, Vars) ->
	lists:join($,, [
		apply_modifier(Modifier, unreserved, $,, Value)
	|| {Modifier, _Name, Value} <- lookup_variables(VarList, Vars)]).

reserved_expansion(VarList, Vars) ->
	lists:join($,, [
		apply_modifier(Modifier, reserved, $,, Value)
	|| {Modifier, _Name, Value} <- lookup_variables(VarList, Vars)]).

fragment_expansion(VarList, Vars) ->
	case reserved_expansion(VarList, Vars) of
		[] -> [];
		Expanded -> [$#, Expanded]
	end.

label_expansion_with_dot_prefix(VarList, Vars) ->
	segment_expansion(VarList, Vars, $.).

path_segment_expansion(VarList, Vars) ->
	segment_expansion(VarList, Vars, $/).

segment_expansion(VarList, Vars, Sep) ->
	Expanded = lists:join(Sep, [
		apply_modifier(Modifier, unreserved, Sep, Value)
	|| {Modifier, _Name, Value} <- lookup_variables(VarList, Vars)]),
	case Expanded of
		[] -> [];
		[[]] -> [];
		_ -> [Sep, Expanded]
	end.

path_style_parameter_expansion(VarList, Vars) ->
	parameter_expansion(VarList, Vars, $;, $;, trim).

form_style_query_expansion(VarList, Vars) ->
	parameter_expansion(VarList, Vars, $?, $&, no_trim).

form_style_query_continuation(VarList, Vars) ->
	parameter_expansion(VarList, Vars, $&, $&, no_trim).

parameter_expansion(VarList, Vars, LeadingSep, Sep, Trim) ->
	Expanded = lists:join(Sep, [
		apply_parameter_modifier(Modifier, unreserved, Sep, Trim, Name, Value)
	|| {Modifier, Name, Value} <- lookup_variables(VarList, Vars)]),
	case Expanded of
		[] -> [];
		[[]] -> [];
		_ -> [LeadingSep, Expanded]
	end.

lookup_variables([], _) ->
	[];
lookup_variables([{Modifier, Name}|Tail], Vars) ->
	case Vars of
		#{Name := Value} -> [{Modifier, Name, Value}|lookup_variables(Tail, Vars)];
		_ -> lookup_variables(Tail, Vars)
	end.

apply_modifier(no_modifier, AllowedChars, _, List) when is_list(List) ->
	lists:join($,, [urlencode(Value, AllowedChars) || Value <- List]);
apply_modifier(explode_modifier, AllowedChars, ExplodeSep, List) when is_list(List) ->
	lists:join(ExplodeSep, [urlencode(Value, AllowedChars) || Value <- List]);
apply_modifier(Modifier, AllowedChars, ExplodeSep, Map) when is_map(Map) ->
	{JoinSep, KVSep} = case Modifier of
		no_modifier -> {$,, $,};
		explode_modifier -> {ExplodeSep, $=}
	end,
	lists:reverse(lists:join(JoinSep,
		maps:fold(fun(Key, Value, Acc) ->
			[[
				urlencode(Key, AllowedChars),
				KVSep,
				urlencode(Value, AllowedChars)
			]|Acc]
		end, [], Map)
	));
apply_modifier({prefix_modifier, MaxLen}, AllowedChars, _, Value) ->
	urlencode(string:slice(binarize(Value), 0, MaxLen), AllowedChars);
apply_modifier(_, AllowedChars, _, Value) ->
	urlencode(binarize(Value), AllowedChars).

apply_parameter_modifier(_, _, _, _, _, []) ->
	[];
apply_parameter_modifier(_, _, _, _, _, Map) when Map =:= #{} ->
	[];
apply_parameter_modifier(no_modifier, AllowedChars, _, _, Name, List) when is_list(List) ->
	[
		Name,
		$=,
		lists:join($,, [urlencode(Value, AllowedChars) || Value <- List])
	];
apply_parameter_modifier(explode_modifier, AllowedChars, ExplodeSep, _, Name, List) when is_list(List) ->
	lists:join(ExplodeSep, [[
		Name,
		$=,
		urlencode(Value, AllowedChars)
	] || Value <- List]);
apply_parameter_modifier(Modifier, AllowedChars, ExplodeSep, _, Name, Map) when is_map(Map) ->
	{JoinSep, KVSep} = case Modifier of
		no_modifier -> {$,, $,};
		explode_modifier -> {ExplodeSep, $=}
	end,
	[
		case Modifier of
			no_modifier ->
				[
					Name,
					$=
				];
			explode_modifier ->
				[]
		end,
		lists:reverse(lists:join(JoinSep,
			maps:fold(fun(Key, Value, Acc) ->
				[[
					urlencode(Key, AllowedChars),
					KVSep,
					urlencode(Value, AllowedChars)
				]|Acc]
			end, [], Map)
		))
	];
apply_parameter_modifier(Modifier, AllowedChars, _, Trim, Name, Value0) ->
	Value1 = binarize(Value0),
	Value = case Modifier of
		{prefix_modifier, MaxLen} ->
			string:slice(Value1, 0, MaxLen);
		no_modifier ->
			Value1
	end,
	[
		Name,
		case Value of
			<<>> when Trim =:= trim ->
				[];
			<<>> when Trim =:= no_trim ->
				$=;
			_ ->
				[
					$=,
					urlencode(Value, AllowedChars)
				]
		end
	].

binarize(Value) when is_integer(Value) ->
	integer_to_binary(Value);
binarize(Value) when is_float(Value) ->
	float_to_binary(Value, [{decimals, 10}, compact]);
binarize(Value) ->
	Value.

urlencode(Value, unreserved) ->
	urlencode_unreserved(Value, <<>>);
urlencode(Value, reserved) ->
	urlencode_reserved(Value, <<>>).

urlencode_unreserved(<<C,R/bits>>, Acc)
		when ?IS_URI_UNRESERVED(C) ->
	urlencode_unreserved(R, <<Acc/binary,C>>);
urlencode_unreserved(<<C,R/bits>>, Acc) ->
	urlencode_unreserved(R, <<Acc/binary,$%,?HEX(C)>>);
urlencode_unreserved(<<>>, Acc) ->
	Acc.

urlencode_reserved(<<$%,H,L,R/bits>>, Acc)
		when ?IS_HEX(H), ?IS_HEX(L) ->
	urlencode_reserved(R, <<Acc/binary,$%,H,L>>);
urlencode_reserved(<<C,R/bits>>, Acc)
		when ?IS_URI_UNRESERVED(C) or ?IS_URI_GEN_DELIMS(C) or ?IS_URI_SUB_DELIMS(C) ->
	urlencode_reserved(R, <<Acc/binary,C>>);
urlencode_reserved(<<C,R/bits>>, Acc) ->
	urlencode_reserved(R, <<Acc/binary,$%,?HEX(C)>>);
urlencode_reserved(<<>>, Acc) ->
	Acc.

-ifdef(TEST).
expand_uritemplate_test_() ->
	Files = filelib:wildcard("deps/uritemplate-tests/*.json"),
	lists:flatten([begin
		{ok, JSON} = file:read_file(File),
		Tests = jsx:decode(JSON, [return_maps]),
		[begin
			%% Erlang doesn't have a NULL value.
			Vars = maps:remove(<<"undef">>, Vars0),
			[
				{iolist_to_binary(io_lib:format("~s - ~s: ~s => ~s",
					[filename:basename(File), Section, URITemplate,
						if
							is_list(Expected) -> lists:join(<<" OR ">>, Expected);
							true -> Expected
						end
					])),
					fun() ->
						io:format("expected: ~0p", [Expected]),
						case Expected of
							false ->
								{'EXIT', _} = (catch expand(URITemplate, Vars));
							[_|_] ->
								Result = iolist_to_binary(expand(URITemplate, Vars)),
								io:format("~p", [Result]),
								true = lists:member(Result, Expected);
							_ ->
								Expected = iolist_to_binary(expand(URITemplate, Vars))
						end
					end}
			|| [URITemplate, Expected] <- Cases]
		end || {Section, #{
			<<"variables">> := Vars0,
			<<"testcases">> := Cases
		}} <- maps:to_list(Tests)]
	end || File <- Files]).
-endif.
