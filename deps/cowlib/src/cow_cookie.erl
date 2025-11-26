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

-module(cow_cookie).

-export([parse_cookie/1]).
-export([parse_set_cookie/1]).
-export([cookie/1]).
-export([setcookie/3]).

-type cookie_attrs() :: #{
	expires => calendar:datetime(),
	max_age => calendar:datetime(),
	domain => binary(),
	path => binary(),
	secure => true,
	http_only => true,
	same_site => default | none | strict | lax
}.
-export_type([cookie_attrs/0]).

-type cookie_opts() :: #{
	domain => binary(),
	http_only => boolean(),
	max_age => non_neg_integer(),
	path => binary(),
	same_site => default | none | strict | lax,
	secure => boolean()
}.
-export_type([cookie_opts/0]).

-include("cow_inline.hrl").

%% Cookie header.

-spec parse_cookie(binary()) -> [{binary(), binary()}].
parse_cookie(Cookie) ->
	parse_cookie(Cookie, []).

parse_cookie(<<>>, Acc) ->
	lists:reverse(Acc);
parse_cookie(<< $\s, Rest/binary >>, Acc) ->
	parse_cookie(Rest, Acc);
parse_cookie(<< $\t, Rest/binary >>, Acc) ->
	parse_cookie(Rest, Acc);
parse_cookie(<< $,, Rest/binary >>, Acc) ->
	parse_cookie(Rest, Acc);
parse_cookie(<< $;, Rest/binary >>, Acc) ->
	parse_cookie(Rest, Acc);
parse_cookie(Cookie, Acc) ->
	parse_cookie_name(Cookie, Acc, <<>>).

parse_cookie_name(<<>>, Acc, Name) ->
	lists:reverse([{<<>>, parse_cookie_trim(Name)}|Acc]);
parse_cookie_name(<< $=, _/binary >>, _, <<>>) ->
	error(badarg);
parse_cookie_name(<< $=, Rest/binary >>, Acc, Name) ->
	parse_cookie_value(Rest, Acc, Name, <<>>);
parse_cookie_name(<< $,, _/binary >>, _, _) ->
	error(badarg);
parse_cookie_name(<< $;, Rest/binary >>, Acc, Name) ->
	parse_cookie(Rest, [{<<>>, parse_cookie_trim(Name)}|Acc]);
parse_cookie_name(<< $\t, _/binary >>, _, _) ->
	error(badarg);
parse_cookie_name(<< $\r, _/binary >>, _, _) ->
	error(badarg);
parse_cookie_name(<< $\n, _/binary >>, _, _) ->
	error(badarg);
parse_cookie_name(<< $\013, _/binary >>, _, _) ->
	error(badarg);
parse_cookie_name(<< $\014, _/binary >>, _, _) ->
	error(badarg);
parse_cookie_name(<< C, Rest/binary >>, Acc, Name) ->
	parse_cookie_name(Rest, Acc, << Name/binary, C >>).

parse_cookie_value(<<>>, Acc, Name, Value) ->
	lists:reverse([{Name, parse_cookie_trim(Value)}|Acc]);
parse_cookie_value(<< $;, Rest/binary >>, Acc, Name, Value) ->
	parse_cookie(Rest, [{Name, parse_cookie_trim(Value)}|Acc]);
parse_cookie_value(<< $\t, _/binary >>, _, _, _) ->
	error(badarg);
parse_cookie_value(<< $\r, _/binary >>, _, _, _) ->
	error(badarg);
parse_cookie_value(<< $\n, _/binary >>, _, _, _) ->
	error(badarg);
parse_cookie_value(<< $\013, _/binary >>, _, _, _) ->
	error(badarg);
parse_cookie_value(<< $\014, _/binary >>, _, _, _) ->
	error(badarg);
parse_cookie_value(<< C, Rest/binary >>, Acc, Name, Value) ->
	parse_cookie_value(Rest, Acc, Name, << Value/binary, C >>).

parse_cookie_trim(Value = <<>>) ->
	Value;
parse_cookie_trim(Value) ->
	case binary:last(Value) of
		$\s ->
			Size = byte_size(Value) - 1,
			<< Value2:Size/binary, _ >> = Value,
			parse_cookie_trim(Value2);
		_ ->
			Value
	end.

-ifdef(TEST).
parse_cookie_test_() ->
	%% {Value, Result}.
	Tests = [
		{<<"name=value; name2=value2">>, [
			{<<"name">>, <<"value">>},
			{<<"name2">>, <<"value2">>}
		]},
		%% Space in value.
		{<<"foo=Thu Jul 11 2013 15:38:43 GMT+0400 (MSK)">>,
			[{<<"foo">>, <<"Thu Jul 11 2013 15:38:43 GMT+0400 (MSK)">>}]},
		%% Comma in value. Google Analytics sets that kind of cookies.
		{<<"refk=sOUZDzq2w2; sk=B602064E0139D842D620C7569640DBB4C81C45080651"
			"9CC124EF794863E10E80; __utma=64249653.825741573.1380181332.1400"
			"015657.1400019557.703; __utmb=64249653.1.10.1400019557; __utmc="
			"64249653; __utmz=64249653.1400019557.703.13.utmcsr=bluesky.chic"
			"agotribune.com|utmccn=(referral)|utmcmd=referral|utmcct=/origin"
			"als/chi-12-indispensable-digital-tools-bsi,0,0.storygallery">>, [
				{<<"refk">>, <<"sOUZDzq2w2">>},
				{<<"sk">>, <<"B602064E0139D842D620C7569640DBB4C81C45080651"
					"9CC124EF794863E10E80">>},
				{<<"__utma">>, <<"64249653.825741573.1380181332.1400"
					"015657.1400019557.703">>},
				{<<"__utmb">>, <<"64249653.1.10.1400019557">>},
				{<<"__utmc">>, <<"64249653">>},
				{<<"__utmz">>, <<"64249653.1400019557.703.13.utmcsr=bluesky.chic"
					"agotribune.com|utmccn=(referral)|utmcmd=referral|utmcct=/origin"
					"als/chi-12-indispensable-digital-tools-bsi,0,0.storygallery">>}
		]},
		%% Potential edge cases (initially from Mochiweb).
		{<<"foo=\\x">>, [{<<"foo">>, <<"\\x">>}]},
		{<<"foo=;bar=">>, [{<<"foo">>, <<>>}, {<<"bar">>, <<>>}]},
		{<<"foo=\\\";;bar=good ">>,
			[{<<"foo">>, <<"\\\"">>}, {<<"bar">>, <<"good">>}]},
		{<<"foo=\"\\\";bar=good">>,
			[{<<"foo">>, <<"\"\\\"">>}, {<<"bar">>, <<"good">>}]},
		{<<>>, []}, %% Flash player.
		{<<"foo=bar , baz=wibble ">>, [{<<"foo">>, <<"bar , baz=wibble">>}]},
		%% Technically invalid, but seen in the wild
		{<<"foo">>, [{<<>>, <<"foo">>}]},
		{<<"foo ">>, [{<<>>, <<"foo">>}]},
		{<<"foo;">>, [{<<>>, <<"foo">>}]},
		{<<"bar;foo=1">>, [{<<>>, <<"bar">>}, {<<"foo">>, <<"1">>}]}
	],
	[{V, fun() -> R = parse_cookie(V) end} || {V, R} <- Tests].

parse_cookie_error_test_() ->
	%% Value.
	Tests = [
		<<"=">>
	],
	[{V, fun() -> {'EXIT', {badarg, _}} = (catch parse_cookie(V)) end} || V <- Tests].
-endif.

%% Set-Cookie header.

-spec parse_set_cookie(binary())
	-> {ok, binary(), binary(), cookie_attrs()}
	| ignore.
parse_set_cookie(SetCookie) ->
	case has_non_ws_ctl(SetCookie) of
		true ->
			ignore;
		false ->
			{NameValuePair, UnparsedAttrs} = take_until_semicolon(SetCookie, <<>>),
			{Name, Value} = case binary:split(NameValuePair, <<$=>>) of
				[Value0] -> {<<>>, trim(Value0)};
				[Name0, Value0] -> {trim(Name0), trim(Value0)}
			end,
			case {Name, Value} of
				{<<>>, <<>>} ->
					ignore;
				_ ->
					Attrs = parse_set_cookie_attrs(UnparsedAttrs, #{}),
					{ok, Name, Value, Attrs}
			end
	end.

has_non_ws_ctl(<<>>) ->
	false;
has_non_ws_ctl(<<C,R/bits>>) ->
	if
		C =< 16#08 -> true;
		C >= 16#0A, C =< 16#1F -> true;
		C =:= 16#7F -> true;
		true -> has_non_ws_ctl(R)
	end.

parse_set_cookie_attrs(<<>>, Attrs) ->
	Attrs;
parse_set_cookie_attrs(<<$;,Rest0/bits>>, Attrs) ->
	{Av, Rest} = take_until_semicolon(Rest0, <<>>),
	{Name, Value} = case binary:split(Av, <<$=>>) of
		[Name0] -> {trim(Name0), <<>>};
		[Name0, Value0] -> {trim(Name0), trim(Value0)}
	end,
	if
		byte_size(Value) > 1024 ->
			parse_set_cookie_attrs(Rest, Attrs);
		true ->
			case parse_set_cookie_attr(?LOWER(Name), Value) of
				{ok, AttrName, AttrValue} ->
					parse_set_cookie_attrs(Rest, Attrs#{AttrName => AttrValue});
				{ignore, AttrName} ->
					parse_set_cookie_attrs(Rest, maps:remove(AttrName, Attrs));
				ignore ->
					parse_set_cookie_attrs(Rest, Attrs)
			end
	end.

take_until_semicolon(Rest = <<$;,_/bits>>, Acc) -> {Acc, Rest};
take_until_semicolon(<<C,R/bits>>, Acc) -> take_until_semicolon(R, <<Acc/binary,C>>);
take_until_semicolon(<<>>, Acc) -> {Acc, <<>>}.

trim(String) ->
	string:trim(String, both, [$\s, $\t]).

parse_set_cookie_attr(<<"expires">>, Value) ->
	try cow_date:parse_date(Value) of
		DateTime ->
			{ok, expires, DateTime}
	catch _:_ ->
		ignore
	end;
parse_set_cookie_attr(<<"max-age">>, Value) ->
	try binary_to_integer(Value) of
		MaxAge when MaxAge =< 0 ->
			%% Year 0 corresponds to 1 BC.
			{ok, max_age, {{0, 1, 1}, {0, 0, 0}}};
		MaxAge ->
			CurrentTime = erlang:universaltime(),
			{ok, max_age, calendar:gregorian_seconds_to_datetime(
				calendar:datetime_to_gregorian_seconds(CurrentTime) + MaxAge)}
	catch _:_ ->
		ignore
	end;
parse_set_cookie_attr(<<"domain">>, Value) ->
	case Value of
		<<>> ->
			ignore;
		<<".",Rest/bits>> ->
			{ok, domain, ?LOWER(Rest)};
		_ ->
			{ok, domain, ?LOWER(Value)}
	end;
parse_set_cookie_attr(<<"path">>, Value) ->
	case Value of
		<<"/",_/bits>> ->
			{ok, path, Value};
		%% When the path is not absolute, or the path is empty, the default-path will be used.
		%% Note that the default-path is also used when there are no path attributes,
		%% so we are simply ignoring the attribute here.
		_ ->
			{ignore, path}
	end;
parse_set_cookie_attr(<<"secure">>, _) ->
	{ok, secure, true};
parse_set_cookie_attr(<<"httponly">>, _) ->
	{ok, http_only, true};
parse_set_cookie_attr(<<"samesite">>, Value) ->
	case ?LOWER(Value) of
		<<"none">> ->
			{ok, same_site, none};
		<<"strict">> ->
			{ok, same_site, strict};
		<<"lax">> ->
			{ok, same_site, lax};
		%% Unknown values and lack of value are equivalent.
		_ ->
			{ok, same_site, default}
	end;
parse_set_cookie_attr(_, _) ->
	ignore.

-ifdef(TEST).
parse_set_cookie_test_() ->
	Tests = [
		{<<"a=b">>, {ok, <<"a">>, <<"b">>, #{}}},
		{<<"a=b; Secure">>, {ok, <<"a">>, <<"b">>, #{secure => true}}},
		{<<"a=b; HttpOnly">>, {ok, <<"a">>, <<"b">>, #{http_only => true}}},
		{<<"a=b; Expires=Wed, 21 Oct 2015 07:28:00 GMT; Expires=Wed, 21 Oct 2015 07:29:00 GMT">>,
			{ok, <<"a">>, <<"b">>, #{expires => {{2015,10,21},{7,29,0}}}}},
		{<<"a=b; Max-Age=999; Max-Age=0">>,
			{ok, <<"a">>, <<"b">>, #{max_age => {{0,1,1},{0,0,0}}}}},
		{<<"a=b; Domain=example.org; Domain=foo.example.org">>,
			{ok, <<"a">>, <<"b">>, #{domain => <<"foo.example.org">>}}},
		{<<"a=b; Path=/path/to/resource; Path=/">>,
			{ok, <<"a">>, <<"b">>, #{path => <<"/">>}}},
		{<<"a=b; SameSite=UnknownValue">>, {ok, <<"a">>, <<"b">>, #{same_site => default}}},
		{<<"a=b; SameSite=None">>, {ok, <<"a">>, <<"b">>, #{same_site => none}}},
		{<<"a=b; SameSite=Lax">>, {ok, <<"a">>, <<"b">>, #{same_site => lax}}},
		{<<"a=b; SameSite=Strict">>, {ok, <<"a">>, <<"b">>, #{same_site => strict}}},
		{<<"a=b; SameSite=Lax; SameSite=Strict">>,
			{ok, <<"a">>, <<"b">>, #{same_site => strict}}}
	],
	[{SetCookie, fun() -> Res = parse_set_cookie(SetCookie) end}
		|| {SetCookie, Res} <- Tests].
-endif.

%% Build a cookie header.

-spec cookie([{iodata(), iodata()}]) -> iolist().
cookie([]) ->
	[];
cookie([{<<>>, Value}]) ->
	[Value];
cookie([{Name, Value}]) ->
	[Name, $=, Value];
cookie([{<<>>, Value}|Tail]) ->
	[Value, $;, $\s|cookie(Tail)];
cookie([{Name, Value}|Tail]) ->
	[Name, $=, Value, $;, $\s|cookie(Tail)].

-ifdef(TEST).
cookie_test_() ->
	Tests = [
		{[], <<>>},
		{[{<<"a">>, <<"b">>}], <<"a=b">>},
		{[{<<"a">>, <<"b">>}, {<<"c">>, <<"d">>}], <<"a=b; c=d">>},
		{[{<<>>, <<"b">>}, {<<"c">>, <<"d">>}], <<"b; c=d">>},
		{[{<<"a">>, <<"b">>}, {<<>>, <<"d">>}], <<"a=b; d">>}
	],
	[{Res, fun() -> Res = iolist_to_binary(cookie(Cookies)) end}
		|| {Cookies, Res} <- Tests].
-endif.

%% Convert a cookie name, value and options to its iodata form.
%%
%% Initially from Mochiweb:
%%   * Copyright 2007 Mochi Media, Inc.
%% Initial binary implementation:
%%   * Copyright 2011 Thomas Burdick <thomas.burdick@gmail.com>
%%
%% @todo Rename the function to set_cookie eventually.

-spec setcookie(iodata(), iodata(), cookie_opts()) -> iolist().
setcookie(Name, Value, Opts) ->
	nomatch = binary:match(iolist_to_binary(Name), [<<$=>>, <<$,>>, <<$;>>,
			<<$\s>>, <<$\t>>, <<$\r>>, <<$\n>>, <<$\013>>, <<$\014>>]),
	nomatch = binary:match(iolist_to_binary(Value), [<<$,>>, <<$;>>,
			<<$\s>>, <<$\t>>, <<$\r>>, <<$\n>>, <<$\013>>, <<$\014>>]),
	[Name, <<"=">>, Value, attributes(maps:to_list(Opts))].

attributes([]) -> [];
attributes([{domain, Domain}|Tail]) -> [<<"; Domain=">>, Domain|attributes(Tail)];
attributes([{http_only, false}|Tail]) -> attributes(Tail);
attributes([{http_only, true}|Tail]) -> [<<"; HttpOnly">>|attributes(Tail)];
%% MSIE requires an Expires date in the past to delete a cookie.
attributes([{max_age, 0}|Tail]) ->
	[<<"; Expires=Thu, 01-Jan-1970 00:00:01 GMT; Max-Age=0">>|attributes(Tail)];
attributes([{max_age, MaxAge}|Tail]) when is_integer(MaxAge), MaxAge > 0 ->
	Secs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	Expires = cow_date:rfc2109(calendar:gregorian_seconds_to_datetime(Secs + MaxAge)),
	[<<"; Expires=">>, Expires, <<"; Max-Age=">>, integer_to_list(MaxAge)|attributes(Tail)];
attributes([Opt={max_age, _}|_]) ->
	error({badarg, Opt});
attributes([{path, Path}|Tail]) -> [<<"; Path=">>, Path|attributes(Tail)];
attributes([{secure, false}|Tail]) -> attributes(Tail);
attributes([{secure, true}|Tail]) -> [<<"; Secure">>|attributes(Tail)];
attributes([{same_site, default}|Tail]) -> attributes(Tail);
attributes([{same_site, none}|Tail]) -> [<<"; SameSite=None">>|attributes(Tail)];
attributes([{same_site, lax}|Tail]) -> [<<"; SameSite=Lax">>|attributes(Tail)];
attributes([{same_site, strict}|Tail]) -> [<<"; SameSite=Strict">>|attributes(Tail)];
%% Skip unknown options.
attributes([_|Tail]) -> attributes(Tail).

-ifdef(TEST).
setcookie_test_() ->
	%% {Name, Value, Opts, Result}
	Tests = [
		{<<"Customer">>, <<"WILE_E_COYOTE">>,
			#{http_only => true, domain => <<"acme.com">>},
			<<"Customer=WILE_E_COYOTE; "
				"Domain=acme.com; HttpOnly">>},
		{<<"Customer">>, <<"WILE_E_COYOTE">>,
			#{path => <<"/acme">>},
			<<"Customer=WILE_E_COYOTE; Path=/acme">>},
		{<<"Customer">>, <<"WILE_E_COYOTE">>,
			#{secure => true},
			<<"Customer=WILE_E_COYOTE; Secure">>},
		{<<"Customer">>, <<"WILE_E_COYOTE">>,
			#{secure => false, http_only => false},
			<<"Customer=WILE_E_COYOTE">>},
		{<<"Customer">>, <<"WILE_E_COYOTE">>,
			#{same_site => default},
			<<"Customer=WILE_E_COYOTE">>},
		{<<"Customer">>, <<"WILE_E_COYOTE">>,
			#{same_site => none},
			<<"Customer=WILE_E_COYOTE; SameSite=None">>},
		{<<"Customer">>, <<"WILE_E_COYOTE">>,
			#{same_site => lax},
			<<"Customer=WILE_E_COYOTE; SameSite=Lax">>},
		{<<"Customer">>, <<"WILE_E_COYOTE">>,
			#{same_site => strict},
			<<"Customer=WILE_E_COYOTE; SameSite=Strict">>},
		{<<"Customer">>, <<"WILE_E_COYOTE">>,
			#{path => <<"/acme">>, badoption => <<"negatory">>},
			<<"Customer=WILE_E_COYOTE; Path=/acme">>}
	],
	[{R, fun() -> R = iolist_to_binary(setcookie(N, V, O)) end}
		|| {N, V, O, R} <- Tests].

setcookie_max_age_test() ->
	F = fun(N, V, O) ->
		binary:split(iolist_to_binary(
			setcookie(N, V, O)), <<";">>, [global])
	end,
	[<<"Customer=WILE_E_COYOTE">>,
		<<" Expires=", _/binary>>,
		<<" Max-Age=111">>,
		<<" Secure">>] = F(<<"Customer">>, <<"WILE_E_COYOTE">>,
			#{max_age => 111, secure => true}),
	case catch F(<<"Customer">>, <<"WILE_E_COYOTE">>, #{max_age => -111}) of
		{'EXIT', {{badarg, {max_age, -111}}, _}} -> ok
	end,
	[<<"Customer=WILE_E_COYOTE">>,
		<<" Expires=", _/binary>>,
		<<" Max-Age=86417">>] = F(<<"Customer">>, <<"WILE_E_COYOTE">>,
			 #{max_age => 86417}),
	ok.

setcookie_failures_test_() ->
	F = fun(N, V) ->
		try setcookie(N, V, #{}) of
			_ ->
				false
		catch _:_ ->
			true
		end
	end,
	Tests = [
		{<<"Na=me">>, <<"Value">>},
		{<<"Name;">>, <<"Value">>},
		{<<"\r\name">>, <<"Value">>},
		{<<"Name">>, <<"Value;">>},
		{<<"Name">>, <<"\value">>}
	],
	[{iolist_to_binary(io_lib:format("{~p, ~p} failure", [N, V])),
		fun() -> true = F(N, V) end}
		|| {N, V} <- Tests].
-endif.
