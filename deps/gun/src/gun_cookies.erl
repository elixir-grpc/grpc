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

-module(gun_cookies).

-export([add_cookie_header/5]).
-export([domain_match/2]).
-export([gc/1]).
-export([path_match/2]).
-export([query/2]).
-export([session_gc/1]).
-export([set_cookie/5]).
-export([set_cookie_header/7]).

-ifdef(TEST).
-export([wpt_http_state_test_files/1]). %% Also used in rfc6265bis_SUITE.
-endif.

-type store_state() :: any().

-type store() :: {module(), store_state()}.
-export_type([store/0]).

-type cookie() :: #{
	name := binary(),
	value := binary(),
	domain := binary(),
	path := binary(),
	creation_time := calendar:datetime(),
	last_access_time := calendar:datetime(),
	expiry_time := calendar:datetime() | infinity,
	persistent := boolean(),
	host_only := boolean(),
	secure_only := boolean(),
	http_only := boolean(),
	same_site := default | none | strict | lax
}.
-export_type([cookie/0]).

-callback init(any()) -> store().

-callback query(State, uri_string:uri_map())
	-> {ok, [gun_cookies:cookie()], State}
	when State::store_state().

-callback set_cookie_secure_match(store_state(), #{
	name := binary(),
%	secure_only := true,
	domain := binary(),
	path := binary()
}) -> match | nomatch.

-callback set_cookie_get_exact_match(State, #{
	name := binary(),
	domain := binary(),
	host_only := boolean(),
	path := binary()
}) -> {ok, cookie(), State} | error
	when State::store_state().

-callback store(State, cookie())
	-> {ok, State} | {error, any()}
	when State::store_state().

-callback gc(State)
	-> {ok, State}
	when State::store_state().

-callback session_gc(State)
	-> {ok, State}
	when State::store_state().

-spec add_cookie_header(binary(), iodata(), iodata(), Headers, Store)
	-> {Headers, Store} when Headers :: [{binary(), iodata()}], Store :: undefined | store().
add_cookie_header(_, _, _, Headers, Store=undefined) ->
	{Headers, Store};
add_cookie_header(Scheme, Authority, PathWithQs, Headers0, Store0) ->
	#{
		host := Host,
		path := Path
	} = uri_string:parse([Scheme, <<"://">>, Authority, PathWithQs]),
	URIMap = uri_string:normalize(#{
		scheme => Scheme,
		host => iolist_to_binary(Host),
		path => iolist_to_binary(Path)
	}, [return_map]),
	{ok, Cookies0, Store} = query(Store0, URIMap),
	Headers = case Cookies0 of
		[] ->
			Headers0;
		_ ->
			Cookies = [{Name, Value} || #{name := Name, value := Value} <- Cookies0],
			%% We put cookies at the end of the headers list as it's the least important header.
			Headers0 ++ [{<<"cookie">>, cow_cookie:cookie(Cookies)}]
	end,
	{Headers, Store}.

-spec domain_match(binary(), binary()) -> boolean().
domain_match(String, String) ->
	true;
domain_match(String, DomainString) ->
	SkipLen = byte_size(String) - byte_size(DomainString) - 1,
	case String of
		<<_:SkipLen/unit:8, $., DomainString/binary>> ->
			case inet:parse_strict_address(binary_to_list(String)) of
				{ok, _} ->
					false;
				{error, einval} ->
					true
			end;
		_ ->
			false
	end.

-spec gc(Store) -> {ok, Store} when Store::store().
gc({Mod, State0}) ->
	{ok, State} = Mod:gc(State0),
	{ok, {Mod, State}}.

-spec path_match(binary(), binary()) -> boolean().
path_match(Path, Path) ->
	true;
path_match(ReqPath, CookiePath) ->
	Len = byte_size(CookiePath),
	CookieLast = binary:last(CookiePath),
	case ReqPath of
		<<CookiePath:Len/binary, _/bits>> when CookieLast =:= $/ ->
			true;
		<<CookiePath:Len/binary, $/, _/bits>> ->
			true;
		_ ->
			false
	end.

-ifdef(TEST).
path_match_test_() ->
	Tests = [
		{<<"/">>, <<"/">>, true},
		{<<"/path/to/resource">>, <<"/path/to/resource">>, true},
		{<<"/path/">>, <<"/path/">>, true},
		{<<"/path/to/resource">>, <<"/path/">>, true},
		{<<"/path/to/resource">>, <<"/path">>, true},
		{<<"/path/to/resource">>, <<"/path/to/">>, true},
		{<<"/path/to/resource">>, <<"/path/to">>, true},
		{<<"/path/to/resource">>, <<"/pa">>, false},
		{<<"/path/to/resource">>, <<"/pat">>, false},
		{<<"/path/to/resource">>, <<"/path/to/r">>, false},
		{<<"/abc">>, <<"/def">>, false}
	],
	[{iolist_to_binary(io_lib:format("(~p,~p)", [PA, PB])),
		fun() -> Res = path_match(PA, PB) end}
	|| {PA, PB, Res} <- Tests].
-endif.

%% @todo The given URI must be normalized.
-spec query(Store, uri_string:uri_map())
	-> {ok, [cookie()], Store}
	when Store::store().
query({Mod, State0}, URI) ->
	{ok, Cookies0, State} = Mod:query(State0, URI),
	Cookies = lists:sort(fun
		(#{path := P, creation_time := CTA}, #{path := P, creation_time := CTB}) ->
			CTA =< CTB;
		(#{path := PA}, #{path := PB}) ->
			PA > PB
	end, Cookies0),
	{ok, Cookies, {Mod, State}}.

-spec session_gc(Store) -> {ok, Store} when Store::store().
session_gc({Mod, State0}) ->
	{ok, State} = Mod:session_gc(State0),
	{ok, {Mod, State}}.

%% @todo The given URI must be normalized.
-spec set_cookie(Store, uri_string:uri_map(), binary(), binary(), cow_cookie:cookie_attrs())
	-> {ok, Store} | {error, any()} when Store::store().
set_cookie(_, _, Name, Value, _) when byte_size(Name) + byte_size(Value) > 4096 ->
	{error, larger_than_4096_bytes};
set_cookie(Store, URI=#{host := Host}, Name, Value, Attrs) ->
	%% This is where we would add a feature to block cookies (like a blacklist).
	CurrentTime = erlang:universaltime(),
	Cookie0 = #{
		name => Name,
		value => Value,
		creation_time => CurrentTime,
		last_access_time => CurrentTime
	},
	Cookie = case Attrs of
		#{max_age := ExpiryTime} ->
			Cookie0#{
				persistent => true,
				expiry_time => ExpiryTime
			};
		#{expires := ExpiryTime} ->
			Cookie0#{
				persistent => true,
				expiry_time => ExpiryTime
			};
		_ ->
			Cookie0#{
				persistent => false,
				expiry_time => infinity
			}
	end,
	Domain0 = maps:get(domain, Attrs, <<>>),
	Domain = case gun_public_suffix:match(Domain0) of
		false ->
			Domain0;
		true when Host =:= Domain0 ->
			<<>>;
		true ->
			{error, domain_is_public_suffix}
	end,
	case Domain of
		<<>> ->
			set_cookie(Store, URI, Attrs, Cookie#{
				host_only => true,
				domain => Host
			});
		Error = {error, _} ->
			Error;
		_ ->
			case domain_match(Host, Domain) of
				true ->
					set_cookie(Store, URI, Attrs, Cookie#{
						host_only => false,
						domain => Domain
					});
				false ->
					{error, domain_match_failed}
			end
	end.

set_cookie(Store, URI, Attrs, Cookie0) ->
	Cookie1 = case Attrs of
		#{path := Path} ->
			Cookie0#{path => Path};
		_ ->
			Cookie0#{path => default_path(URI)}
	end,
	SecureOnly = maps:get(secure, Attrs, false),
	case {SecureOnly, maps:get(scheme, URI)} of
		{true, <<"http">>} ->
			{error, secure_scheme_only};
		_ ->
			Cookie = Cookie1#{
				secure_only => SecureOnly,
				http_only => maps:get(http_only, Attrs, false)
			},
			%% This is where we would drop cookies from non-HTTP APIs.
			set_cookie1(Store, URI, Attrs, Cookie)
	end.

default_path(#{path := Path}) ->
	case string:split(Path, <<"/">>, trailing) of
		[_] -> <<"/">>;
		[<<>>, _] -> <<"/">>;
		[DefaultPath, _] -> DefaultPath
	end;
default_path(_) ->
	<<"/">>.

set_cookie1(Store, URI=#{scheme := <<"http">>}, Attrs, Cookie=#{secure_only := false}) ->
	Match = maps:with([name, domain, path], Cookie),
	case set_cookie_secure_match(Store, Match) of
		match ->
			{error, secure_cookie_matches};
		nomatch ->
			set_cookie2(Store, URI, Attrs, Cookie)
	end;
set_cookie1(Store, URI, Attrs, Cookie) ->
	set_cookie2(Store, URI, Attrs, Cookie).

set_cookie_secure_match({Mod, State}, Match) ->
	Mod:set_cookie_secure_match(State, Match).

set_cookie2(Store, _URI, Attrs, Cookie0) ->
	Cookie = Cookie0#{same_site => maps:get(same_site, Attrs, default)},
	%% This is where we would perform the same-site checks.
	%%
	%% It seems that an option would need to be added to Gun
	%% in order to define the "site for cookies" value. It is
	%% not the same as the site identified by the URI. Although
	%% I do wonder if in the case of server push we may consider
	%% the requested URI to be the "site for cookies", at least
	%% by default.
	%%
	%% The URI argument will be used if/when the above gets
	%% implemented.
	set_cookie3(Store, Attrs, Cookie).

set_cookie3(Store, Attrs, Cookie=#{name := Name,
		host_only := HostOnly, secure_only := SecureOnly}) ->
	Path = maps:get(path, Attrs, undefined),
	case Name of
		<<"__Secure-",_/bits>> when not SecureOnly ->
			{error, name_prefix_secure_requires_secure_only};
		<<"__Host-",_/bits>> when not SecureOnly ->
			{error, name_prefix_host_requires_secure_only};
		<<"__Host-",_/bits>> when not HostOnly ->
			{error, name_prefix_host_requires_host_only};
		<<"__Host-",_/bits>> when Path =/= <<"/">> ->
			{error, name_prefix_host_requires_top_level_path};
		_ ->
			set_cookie_store(Store, Cookie)
	end.

set_cookie_store(Store0, Cookie) ->
	Match = maps:with([name, domain, host_only, path], Cookie),
	case set_cookie_get_exact_match(Store0, Match) of
		{ok, #{creation_time := CreationTime}, Store} ->
			%% This is where we would reject a new non-HTTP cookie
			%% if the OldCookie has http_only set to true.
			store(Store, Cookie#{creation_time => CreationTime});
		error ->
			store(Store0, Cookie)
	end.

set_cookie_get_exact_match({Mod, State0}, Match) ->
	case Mod:set_cookie_get_exact_match(State0, Match) of
		{ok, Cookie, State} ->
			{ok, Cookie, {Mod, State}};
		Error ->
			Error
	end.

store({Mod, State0}, Cookie) ->
	case Mod:store(State0, Cookie) of
		{ok, State} ->
			{ok, {Mod, State}};
		%% @todo Is this return value useful? Can't it just return {ok, State}?
		Error ->
			Error
	end.

-spec set_cookie_header(binary(), iodata(), iodata(), cow_http:status(),
	Headers, Store, #{cookie_ignore_informational := boolean()})
	-> {Headers, Store} when Headers :: [{binary(), iodata()}], Store :: undefined | store().
%% Don't set cookies when cookie store isn't configured.
set_cookie_header(_, _, _, _, _, Store=undefined, _) ->
	Store;
%% Ignore cookies set on informational responses when configured to do so.
%% This includes cookies set to Websocket upgrade responses!
set_cookie_header(_, _, _, Status, _, Store, #{cookie_ignore_informational := true})
		when Status >= 100, Status =< 199 ->
	Store;
set_cookie_header(Scheme, Authority, PathWithQs, _, Headers, Store0, _) ->
	#{host := Host, path := Path} = uri_string:parse([Scheme, <<"://">>, Authority, PathWithQs]),
	URIMap = uri_string:normalize(#{
		scheme => Scheme,
		host => iolist_to_binary(Host),
		path => iolist_to_binary(Path)
	}, [return_map]),
	SetCookies = [SC || {<<"set-cookie">>, SC} <- Headers],
	lists:foldl(fun(SC, Store1) ->
		case cow_cookie:parse_set_cookie(SC) of
			{ok, N, V, A} ->
				case set_cookie(Store1, URIMap, N, V, A) of
					{ok, Store} -> Store;
					{error, _} -> Store1
				end;
			ignore ->
				Store1
		end
	end, Store0, SetCookies).

-ifdef(TEST).
gc_test() ->
	URIMap = #{scheme => <<"http">>, host => <<"example.org">>, path => <<"/path/to/resource">>},
	Store0 = gun_cookies_list:init(),
	%% Add a cookie that expires in 2 seconds. GC. Cookie can be retrieved.
	{ok, N0, V0, A0} = cow_cookie:parse_set_cookie(<<"a=b; Path=/; Max-Age=2">>),
	{ok, Store1} = set_cookie(Store0, URIMap, N0, V0, A0),
	{ok, Store2} = gc(Store1),
	{ok, [_], _} = query(Store2, URIMap),
	%% Wait 3 seconds. GC. Cookie was removed.
	timer:sleep(3000),
	{ok, Store} = gc(Store2),
	{ok, [], _} = query(Store, URIMap),
	ok.

gc_expiry_time_infinity_test() ->
	URIMap = #{scheme => <<"http">>, host => <<"example.org">>, path => <<"/path/to/resource">>},
	Store0 = gun_cookies_list:init(),
	%% Add a session cookie. GC. Cookie can be retrieved.
	{ok, N0, V0, A0} = cow_cookie:parse_set_cookie(<<"a=b; Path=/">>),
	{ok, Store1} = set_cookie(Store0, URIMap, N0, V0, A0),
	{ok, Store} = gc(Store1),
	{ok, [_], _} = query(Store, URIMap),
	ok.

session_gc_test() ->
	URIMap = #{scheme => <<"http">>, host => <<"example.org">>, path => <<"/path/to/resource">>},
	Store0 = gun_cookies_list:init(),
	%% Add a persistent and a session cookie. GC session cookies. Only persistent can be retrieved.
	{ok, N0, V0, A0} = cow_cookie:parse_set_cookie(<<"s=s; Path=/">>),
	{ok, Store1} = set_cookie(Store0, URIMap, N0, V0, A0),
	{ok, N1, V1, A1} = cow_cookie:parse_set_cookie(<<"p=p; Path=/; Max-Age=999">>),
	{ok, Store2} = set_cookie(Store1, URIMap, N1, V1, A1),
	{ok, Store} = session_gc(Store2),
	{ok, [#{name := <<"p">>}], _} = query(Store, URIMap),
	ok.

%% Most of the tests for this module are converted from the
%% Web platform test suite. At the time of writing they could
%% be found at https://github.com/web-platform-tests/wpt/tree/master/cookies
%%
%% Some of the tests use files from wpt directly, namely the
%% http-state ones. They are copied to the test/wpt/cookies directory.

-define(HOST, "web-platform.test").

%% WPT: domain/domain-attribute-host-with-and-without-leading-period
%% WPT: domain/domain-attribute-host-with-leading-period
wpt_domain_with_and_without_leading_period_test() ->
	URIMap = #{scheme => <<"http">>, host => <<?HOST>>, path => <<"/path/to/resource">>},
	Store0 = gun_cookies_list:init(),
	%% Add a cookie with a leading period in the domain. Cookie can be retrieved.
	{ok, N0, V0, A0} = cow_cookie:parse_set_cookie(<<"a=b; Path=/; Domain=." ?HOST>>),
	{ok, Store1} = set_cookie(Store0, URIMap, N0, V0, A0),
	{ok, [#{value := <<"b">>}], _} = query(Store1, URIMap),
	{ok, [#{value := <<"b">>}], _} = query(Store1, URIMap#{host => <<"sub." ?HOST>>}),
	%% Add a cookie without a leading period in the domain. Overrides the existing cookie.
	{ok, N1, V1, A1} = cow_cookie:parse_set_cookie(<<"a=c; Path=/; Domain=" ?HOST>>),
	{ok, Store} = set_cookie(Store1, URIMap, N1, V1, A1),
	{ok, [#{value := <<"c">>}], _} = query(Store, URIMap),
	{ok, [#{value := <<"c">>}], _} = query(Store, URIMap#{host => <<"sub." ?HOST>>}),
	ok.

%% WPT: domain/domain-attribute-matches-host
wpt_domain_matches_host_test() ->
	URIMap = #{scheme => <<"http">>, host => <<?HOST>>, path => <<"/path/to/resource">>},
	Store0 = gun_cookies_list:init(),
	%% Add a cookie without a leading period in the domain. Cookie can be retrieved.
	{ok, N1, V1, A1} = cow_cookie:parse_set_cookie(<<"a=c; Path=/; Domain=" ?HOST>>),
	{ok, Store} = set_cookie(Store0, URIMap, N1, V1, A1),
	{ok, [#{value := <<"c">>}], _} = query(Store, URIMap),
	{ok, [#{value := <<"c">>}], _} = query(Store, URIMap#{host => <<"sub." ?HOST>>}),
	ok.

%% WPT: domain/domain-attribute-missing
wpt_domain_missing_test() ->
	URIMap = #{scheme => <<"http">>, host => <<?HOST>>, path => <<"/path/to/resource">>},
	Store0 = gun_cookies_list:init(),
	%% Add a cookie without a domain attribute. Cookie is not sent on subdomains.
	{ok, N1, V1, A1} = cow_cookie:parse_set_cookie(<<"a=c; Path=/">>),
	{ok, Store} = set_cookie(Store0, URIMap, N1, V1, A1),
	{ok, [#{value := <<"c">>}], _} = query(Store, URIMap),
	{ok, [], _} = query(Store, URIMap#{host => <<"sub." ?HOST>>}),
	ok.

%% WPT: http-state/*-tests
wpt_http_state_test_files() ->
	wpt_http_state_test_files("test/").

wpt_http_state_test_files(TestPath) ->
	filelib:wildcard(TestPath ++ "wpt/cookies/*-test") -- [
		TestPath ++ "wpt/cookies/attribute0023-test", %% Doesn't match the spec (path override).
		TestPath ++ "wpt/cookies/disabled-chromium0020-test", %% Maximum cookie name of 4096 characters.
		TestPath ++ "wpt/cookies/disabled-chromium0022-test" %% Nonsense.
	].

wpt_http_state_test_() ->
	URIMap0 = #{scheme => <<"http">>, host => <<"home.example.org">>, path => <<"/cookie-parser">>},
	TestFiles = wpt_http_state_test_files(),
	[{F, fun() ->
		{ok, Test} = file:read_file(F),
		%% We don't want the final empty line.
		Lines = lists:reverse(tl(lists:reverse(string:split(Test, <<"\n">>, all)))),
		{Store, URIMap2} = lists:foldl(fun
			(<<"Set-Cookie: ",SetCookie/bits>>, Acc={Store0, URIMap1}) ->
				case cow_cookie:parse_set_cookie(SetCookie) of
					{ok, N, V, A} ->
						%% We use the URIMap that corresponds to the request.
						case set_cookie(Store0, URIMap0, N, V, A) of
							{ok, Store1} -> {Store1, URIMap1};
							{error, _} -> Acc
						end;
					ignore ->
						Acc
				end;
			(<<"Set-Cookie:">>, Acc) ->
				Acc;
			(<<"Location: ",Location/bits>>, {Store0, URIMap1}) ->
				{Store0, maps:merge(URIMap1, uri_string:normalize(Location, [return_map]))}
		end, {gun_cookies_list:init(), URIMap0}, Lines),
		%% We must change the URI if it wasn't already changed by the test.
		URIMap = case URIMap2 of
			URIMap0 -> maps:merge(URIMap0, uri_string:normalize(<<"/cookie-parser-result">>, [return_map]));
			_ -> URIMap2
		end,
		{ok, Cookies, _} = query(Store, URIMap),
		case file:read_file(iolist_to_binary(string:replace(F, <<"-test">>, <<"-expected">>))) of
			{ok, ExpectedFile} when ExpectedFile =:= <<>>; ExpectedFile =:= <<"\n">> ->
				[] = Cookies,
				ok;
			{ok, <<"Cookie: ",CookiesBin0/bits>>} ->
				%% We only care about the first line.
				[CookiesBin, <<>>|_] = string:split(CookiesBin0, <<"\n">>, all),
				CookiesBin = iolist_to_binary(cow_cookie:cookie(
					[{Name, Value} || #{name := Name, value := Value} <- Cookies])),
				ok
		end
	end} || F <- TestFiles].

%% WPT: path/default
wpt_path_default_test() ->
	URIMap = #{scheme => <<"http">>, host => <<?HOST>>, path => <<"/path/to/resource">>},
	Store0 = gun_cookies_list:init(),
	%% Add a cookie without a path attribute.
	{ok, N1, V1, A1} = cow_cookie:parse_set_cookie(<<"cookies-path-default=1">>),
	{ok, Store1} = set_cookie(Store0, URIMap, N1, V1, A1),
	%% Confirm the cookie was stored with the proper default path,
	%% and gets sent for the same path, other resources at the same level or child paths.
	{ok, [#{path := <<"/path/to">>}], _} = query(Store1, URIMap),
	{ok, [#{path := <<"/path/to">>}], _} = query(Store1, URIMap#{path => <<"/path/to/other">>}),
	{ok, [#{path := <<"/path/to">>}], _} = query(Store1, URIMap#{path => <<"/path/to/resource/sub">>}),
	%% Confirm that the cookie cannot be retrieved for parent or unrelated paths.
	{ok, [], _} = query(Store1, URIMap#{path => <<"/path">>}),
	{ok, [], _} = query(Store1, URIMap#{path => <<"/path/toon">>}),
	{ok, [], _} = query(Store1, URIMap#{path => <<"/">>}),
	%% Expire the cookie.
	{ok, N2, V2, A2} = cow_cookie:parse_set_cookie(<<"cookies-path-default=1; Max-Age=0">>),
	{ok, Store} = set_cookie(Store1, URIMap, N2, V2, A2),
	{ok, [], _} = query(Store, URIMap),
	{ok, [], _} = query(Store, URIMap#{path => <<"/path/to/other">>}),
	{ok, [], _} = query(Store, URIMap#{path => <<"/path/to/resource/sub">>}),
	ok.

%% WPT: path/match
wpt_path_match_test_() ->
	URIMap = #{
		scheme => <<"http">>,
		host => <<?HOST>>,
		path => <<"/cookies/resources/echo-cookie.html">>
	},
	MatchTests = [
		<<"/">>,
		<<"match.html">>,
		<<"cookies">>,
		<<"/cookies">>,
		<<"/cookies/">>,
		<<"/cookies/resources/echo-cookie.html">>
	],
	NegTests = [
		<<"/cook">>,
		<<"/w/">>
	],
	[{P, fun() ->
		{ok, N1, V1, A1} = cow_cookie:parse_set_cookie(<<"a=b; Path=",P/binary>>),
		{ok, Store0} = set_cookie(gun_cookies_list:init(), URIMap, N1, V1, A1),
		{ok, [#{name := <<"a">>}], _} = query(Store0, URIMap),
		{ok, N2, V2, A2} = cow_cookie:parse_set_cookie(<<"a=b; Max-Age=0; Path=",P/binary>>),
		{ok, Store} = set_cookie(Store0, URIMap, N2, V2, A2),
		{ok, [], _} = query(Store, URIMap)
	end} || P <- MatchTests]
	++
	[{P, fun() ->
		{ok, N, V, A} = cow_cookie:parse_set_cookie(<<"a=b; Path=",P/binary>>),
		{ok, Store} = set_cookie(gun_cookies_list:init(), URIMap, N, V, A),
		{ok, [], _} = query(Store, URIMap)
	end} || P <- NegTests].

%% WPT: prefix/__host.header
wpt_prefix_host_test_() ->
	Tests = [
		{<<"http">>, <<"__Host-foo=bar; Path=/;">>, false},
		{<<"http">>, <<"__Host-foo=bar; Path=/;domain=" ?HOST>>, false},
		{<<"http">>, <<"__Host-foo=bar; Path=/;Max-Age=10">>, false},
		{<<"http">>, <<"__Host-foo=bar; Path=/;HttpOnly">>, false},
		{<<"http">>, <<"__Host-foo=bar; Secure; Path=/;">>, false},
		{<<"http">>, <<"__Host-foo=bar; Secure; Path=/;domain=" ?HOST>>, false},
		{<<"http">>, <<"__Host-foo=bar; Secure; Path=/;Max-Age=10">>, false},
		{<<"http">>, <<"__Host-foo=bar; Secure; Path=/;HttpOnly">>, false},
		{<<"http">>, <<"__Host-foo=bar; Secure; Path=/; Domain=" ?HOST "; ">>, false},
		{<<"http">>, <<"__Host-foo=bar; Secure; Path=/; Domain=" ?HOST "; domain=" ?HOST>>, false},
		{<<"http">>, <<"__Host-foo=bar; Secure; Path=/; Domain=" ?HOST "; Max-Age=10">>, false},
		{<<"http">>, <<"__Host-foo=bar; Secure; Path=/; Domain=" ?HOST "; HttpOnly">>, false},
		{<<"http">>, <<"__Host-foo=bar; Secure; Path=/cookies/resources/list.py">>, false},
		{<<"https">>, <<"__Host-foo=bar; Path=/;">>, false},
		{<<"https">>, <<"__Host-foo=bar; Path=/;Max-Age=10">>, false},
		{<<"https">>, <<"__Host-foo=bar; Path=/;HttpOnly">>, false},
		{<<"https">>, <<"__Host-foo=bar; Secure; Path=/;">>, true},
		{<<"https">>, <<"__Host-foo=bar; Secure; Path=/;Max-Age=10">>, true},
		{<<"https">>, <<"__Host-foo=bar; Secure; Path=/;HttpOnly">>, true},
		{<<"https">>, <<"__Host-foo=bar; Secure; Path=/; Domain=" ?HOST "; ">>, false},
		{<<"https">>, <<"__Host-foo=bar; Secure; Path=/; Domain=" ?HOST "; Max-Age=10">>, false},
		{<<"https">>, <<"__Host-foo=bar; Secure; Path=/; Domain=" ?HOST "; HttpOnly">>, false},
		{<<"https">>, <<"__Host-foo=bar; Secure; Path=/cookies/resources/list.py">>, false}
	],
	wpt_prefix_common(Tests, <<"__Host-foo">>).

%% WPT: prefix/__secure.header
wpt_prefix_secure_test_() ->
	Tests = [
		{<<"http">>, <<"__Secure-foo=bar; Path=/;">>, false},
		{<<"http">>, <<"__Secure-foo=bar; Path=/;domain=" ?HOST>>, false},
		{<<"http">>, <<"__Secure-foo=bar; Path=/;Max-Age=10">>, false},
		{<<"http">>, <<"__Secure-foo=bar; Path=/;HttpOnly">>, false},
		{<<"http">>, <<"__Secure-foo=bar; Secure; Path=/;">>, false},
		{<<"http">>, <<"__Secure-foo=bar; Secure; Path=/;domain=" ?HOST>>, false},
		{<<"http">>, <<"__Secure-foo=bar; Secure; Path=/;Max-Age=10">>, false},
		{<<"http">>, <<"__Secure-foo=bar; Secure; Path=/;HttpOnly">>, false},
		{<<"https">>, <<"__Secure-foo=bar; Path=/;">>, false},
		{<<"https">>, <<"__Secure-foo=bar; Path=/;Max-Age=10">>, false},
		{<<"https">>, <<"__Secure-foo=bar; Path=/;HttpOnly">>, false},
		{<<"https">>, <<"__Secure-foo=bar; Secure; Path=/;">>, true},
		{<<"https">>, <<"__Secure-foo=bar; Secure; Path=/;Max-Age=10">>, true},
		{<<"https">>, <<"__Secure-foo=bar; Secure; Path=/;HttpOnly">>, true}
		%% Missing two SameSite cases from prefix/__secure.header.https. (Not implemented.)
	],
	wpt_prefix_common(Tests, <<"__Secure-foo">>).

wpt_prefix_common(Tests, Name) ->
	URIMap0 = #{
		host => <<?HOST>>,
		path => <<"/cookies/resources/set.py">>
	},
	[{<<S/binary," ",H/binary>>, fun() ->
		URIMap1 = URIMap0#{scheme => S},
		{ok, N, V, A} = cow_cookie:parse_set_cookie(H),
		case set_cookie(gun_cookies_list:init(), URIMap1, N, V, A) of
			{ok, Store} when Res =:= true ->
				URIMap = URIMap1#{path => <<"/cookies/resources/list.py">>},
				{ok, [#{name := Name}], _} = query(Store, URIMap),
				ok;
			{error, _} ->
				ok
		end
	end} || {S, H, Res} <- Tests].

%% WPT: samesite-none-secure/ (Not implemented.)
%% WPT: samesite/ (Not implemented.)

wpt_secure_https_test() ->
	URIMap = #{
		scheme => <<"https">>,
		host => <<?HOST>>,
		path => <<"/cookies/secure/any.html">>
	},
	{ok, N, V, A} = cow_cookie:parse_set_cookie(<<"secure_from_secure_http=1; Secure; Path=/">>),
	{ok, Store} = set_cookie(gun_cookies_list:init(), URIMap, N, V, A),
	{ok, [#{name := <<"secure_from_secure_http">>}], _} = query(Store, URIMap),
	ok.

wpt_secure_http_test() ->
	URIMap = #{
		scheme => <<"http">>,
		host => <<?HOST>>,
		path => <<"/cookies/secure/any.html">>
	},
	{ok, N, V, A} = cow_cookie:parse_set_cookie(<<"secure_from_nonsecure_http=1; Secure; Path=/">>),
	{error, secure_scheme_only} = set_cookie(gun_cookies_list:init(), URIMap, N, V, A),
	ok.

%% WPT: secure/set-from-ws* (Anything special required?)
-endif.
