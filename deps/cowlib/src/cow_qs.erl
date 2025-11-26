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

-module(cow_qs).
-dialyzer(no_improper_lists).

-export([parse_qs/1]).
-export([qs/1]).
-export([urldecode/1]).
-export([urlencode/1]).

-include("cow_inline.hrl").

-type qs_vals() :: [{binary(), binary() | true}].

%% Parse an application/x-www-form-urlencoded string.
%%
%% The percent decoding is inlined to greatly improve the performance
%% by avoiding copying binaries twice (once for extracting, once for
%% decoding) instead of just extracting the proper representation.

-spec parse_qs(binary()) -> qs_vals().

parse_qs(Binary) ->
	parse_qs_name(Binary, Binary, 0, 0).

-define(IS_NOT_SEP(C), (C =/= $&) andalso (C =/= $=)).

parse_qs_name(<<C1, C2, C3, C4, Rest/bits>>, Orig, Skip, Len)
		when ?IS_NOT_SEP(C1) andalso ?IS_NOT_SEP(C2)
		andalso ?IS_NOT_SEP(C3) andalso ?IS_NOT_SEP(C4) ->
	parse_qs_name(Rest, Orig, Skip, Len + 4);
parse_qs_name(<<C, Rest/bits>>, Orig, Skip, Len)
		when ?IS_NOT_SEP(C) ->
	parse_qs_name(Rest, Orig, Skip, Len + 1);
parse_qs_name(<<$=, Rest/bits>>, Orig, Skip, Len)
		when Len =/= 0 ->
	Name = urldecode(binary:part(Orig, Skip, Len)),
	parse_qs_value(Rest, Orig, Skip + Len + 1, 0, Name);
parse_qs_name(<<$&, Rest/bits>>, Orig, Skip, 0) ->
	parse_qs_name(Rest, Orig, Skip + 1, 0);
parse_qs_name(<<$&, Rest/bits>>, Orig, Skip, Len) ->
	Name = urldecode(binary:part(Orig, Skip, Len)),
	[{Name, true}|parse_qs_name(Rest, Orig, Skip + Len + 1, 0)];
parse_qs_name(<<>>, _, _, 0) ->
	[];
parse_qs_name(<<>>, Orig, Skip, Len) ->
	Name = urldecode(binary:part(Orig, Skip, Len)),
	[{Name, true}].

parse_qs_value(<<C1, C2, C3, C4, Rest/bits>>, Orig, Skip, Len, Name)
		when (C1 =/= $&) andalso (C2 =/= $&)
		andalso (C3 =/= $&) andalso (C4 =/= $&) ->
	parse_qs_value(Rest, Orig, Skip, Len + 4, Name);
parse_qs_value(<<C, Rest/bits>>, Orig, Skip, Len, Name)
		when (C =/= $&) ->
	parse_qs_value(Rest, Orig, Skip, Len + 1, Name);
parse_qs_value(<<$&, Rest/bits>>, Orig, Skip, Len, Name) ->
	Value = urldecode(binary:part(Orig, Skip, Len)),
	[{Name, Value}|parse_qs_name(Rest, Orig, Skip + Len + 1, 0)];
parse_qs_value(<<>>, _, _, 0, Name) ->
	[{Name, <<>>}];
parse_qs_value(<<>>, Orig, Skip, Len, Name) ->
	Value = urldecode(binary:part(Orig, Skip, Len)),
	[{Name, Value}].

-ifdef(TEST).
parse_qs_test_() ->
	Tests = [
		{<<>>, []},
		{<<"&">>, []},
		{<<"a">>, [{<<"a">>, true}]},
		{<<"a&">>, [{<<"a">>, true}]},
		{<<"&a">>, [{<<"a">>, true}]},
		{<<"a&b">>, [{<<"a">>, true}, {<<"b">>, true}]},
		{<<"a&&b">>, [{<<"a">>, true}, {<<"b">>, true}]},
		{<<"a&b&">>, [{<<"a">>, true}, {<<"b">>, true}]},
		{<<"=">>, error},
		{<<"=b">>, error},
		{<<"a=">>, [{<<"a">>, <<>>}]},
		{<<"a=b">>, [{<<"a">>, <<"b">>}]},
		{<<"a=&b=">>, [{<<"a">>, <<>>}, {<<"b">>, <<>>}]},
		{<<"a=b&c&d=e">>, [{<<"a">>, <<"b">>},
			{<<"c">>, true}, {<<"d">>, <<"e">>}]},
		{<<"a=b=c&d=e=f&g=h=i">>, [{<<"a">>, <<"b=c">>},
			{<<"d">>, <<"e=f">>}, {<<"g">>, <<"h=i">>}]},
		{<<"+">>, [{<<" ">>, true}]},
		{<<"+=+">>, [{<<" ">>, <<" ">>}]},
		{<<"a+b=c+d">>, [{<<"a b">>, <<"c d">>}]},
		{<<"+a+=+b+&+c+=+d+">>, [{<<" a ">>, <<" b ">>},
			{<<" c ">>, <<" d ">>}]},
		{<<"a%20b=c%20d">>, [{<<"a b">>, <<"c d">>}]},
		{<<"%25%26%3D=%25%26%3D&_-.=.-_">>, [{<<"%&=">>, <<"%&=">>},
			{<<"_-.">>, <<".-_">>}]},
		{<<"for=extend%2Franch">>, [{<<"for">>, <<"extend/ranch">>}]}
	],
	[{Qs, fun() ->
		E = try parse_qs(Qs) of
			R -> R
		catch _:_ ->
			error
		end
	end} || {Qs, E} <- Tests].

parse_qs_identity_test_() ->
	Tests = [
		<<"+">>,
		<<"hl=en&q=erlang+cowboy">>,
		<<"direction=desc&for=extend%2Franch&sort=updated&state=open">>,
		<<"i=EWiIXmPj5gl6&v=QowBp0oDLQXdd4x_GwiywA&ip=98.20.31.81&"
			"la=en&pg=New8.undertonebrandsafe.com%2F698a2525065ee2"
			"60c0b2f2aaad89ab82&re=&sz=1&fc=1&fr=140&br=3&bv=11.0."
			"696.16&os=3&ov=&rs=vpl&k=cookies%7Csale%7Cbrowser%7Cm"
			"ore%7Cprivacy%7Cstatistics%7Cactivities%7Cauction%7Ce"
			"mail%7Cfree%7Cin...&t=112373&xt=5%7C61%7C0&tz=-1&ev=x"
			"&tk=&za=1&ortb-za=1&zu=&zl=&ax=U&ay=U&ortb-pid=536454"
			".55&ortb-sid=112373.8&seats=999&ortb-xt=IAB24&ortb-ugc=">>,
		<<"i=9pQNskA&v=0ySQQd1F&ev=12345678&t=12345&sz=3&ip=67.58."
			"236.89&la=en&pg=http%3A%2F%2Fwww.yahoo.com%2Fpage1.ht"
			"m&re=http%3A%2F%2Fsearch.google.com&fc=1&fr=1&br=2&bv"
			"=3.0.14&os=1&ov=XP&k=cars%2Cford&rs=js&xt=5%7C22%7C23"
			"4&tz=%2B180&tk=key1%3Dvalue1%7Ckey2%3Dvalue2&zl=4%2C5"
			"%2C6&za=4&zu=competitor.com&ua=Mozilla%2F5.0+%28Windo"
			"ws%3B+U%3B+Windows+NT+6.1%3B+en-US%29+AppleWebKit%2F5"
			"34.13+%28KHTML%2C+like+Gecko%29+Chrome%2F9.0.597.98+S"
			"afari%2F534.13&ortb-za=1%2C6%2C13&ortb-pid=521732&ort"
			"b-sid=521732&ortb-xt=IAB3&ortb-ugc=">>
	],
	[{V, fun() -> V = qs(parse_qs(V)) end} || V <- Tests].

horse_parse_qs_shorter() ->
	horse:repeat(20000,
		parse_qs(<<"hl=en&q=erlang%20cowboy">>)
	).

horse_parse_qs_short() ->
	horse:repeat(20000,
		parse_qs(
			<<"direction=desc&for=extend%2Franch&sort=updated&state=open">>)
	).

horse_parse_qs_long() ->
	horse:repeat(20000,
		parse_qs(<<"i=EWiIXmPj5gl6&v=QowBp0oDLQXdd4x_GwiywA&ip=98.20.31.81&"
			"la=en&pg=New8.undertonebrandsafe.com%2F698a2525065ee260c0b2f2a"
			"aad89ab82&re=&sz=1&fc=1&fr=140&br=3&bv=11.0.696.16&os=3&ov=&rs"
			"=vpl&k=cookies%7Csale%7Cbrowser%7Cmore%7Cprivacy%7Cstatistics%"
			"7Cactivities%7Cauction%7Cemail%7Cfree%7Cin...&t=112373&xt=5%7C"
			"61%7C0&tz=-1&ev=x&tk=&za=1&ortb-za=1&zu=&zl=&ax=U&ay=U&ortb-pi"
			"d=536454.55&ortb-sid=112373.8&seats=999&ortb-xt=IAB24&ortb-ugc"
			"=">>)
	).

horse_parse_qs_longer() ->
	horse:repeat(20000,
		parse_qs(<<"i=9pQNskA&v=0ySQQd1F&ev=12345678&t=12345&sz=3&ip=67.58."
			"236.89&la=en&pg=http%3A%2F%2Fwww.yahoo.com%2Fpage1.htm&re=http"
			"%3A%2F%2Fsearch.google.com&fc=1&fr=1&br=2&bv=3.0.14&os=1&ov=XP"
			"&k=cars%2cford&rs=js&xt=5%7c22%7c234&tz=%2b180&tk=key1%3Dvalue"
			"1%7Ckey2%3Dvalue2&zl=4,5,6&za=4&zu=competitor.com&ua=Mozilla%2"
			"F5.0%20(Windows%3B%20U%3B%20Windows%20NT%206.1%3B%20en-US)%20A"
			"ppleWebKit%2F534.13%20(KHTML%2C%20like%20Gecko)%20Chrome%2F9.0"
			".597.98%20Safari%2F534.13&ortb-za=1%2C6%2C13&ortb-pid=521732&o"
			"rtb-sid=521732&ortb-xt=IAB3&ortb-ugc=">>)
	).
-endif.

%% Build an application/x-www-form-urlencoded string.

-spec qs(qs_vals()) -> binary().

qs([]) ->
	<<>>;
qs(L) ->
	qs(L, []).

qs([], Acc) ->
	iolist_to_binary(lists:join(<<$&>>, lists:reverse(Acc)));
qs([{Name, true}|Tail], Acc) ->
	qs(Tail, [urlencode_to_iolist(Name)|Acc]);
qs([{Name, Value}|Tail], Acc) ->
	qs(Tail, [[urlencode_to_iolist(Name), $=, urlencode_to_iolist(Value)]|Acc]).

-define(QS_SHORTER, [
	{<<"hl">>, <<"en">>},
	{<<"q">>, <<"erlang cowboy">>}
]).

-define(QS_SHORT, [
	{<<"direction">>, <<"desc">>},
	{<<"for">>, <<"extend/ranch">>},
	{<<"sort">>, <<"updated">>},
	{<<"state">>, <<"open">>}
]).

-define(QS_LONG, [
	{<<"i">>, <<"EWiIXmPj5gl6">>},
	{<<"v">>, <<"QowBp0oDLQXdd4x_GwiywA">>},
	{<<"ip">>, <<"98.20.31.81">>},
	{<<"la">>, <<"en">>},
	{<<"pg">>, <<"New8.undertonebrandsafe.com/"
		"698a2525065ee260c0b2f2aaad89ab82">>},
	{<<"re">>, <<>>},
	{<<"sz">>, <<"1">>},
	{<<"fc">>, <<"1">>},
	{<<"fr">>, <<"140">>},
	{<<"br">>, <<"3">>},
	{<<"bv">>, <<"11.0.696.16">>},
	{<<"os">>, <<"3">>},
	{<<"ov">>, <<>>},
	{<<"rs">>, <<"vpl">>},
	{<<"k">>, <<"cookies|sale|browser|more|privacy|statistics|"
		"activities|auction|email|free|in...">>},
	{<<"t">>, <<"112373">>},
	{<<"xt">>, <<"5|61|0">>},
	{<<"tz">>, <<"-1">>},
	{<<"ev">>, <<"x">>},
	{<<"tk">>, <<>>},
	{<<"za">>, <<"1">>},
	{<<"ortb-za">>, <<"1">>},
	{<<"zu">>, <<>>},
	{<<"zl">>, <<>>},
	{<<"ax">>, <<"U">>},
	{<<"ay">>, <<"U">>},
	{<<"ortb-pid">>, <<"536454.55">>},
	{<<"ortb-sid">>, <<"112373.8">>},
	{<<"seats">>, <<"999">>},
	{<<"ortb-xt">>, <<"IAB24">>},
	{<<"ortb-ugc">>, <<>>}
]).

-define(QS_LONGER, [
	{<<"i">>, <<"9pQNskA">>},
	{<<"v">>, <<"0ySQQd1F">>},
	{<<"ev">>, <<"12345678">>},
	{<<"t">>, <<"12345">>},
	{<<"sz">>, <<"3">>},
	{<<"ip">>, <<"67.58.236.89">>},
	{<<"la">>, <<"en">>},
	{<<"pg">>, <<"http://www.yahoo.com/page1.htm">>},
	{<<"re">>, <<"http://search.google.com">>},
	{<<"fc">>, <<"1">>},
	{<<"fr">>, <<"1">>},
	{<<"br">>, <<"2">>},
	{<<"bv">>, <<"3.0.14">>},
	{<<"os">>, <<"1">>},
	{<<"ov">>, <<"XP">>},
	{<<"k">>, <<"cars,ford">>},
	{<<"rs">>, <<"js">>},
	{<<"xt">>, <<"5|22|234">>},
	{<<"tz">>, <<"+180">>},
	{<<"tk">>, <<"key1=value1|key2=value2">>},
	{<<"zl">>, <<"4,5,6">>},
	{<<"za">>, <<"4">>},
	{<<"zu">>, <<"competitor.com">>},
	{<<"ua">>, <<"Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) "
		"AppleWebKit/534.13 (KHTML, like Gecko) Chrome/9.0.597.98 "
		"Safari/534.13">>},
	{<<"ortb-za">>, <<"1,6,13">>},
	{<<"ortb-pid">>, <<"521732">>},
	{<<"ortb-sid">>, <<"521732">>},
	{<<"ortb-xt">>, <<"IAB3">>},
	{<<"ortb-ugc">>, <<>>}
]).

-ifdef(TEST).
qs_test_() ->
	Tests = [
		{[<<"a">>], error},
		{[{<<"a">>, <<"b">>, <<"c">>}], error},
		{[], <<>>},
		{[{<<"a">>, true}], <<"a">>},
		{[{<<"a">>, true}, {<<"b">>, true}], <<"a&b">>},
		{[{<<"a">>, <<>>}], <<"a=">>},
		{[{<<"a">>, <<"b">>}], <<"a=b">>},
		{[{<<"a">>, <<>>}, {<<"b">>, <<>>}], <<"a=&b=">>},
		{[{<<"a">>, <<"b">>}, {<<"c">>, true}, {<<"d">>, <<"e">>}],
			<<"a=b&c&d=e">>},
		{[{<<"a">>, <<"b=c">>}, {<<"d">>, <<"e=f">>}, {<<"g">>, <<"h=i">>}],
			<<"a=b%3Dc&d=e%3Df&g=h%3Di">>},
		{[{<<" ">>, true}], <<"+">>},
		{[{<<" ">>, <<" ">>}], <<"+=+">>},
		{[{<<"a b">>, <<"c d">>}], <<"a+b=c+d">>},
		{[{<<" a ">>, <<" b ">>}, {<<" c ">>, <<" d ">>}],
			<<"+a+=+b+&+c+=+d+">>},
		{[{<<"%&=">>, <<"%&=">>}, {<<"_-.">>, <<".-_">>}],
			<<"%25%26%3D=%25%26%3D&_-.=.-_">>},
		{[{<<"for">>, <<"extend/ranch">>}], <<"for=extend%2Franch">>}
	],
	[{lists:flatten(io_lib:format("~p", [Vals])), fun() ->
		E = try qs(Vals) of
			R -> R
		catch _:_ ->
			error
		end
	end} || {Vals, E} <- Tests].

qs_identity_test_() ->
	Tests = [
		[{<<"+">>, true}],
		?QS_SHORTER,
		?QS_SHORT,
		?QS_LONG,
		?QS_LONGER
	],
	[{lists:flatten(io_lib:format("~p", [V])), fun() ->
		V = parse_qs(qs(V))
	end} || V <- Tests].

horse_qs_shorter() ->
	horse:repeat(20000, qs(?QS_SHORTER)).

horse_qs_short() ->
	horse:repeat(20000, qs(?QS_SHORT)).

horse_qs_long() ->
	horse:repeat(20000, qs(?QS_LONG)).

horse_qs_longer() ->
	horse:repeat(20000, qs(?QS_LONGER)).
-endif.

%% Decode a percent encoded string (x-www-form-urlencoded rules).

-spec urldecode(binary()) -> binary().

urldecode(Binary) ->
	skip_dec(Binary, Binary, 0).

-define(IS_NOT_ENC(C), (C =/= $%) andalso (C =/= $+)).

%% This functions helps avoid a binary allocation when
%% there is nothing to decode.
skip_dec(Binary, Orig, Len) ->
	case Binary of
		<<C1, C2, C3, C4, Rest/bits>>
				when ?IS_NOT_ENC(C1) andalso ?IS_NOT_ENC(C2)
				andalso ?IS_NOT_ENC(C3) andalso ?IS_NOT_ENC(C4) ->
			skip_dec(Rest, Orig, Len + 4);
		_ ->
			dec(Binary, [], Orig, 0, Len)
	end.

%% This clause helps speed up decoding of highly encoded values.
dec(<<$%, H1, L1, $%, H2, L2, $%, H3, L3, $%, H4, L4, Rest/bits>>, Acc, Orig, Skip, Len) ->
	C1 = ?UNHEX(H1, L1),
	C2 = ?UNHEX(H2, L2),
	C3 = ?UNHEX(H3, L3),
	C4 = ?UNHEX(H4, L4),
	case Len of
		0 ->
			dec(Rest, [Acc|<<C1, C2, C3, C4>>], Orig, Skip + 12, 0);
		_ ->
			Part = binary_part(Orig, Skip, Len),
			dec(Rest, [Acc, Part|<<C1, C2, C3, C4>>], Orig, Skip + Len + 12, 0)
	end;
dec(<<$%, H, L, Rest/bits>>, Acc, Orig, Skip, Len) ->
	C = ?UNHEX(H, L),
	case Len of
		0 ->
			dec(Rest, [Acc|<<C>>], Orig, Skip + 3, 0);
		_ ->
			Part = binary_part(Orig, Skip, Len),
			dec(Rest, [Acc, Part|<<C>>], Orig, Skip + Len + 3, 0)
	end;
dec(<<$+, Rest/bits>>, Acc, Orig, Skip, Len) ->
	case Len of
		0 ->
			dec(Rest, [Acc|<<" ">>], Orig, Skip + 1, 0);
		_ ->
			Part = binary_part(Orig, Skip, Len),
			dec(Rest, [Acc, Part|<<" ">>], Orig, Skip + Len + 1, 0)
	end;
%% This clause helps speed up decoding of barely encoded values.
dec(<<C1, C2, C3, C4, Rest/bits>>, Acc, Orig, Skip, Len)
		when (C1 =/= $%) andalso ?IS_NOT_ENC(C2)
		andalso ?IS_NOT_ENC(C3) andalso ?IS_NOT_ENC(C4) ->
	dec(Rest, Acc, Orig, Skip, Len + 4);
dec(<<C, Rest/bits>>, Acc, Orig, Skip, Len)
		when (C =/= $%) ->
	dec(Rest, Acc, Orig, Skip, Len + 1);
dec(<<>>, _, Orig, 0, _) ->
	Orig;
dec(<<>>, Acc, _, _, 0) ->
	iolist_to_binary(Acc);
dec(<<>>, Acc, Orig, Skip, Len) ->
	Part = binary_part(Orig, Skip, Len),
	iolist_to_binary([Acc|Part]);
dec(_, _, Orig, Skip, Len) ->
	error({invalid_byte, binary:at(Orig, Skip + Len)}).

-ifdef(TEST).
urldecode_test_() ->
	Tests = [
		{<<"%20">>, <<" ">>},
		{<<"+">>, <<" ">>},
		{<<"%00">>, <<0>>},
		{<<"%fF">>, <<255>>},
		{<<"123">>, <<"123">>},
		{<<"%i5">>, error},
		{<<"%5">>, error}
	],
	[{Qs, fun() ->
		E = try urldecode(Qs) of
			R -> R
		catch _:_ ->
			error
		end
	end} || {Qs, E} <- Tests].

urldecode_identity_test_() ->
	Tests = [
		<<"+">>,
		<<"nothingnothingnothingnothing">>,
		<<"Small+fast+modular+HTTP+server">>,
		<<"Small%2C+fast%2C+modular+HTTP+server.">>,
		<<"%E3%83%84%E3%82%A4%E3%83%B3%E3%82%BD%E3%82%A6%E3%83"
			"%AB%E3%80%9C%E8%BC%AA%E5%BB%BB%E3%81%99%E3%82%8B%E6%97%8B%E5"
			"%BE%8B%E3%80%9C">>
	],
	[{V, fun() -> V = urlencode(urldecode(V)) end} || V <- Tests].

horse_urldecode() ->
	horse:repeat(100000,
		urldecode(<<"nothingnothingnothingnothing">>)
	).

horse_urldecode_plus() ->
	horse:repeat(100000,
		urldecode(<<"Small+fast+modular+HTTP+server">>)
	).

horse_urldecode_hex() ->
	horse:repeat(100000,
		urldecode(<<"Small%2C%20fast%2C%20modular%20HTTP%20server.">>)
	).

horse_urldecode_jp_hex() ->
	horse:repeat(100000,
		urldecode(<<"%E3%83%84%E3%82%A4%E3%83%B3%E3%82%BD%E3%82%A6%E3%83"
			"%AB%E3%80%9C%E8%BC%AA%E5%BB%BB%E3%81%99%E3%82%8B%E6%97%8B%E5"
			"%BE%8B%E3%80%9C">>)
	).

horse_urldecode_mix() ->
	horse:repeat(100000,
		urldecode(<<"Small%2C+fast%2C+modular+HTTP+server.">>)
	).
-endif.

%% Percent encode a string (x-www-form-urlencoded rules).

-spec urlencode(binary()) -> binary().

urlencode(Binary) ->
	case skip_enc(Binary, Binary, 0) of
		orig -> Binary;
		IOList -> iolist_to_binary(IOList)
	end.

%% Used in qs/1 to avoid calling iolist_to_binary unnecessarily.
urlencode_to_iolist(Binary) ->
	case skip_enc(Binary, Binary, 0) of
		orig -> Binary;
		IOList -> IOList
	end.

-define(IS_PLAIN(C), (
	(C =:= $-) orelse (C =:= $.) orelse (C =:= $0) orelse (C =:= $1) orelse
	(C =:= $2) orelse (C =:= $3) orelse (C =:= $4) orelse (C =:= $5) orelse
	(C =:= $6) orelse (C =:= $7) orelse (C =:= $8) orelse (C =:= $9) orelse
	(C =:= $A) orelse (C =:= $B) orelse (C =:= $C) orelse (C =:= $D) orelse
	(C =:= $E) orelse (C =:= $F) orelse (C =:= $G) orelse (C =:= $H) orelse
	(C =:= $I) orelse (C =:= $J) orelse (C =:= $K) orelse (C =:= $L) orelse
	(C =:= $M) orelse (C =:= $N) orelse (C =:= $O) orelse (C =:= $P) orelse
	(C =:= $Q) orelse (C =:= $R) orelse (C =:= $S) orelse (C =:= $T) orelse
	(C =:= $U) orelse (C =:= $V) orelse (C =:= $W) orelse (C =:= $X) orelse
	(C =:= $Y) orelse (C =:= $Z) orelse (C =:= $_) orelse (C =:= $a) orelse
	(C =:= $b) orelse (C =:= $c) orelse (C =:= $d) orelse (C =:= $e) orelse
	(C =:= $f) orelse (C =:= $g) orelse (C =:= $h) orelse (C =:= $i) orelse
	(C =:= $j) orelse (C =:= $k) orelse (C =:= $l) orelse (C =:= $m) orelse
	(C =:= $n) orelse (C =:= $o) orelse (C =:= $p) orelse (C =:= $q) orelse
	(C =:= $r) orelse (C =:= $s) orelse (C =:= $t) orelse (C =:= $u) orelse
	(C =:= $v) orelse (C =:= $w) orelse (C =:= $x) orelse (C =:= $y) orelse
	(C =:= $z)
)).

skip_enc(Binary, Orig, Len) ->
	case Binary of
		<<C1, C2, C3, C4, Rest/bits>>
				when ?IS_PLAIN(C1) andalso ?IS_PLAIN(C2)
				andalso ?IS_PLAIN(C3) andalso ?IS_PLAIN(C4) ->
			skip_enc(Rest, Orig, Len + 4);
		_ ->
			enc(Binary, [], Orig, 0, Len)
	end.

enc(<<C1, C2, C3, C4, Rest/bits>>, Acc, Orig, Skip, Len)
		when ?IS_PLAIN(C1) andalso ?IS_PLAIN(C2)
		andalso ?IS_PLAIN(C3) andalso ?IS_PLAIN(C4) ->
	enc(Rest, Acc, Orig, Skip, Len + 4);
enc(<<C, Rest/bits>>, Acc, Orig, Skip, Len)
		when ?IS_PLAIN(C) ->
	enc(Rest, Acc, Orig, Skip, Len + 1);
enc(<<C1, C2, C3, C4, Rest/bits>>, Acc, Orig, Skip, Len)
		when (not ?IS_PLAIN(C2)) andalso (not ?IS_PLAIN(C3))
		andalso (not ?IS_PLAIN(C4)) ->
	Enc = <<$%, ?HEX(C1), $%, ?HEX(C2), $%, ?HEX(C3), $%, ?HEX(C4)>>,
	case Len of
		0 ->
			enc(Rest, [Acc|Enc], Orig, Skip + 4, 0);
		_ ->
			Part = binary_part(Orig, Skip, Len),
			enc(Rest, [Acc, Part|Enc], Orig, Skip + Len + 4, 0)
	end;
enc(<<C, Rest/bits>>, Acc, Orig, Skip, Len) ->
	Enc = case C of
		$\s -> <<$+>>;
		_ -> <<$%, ?HEX(C)>>
	end,
	case Len of
		0 ->
			enc(Rest, [Acc|Enc], Orig, Skip + 1, 0);
		_ ->
			Part = binary_part(Orig, Skip, Len),
			enc(Rest, [Acc, Part|Enc], Orig, Skip + Len + 1, 0)
	end;
enc(<<>>, _, _, 0, _) ->
	orig;
enc(<<>>, Acc, _, _, 0) ->
	Acc;
enc(<<>>, Acc, Orig, Skip, Len) ->
	Part = binary_part(Orig, Skip, Len),
	[Acc|Part];
enc(_, _, Orig, Skip, Len) ->
	error({invalid_byte, binary:at(Orig, Skip + Len)}).

-ifdef(TEST).
urlencode_test_() ->
	Tests = [
		{<<255, 0>>, <<"%FF%00">>},
		{<<255, " ">>, <<"%FF+">>},
		{<<" ">>, <<"+">>},
		{<<"aBc123">>, <<"aBc123">>},
		{<<".-_">>, <<".-_">>}
	],
	[{V, fun() -> E = urlencode(V) end} || {V, E} <- Tests].

urlencode_identity_test_() ->
	Tests = [
		<<"+">>,
		<<"nothingnothingnothingnothing">>,
		<<"Small fast modular HTTP server">>,
		<<"Small, fast, modular HTTP server.">>,
		<<227,131,132,227,130,164,227,131,179,227,130,189,227,
			130,166,227,131,171,227,128,156,232,188,170,229,187,187,227,
			129,153,227,130,139,230,151,139,229,190,139,227,128,156>>
	],
	[{V, fun() -> V = urldecode(urlencode(V)) end} || V <- Tests].

horse_urlencode() ->
	horse:repeat(100000,
		urlencode(<<"nothingnothingnothingnothing">>)
	).

horse_urlencode_plus() ->
	horse:repeat(100000,
		urlencode(<<"Small fast modular HTTP server">>)
	).

horse_urlencode_jp() ->
	horse:repeat(100000,
		urlencode(<<227,131,132,227,130,164,227,131,179,227,130,189,227,
			130,166,227,131,171,227,128,156,232,188,170,229,187,187,227,
			129,153,227,130,139,230,151,139,229,190,139,227,128,156>>)
	).

horse_urlencode_mix() ->
	horse:repeat(100000,
		urlencode(<<"Small, fast, modular HTTP server.">>)
	).
-endif.
