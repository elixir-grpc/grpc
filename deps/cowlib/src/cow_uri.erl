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

-module(cow_uri).
-dialyzer(no_improper_lists).

-export([urldecode/1]).
-export([urlencode/1]).

-include("cow_inline.hrl").

-define(IS_PLAIN(C), (
	(C =:= $!) orelse (C =:= $$) orelse (C =:= $&) orelse (C =:= $') orelse
	(C =:= $() orelse (C =:= $)) orelse (C =:= $*) orelse (C =:= $+) orelse
	(C =:= $,) orelse (C =:= $-) orelse (C =:= $.) orelse (C =:= $0) orelse
	(C =:= $1) orelse (C =:= $2) orelse (C =:= $3) orelse (C =:= $4) orelse
	(C =:= $5) orelse (C =:= $6) orelse (C =:= $7) orelse (C =:= $8) orelse
	(C =:= $9) orelse (C =:= $:) orelse (C =:= $;) orelse (C =:= $=) orelse
	(C =:= $@) orelse (C =:= $A) orelse (C =:= $B) orelse (C =:= $C) orelse
	(C =:= $D) orelse (C =:= $E) orelse (C =:= $F) orelse (C =:= $G) orelse
	(C =:= $H) orelse (C =:= $I) orelse (C =:= $J) orelse (C =:= $K) orelse
	(C =:= $L) orelse (C =:= $M) orelse (C =:= $N) orelse (C =:= $O) orelse
	(C =:= $P) orelse (C =:= $Q) orelse (C =:= $R) orelse (C =:= $S) orelse
	(C =:= $T) orelse (C =:= $U) orelse (C =:= $V) orelse (C =:= $W) orelse
	(C =:= $X) orelse (C =:= $Y) orelse (C =:= $Z) orelse (C =:= $_) orelse
	(C =:= $a) orelse (C =:= $b) orelse (C =:= $c) orelse (C =:= $d) orelse
	(C =:= $e) orelse (C =:= $f) orelse (C =:= $g) orelse (C =:= $h) orelse
	(C =:= $i) orelse (C =:= $j) orelse (C =:= $k) orelse (C =:= $l) orelse
	(C =:= $m) orelse (C =:= $n) orelse (C =:= $o) orelse (C =:= $p) orelse
	(C =:= $q) orelse (C =:= $r) orelse (C =:= $s) orelse (C =:= $t) orelse
	(C =:= $u) orelse (C =:= $v) orelse (C =:= $w) orelse (C =:= $x) orelse
	(C =:= $y) orelse (C =:= $z) orelse (C =:= $~)
)).

%% Decode a percent encoded string. (RFC3986 2.1)
%%
%% Inspiration for some of the optimisations done here come
%% from the new `json` module as it was in mid-2024.
%%
%% Possible input includes:
%%
%% * nothing encoded (no % character):
%%   We want to return the binary as-is to avoid an allocation.
%%
%% * small number of encoded characters:
%%   We can "skip" words of text.
%%
%% * mostly encoded characters (non-ascii languages)
%%   We can decode characters in bulk.

-spec urldecode(binary()) -> binary().

urldecode(Binary) ->
	skip_dec(Binary, Binary, 0).

%% This functions helps avoid a binary allocation when
%% there is nothing to decode.
skip_dec(Binary, Orig, Len) ->
	case Binary of
		<<C1, C2, C3, C4, Rest/bits>>
				when ?IS_PLAIN(C1) andalso ?IS_PLAIN(C2)
				andalso ?IS_PLAIN(C3) andalso ?IS_PLAIN(C4) ->
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
%% This clause helps speed up decoding of barely encoded values.
dec(<<C1, C2, C3, C4, Rest/bits>>, Acc, Orig, Skip, Len)
		when ?IS_PLAIN(C1) andalso ?IS_PLAIN(C2)
		andalso ?IS_PLAIN(C3) andalso ?IS_PLAIN(C4) ->
	dec(Rest, Acc, Orig, Skip, Len + 4);
dec(<<C, Rest/bits>>, Acc, Orig, Skip, Len)
		when ?IS_PLAIN(C) ->
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
		{<<"+">>, <<"+">>},
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
		<<"%20">>,
		<<"+">>,
		<<"nothingnothingnothingnothing">>,
		<<"Small+fast+modular+HTTP+server">>,
		<<"Small%20fast%20modular%20HTTP%20server">>,
		<<"Small%2F+fast%2F+modular+HTTP+server.">>,
		<<"%E3%83%84%E3%82%A4%E3%83%B3%E3%82%BD%E3%82%A6%E3%83"
			"%AB%E3%80%9C%E8%BC%AA%E5%BB%BB%E3%81%99%E3%82%8B%E6%97%8B%E5"
			"%BE%8B%E3%80%9C">>
	],
	[{V, fun() -> V = urlencode(urldecode(V)) end} || V <- Tests].

horse_urldecode() ->
	horse:repeat(100000,
		urldecode(<<"nothingnothingnothingnothing">>)
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

horse_urldecode_jp_mixed_hex() ->
	horse:repeat(100000,
		urldecode(<<"%E3%83%84%E3%82%A4%E3%83%B3%E3%82%BD%E3123%82%A6%E3%83"
			"%AB%E3%80%9C%E8%BC%AA%E5%BB%BB%E3%81%99%E3%82%8B%E6%97%8B%E5"
			"%BE%8B%E3%80%9C">>)
	).

horse_urldecode_worst_case_hex() ->
	horse:repeat(100000,
		urldecode(<<"%e3%83123%84%e3123%82%a4123%e3%83123%b3%e3123%82%bd123">>)
	).
-endif.

%% Percent encode a string. (RFC3986 2.1)
%%
%% This function is meant to be used for path components.

-spec urlencode(binary()) -> binary().

urlencode(Binary) ->
	skip_enc(Binary, Binary, 0).

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
	Enc = <<$%, ?HEX(C)>>,
	case Len of
		0 ->
			enc(Rest, [Acc|Enc], Orig, Skip + 1, 0);
		_ ->
			Part = binary_part(Orig, Skip, Len),
			enc(Rest, [Acc, Part|Enc], Orig, Skip + Len + 1, 0)
	end;
enc(<<>>, _, Orig, 0, _) ->
	Orig;
enc(<<>>, Acc, _, _, 0) ->
	iolist_to_binary(Acc);
enc(<<>>, Acc, Orig, Skip, Len) ->
	Part = binary_part(Orig, Skip, Len),
	iolist_to_binary([Acc|Part]);
enc(_, _, Orig, Skip, Len) ->
	error({invalid_byte, binary:at(Orig, Skip + Len)}).

-ifdef(TEST).
urlencode_test_() ->
	Tests = [
		{<<255, 0>>, <<"%FF%00">>},
		{<<255, " ">>, <<"%FF%20">>},
		{<<"+">>, <<"+">>},
		{<<"aBc123">>, <<"aBc123">>},
		{<<"!$&'()*+,:;=@-._~">>, <<"!$&'()*+,:;=@-._~">>}
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

horse_urlencode_spaces() ->
	horse:repeat(100000,
		urlencode(<<"Small fast modular HTTP server">>)
	).

horse_urlencode_jp() ->
	horse:repeat(100000,
		urlencode(<<227,131,132,227,130,164,227,131,179,227,130,189,227,
			130,166,227,131,171,227,128,156,232,188,170,229,187,187,227,
			129,153,227,130,139,230,151,139,229,190,139,227,128,156>>)
	).

horse_urlencode_jp_mixed() ->
	horse:repeat(100000,
		urlencode(<<227,131,132,227,130,164,227,131,179,227,130,189,227,
			$1, $2, $3,
			130,166,227,131,171,227,128,156,232,188,170,229,187,187,227,
			129,153,227,130,139,230,151,139,229,190,139,227,128,156>>)
	).

horse_urlencode_mix() ->
	horse:repeat(100000,
		urlencode(<<"Small, fast, modular HTTP server.">>)
	).
-endif.
