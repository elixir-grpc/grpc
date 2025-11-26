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

-module(cow_capsule).

%% Parsing.
-export([parse/1]).

%% Building.
-export([wt_drain_session/0]).
-export([wt_close_session/2]).

-type capsule() ::
	wt_drain_session |
	{wt_close_session, cow_http3:wt_app_error_code(), binary()}.

%% Parsing.

-spec parse(binary())
	-> {ok, capsule(), binary()}
	| {ok, binary()} %% Unknown capsule gets skipped.
	| more
	| {skip, non_neg_integer()} %% Unknown capsule; remaining length to skip.
	| error.

%% @todo Handle DATAGRAM capsules. {datagram, binary()}
parse(<<2:2, 16#78ae:30, 0, Rest/bits>>) ->
	{ok, wt_drain_session, Rest};
parse(<<1:2, 16#2843:14, Rest0/bits>>) when byte_size(Rest0) >= 5 ->
	LenOrError = case Rest0 of
		<<0:2, Len0:6, Rest1/bits>> ->
			{Len0, Rest1};
		<<1:2, Len0:14, Rest1/bits>> when Len0 =< 1028 ->
			{Len0, Rest1};
		%% AppCode is 4 bytes and AppMsg is up to 1024 bytes.
		_ ->
			error
	end,
	case LenOrError of
		{Len1, Rest2} ->
			AppMsgLen = Len1 - 4,
			case Rest2 of
				<<AppCode:32, AppMsg:AppMsgLen/binary, Rest/bits>> ->
					{ok, {wt_close_session, AppCode, AppMsg}, Rest};
				_ ->
					more
			end;
		error ->
			error
	end;
parse(<<>>) ->
	more;
%% Skip unknown capsules.
parse(Data) ->
	%% @todo This can use maybe_expr in OTP-25+.
	case cow_http3:parse_int(Data) of
		more ->
			more;
		{_Type, Rest0} ->
			case cow_http3:parse_int(Rest0) of
				more ->
					more;
				{Len, Rest1} ->
					case Rest1 of
						<<_:Len/unit:8, Rest>> ->
							{ok, Rest};
						_ ->
							{skip, Len - byte_size(Rest1)}
					end
			end
	end.

%% Building.

-spec wt_drain_session() -> binary().

%% @todo Where should I put capsules?
wt_drain_session() ->
	<<2:2, 16#78ae:30, 0>>.

-spec wt_close_session(cow_http3:wt_app_error_code(), iodata()) -> iodata().

wt_close_session(AppCode, <<>>) ->
	<<1:2, 16#2843:14, 4, AppCode:32>>;
wt_close_session(AppCode, AppMsg) ->
	Len = 4 + iolist_size(AppMsg),
	[<<1:2, 16#2843:14>>, cow_http3:encode_int(Len), <<AppCode:32>>, AppMsg].
