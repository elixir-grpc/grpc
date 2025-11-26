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

%% The current implementation is not suitable for use in
%% intermediaries as the information about headers that
%% should never be indexed is currently lost.

-module(cow_hpack).
-dialyzer(no_improper_lists).

-export([init/0]).
-export([init/1]).
-export([set_max_size/2]).

-export([decode/1]).
-export([decode/2]).

-export([encode/1]).
-export([encode/2]).
-export([encode/3]).

-record(state, {
	size = 0 :: non_neg_integer(),
	max_size = 4096 :: non_neg_integer(),
	configured_max_size = 4096 :: non_neg_integer(),
	dyn_table = [] :: [{pos_integer(), {binary(), binary()}}]
}).

-opaque state() :: #state{}.
-export_type([state/0]).

-type encoder_opts() :: #{
	huffman => boolean()
}.
-export_type([encoder_opts/0]).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-endif.

-include("cow_hpack_common.hrl").

%% State initialization.

-spec init() -> state().
init() ->
	#state{}.

-spec init(non_neg_integer()) -> state().
init(MaxSize) ->
	#state{max_size=MaxSize, configured_max_size=MaxSize}.

%% Update the configured max size.
%%
%% When decoding, the local endpoint also needs to send a SETTINGS
%% frame with this value and it is then up to the remote endpoint
%% to decide what actual limit it will use. The actual limit is
%% signaled via dynamic table size updates in the encoded data.
%%
%% When encoding, the local endpoint will call this function after
%% receiving a SETTINGS frame with this value. The encoder will
%% then use this value as the new max after signaling via a dynamic
%% table size update. The value given as argument may be lower
%% than the one received in the SETTINGS.

-spec set_max_size(non_neg_integer(), State) -> State when State::state().
set_max_size(MaxSize, State) ->
	State#state{configured_max_size=MaxSize}.

%% Decoding.

-spec decode(binary()) -> {cow_http:headers(), state()}.
decode(Data) ->
	decode(Data, init()).

-spec decode(binary(), State) -> {cow_http:headers(), State} when State::state().
%% Dynamic table size update is only allowed at the beginning of a HEADERS block.
decode(<< 0:2, 1:1, Rest/bits >>, State=#state{configured_max_size=ConfigMaxSize}) ->
	{MaxSize, Rest2} = dec_int5(Rest),
	if
		MaxSize =< ConfigMaxSize ->
			State2 = table_update_size(MaxSize, State),
			decode(Rest2, State2)
	end;
decode(Data, State) ->
	decode(Data, State, []).

decode(<<>>, State, Acc) ->
	{lists:reverse(Acc), State};
%% Indexed header field representation.
decode(<< 1:1, Rest/bits >>, State, Acc) ->
	dec_indexed(Rest, State, Acc);
%% Literal header field with incremental indexing: new name.
decode(<< 0:1, 1:1, 0:6, Rest/bits >>, State, Acc) ->
	dec_lit_index_new_name(Rest, State, Acc);
%% Literal header field with incremental indexing: indexed name.
decode(<< 0:1, 1:1, Rest/bits >>, State, Acc) ->
	dec_lit_index_indexed_name(Rest, State, Acc);
%% Literal header field without indexing: new name.
decode(<< 0:8, Rest/bits >>, State, Acc) ->
	dec_lit_no_index_new_name(Rest, State, Acc);
%% Literal header field without indexing: indexed name.
decode(<< 0:4, Rest/bits >>, State, Acc) ->
	dec_lit_no_index_indexed_name(Rest, State, Acc);
%% Literal header field never indexed: new name.
%% @todo Keep track of "never indexed" headers.
decode(<< 0:3, 1:1, 0:4, Rest/bits >>, State, Acc) ->
	dec_lit_no_index_new_name(Rest, State, Acc);
%% Literal header field never indexed: indexed name.
%% @todo Keep track of "never indexed" headers.
decode(<< 0:3, 1:1, Rest/bits >>, State, Acc) ->
	dec_lit_no_index_indexed_name(Rest, State, Acc).

%% Indexed header field representation.

%% We do the integer decoding inline where appropriate, falling
%% back to dec_big_int for larger values.
dec_indexed(<<2#1111111:7, 0:1, Int:7, Rest/bits>>, State, Acc) ->
	{Name, Value} = table_get(127 + Int, State),
	decode(Rest, State, [{Name, Value}|Acc]);
dec_indexed(<<2#1111111:7, Rest0/bits>>, State, Acc) ->
	{Index, Rest} = dec_big_int(Rest0, 127, 0),
	{Name, Value} = table_get(Index, State),
	decode(Rest, State, [{Name, Value}|Acc]);
dec_indexed(<<Index:7, Rest/bits>>, State, Acc) ->
	{Name, Value} = table_get(Index, State),
	decode(Rest, State, [{Name, Value}|Acc]).

%% Literal header field with incremental indexing.

dec_lit_index_new_name(Rest, State, Acc) ->
	{Name, Rest2} = dec_str(Rest),
	dec_lit_index(Rest2, State, Acc, Name).

%% We do the integer decoding inline where appropriate, falling
%% back to dec_big_int for larger values.
dec_lit_index_indexed_name(<<2#111111:6, 0:1, Int:7, Rest/bits>>, State, Acc) ->
	Name = table_get_name(63 + Int, State),
	dec_lit_index(Rest, State, Acc, Name);
dec_lit_index_indexed_name(<<2#111111:6, Rest0/bits>>, State, Acc) ->
	{Index, Rest} = dec_big_int(Rest0, 63, 0),
	Name = table_get_name(Index, State),
	dec_lit_index(Rest, State, Acc, Name);
dec_lit_index_indexed_name(<<Index:6, Rest/bits>>, State, Acc) ->
	Name = table_get_name(Index, State),
	dec_lit_index(Rest, State, Acc, Name).

dec_lit_index(Rest, State, Acc, Name) ->
	{Value, Rest2} = dec_str(Rest),
	State2 = table_insert({Name, Value}, State),
	decode(Rest2, State2, [{Name, Value}|Acc]).

%% Literal header field without indexing.

dec_lit_no_index_new_name(Rest, State, Acc) ->
	{Name, Rest2} = dec_str(Rest),
	dec_lit_no_index(Rest2, State, Acc, Name).

%% We do the integer decoding inline where appropriate, falling
%% back to dec_big_int for larger values.
dec_lit_no_index_indexed_name(<<2#1111:4, 0:1, Int:7, Rest/bits>>, State, Acc) ->
	Name = table_get_name(15 + Int, State),
	dec_lit_no_index(Rest, State, Acc, Name);
dec_lit_no_index_indexed_name(<<2#1111:4, Rest0/bits>>, State, Acc) ->
	{Index, Rest} = dec_big_int(Rest0, 15, 0),
	Name = table_get_name(Index, State),
	dec_lit_no_index(Rest, State, Acc, Name);
dec_lit_no_index_indexed_name(<<Index:4, Rest/bits>>, State, Acc) ->
	Name = table_get_name(Index, State),
	dec_lit_no_index(Rest, State, Acc, Name).

dec_lit_no_index(Rest, State, Acc, Name) ->
	{Value, Rest2} = dec_str(Rest),
	decode(Rest2, State, [{Name, Value}|Acc]).

%% @todo Literal header field never indexed.

%% Decode a string.

dec_str(<<0:1, 2#1111111:7, Rest0/bits>>) ->
	{Length, Rest1} = dec_big_int(Rest0, 127, 0),
	<<Str:Length/binary, Rest/bits>> = Rest1,
	{Str, Rest};
dec_str(<<0:1, Length:7, Rest0/bits>>) ->
	<<Str:Length/binary, Rest/bits>> = Rest0,
	{Str, Rest};
dec_str(<<1:1, 2#1111111:7, Rest0/bits>>) ->
	{Length, Rest} = dec_big_int(Rest0, 127, 0),
	dec_huffman(Rest, Length, 0, <<>>);
dec_str(<<1:1, Length:7, Rest/bits>>) ->
	dec_huffman(Rest, Length, 0, <<>>).

-ifdef(TEST).
%% Test case extracted from h2spec.
decode_reject_eos_test() ->
	{'EXIT', _} = (catch decode(<<16#0085f2b24a84ff874951fffffffa7f:120>>)),
	ok.

req_decode_test() ->
	%% First request (raw then huffman).
	{Headers1, State1} = decode(<< 16#828684410f7777772e6578616d706c652e636f6d:160 >>),
	{Headers1, State1} = decode(<< 16#828684418cf1e3c2e5f23a6ba0ab90f4ff:136 >>),
	Headers1 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>},
		{<<":authority">>, <<"www.example.com">>}
	],
	#state{size=57, dyn_table=[{57,{<<":authority">>, <<"www.example.com">>}}]} = State1,
	%% Second request (raw then huffman).
	{Headers2, State2} = decode(<< 16#828684be58086e6f2d6361636865:112 >>, State1),
	{Headers2, State2} = decode(<< 16#828684be5886a8eb10649cbf:96 >>, State1),
	Headers2 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>},
		{<<":authority">>, <<"www.example.com">>},
		{<<"cache-control">>, <<"no-cache">>}
	],
	#state{size=110, dyn_table=[
		{53,{<<"cache-control">>, <<"no-cache">>}},
		{57,{<<":authority">>, <<"www.example.com">>}}]} = State2,
	%% Third request (raw then huffman).
	{Headers3, State3} = decode(<< 16#828785bf400a637573746f6d2d6b65790c637573746f6d2d76616c7565:232 >>, State2),
	{Headers3, State3} = decode(<< 16#828785bf408825a849e95ba97d7f8925a849e95bb8e8b4bf:192 >>, State2),
	Headers3 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/index.html">>},
		{<<":authority">>, <<"www.example.com">>},
		{<<"custom-key">>, <<"custom-value">>}
	],
	#state{size=164, dyn_table=[
		{54,{<<"custom-key">>, <<"custom-value">>}},
		{53,{<<"cache-control">>, <<"no-cache">>}},
		{57,{<<":authority">>, <<"www.example.com">>}}]} = State3,
	ok.

resp_decode_test() ->
	%% Use a max_size of 256 to trigger header evictions.
	State0 = init(256),
	%% First response (raw then huffman).
	{Headers1, State1} = decode(<< 16#4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d:560 >>, State0),
	{Headers1, State1} = decode(<< 16#488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3:432 >>, State0),
	Headers1 = [
		{<<":status">>, <<"302">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	#state{size=222, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State1,
	%% Second response (raw then huffman).
	{Headers2, State2} = decode(<< 16#4803333037c1c0bf:64 >>, State1),
	{Headers2, State2} = decode(<< 16#4883640effc1c0bf:64 >>, State1),
	Headers2 = [
		{<<":status">>, <<"307">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	#state{size=222, dyn_table=[
		{42,{<<":status">>, <<"307">>}},
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}}]} = State2,
	%% Third response (raw then huffman).
	{Headers3, State3} = decode(<< 16#88c1611d4d6f6e2c203231204f637420323031332032303a31333a323220474d54c05a04677a69707738666f6f3d4153444a4b48514b425a584f5157454f50495541585157454f49553b206d61782d6167653d333630303b2076657273696f6e3d31:784 >>, State2),
	{Headers3, State3} = decode(<< 16#88c16196d07abe941054d444a8200595040b8166e084a62d1bffc05a839bd9ab77ad94e7821dd7f2e6c7b335dfdfcd5b3960d5af27087f3672c1ab270fb5291f9587316065c003ed4ee5b1063d5007:632 >>, State2),
	Headers3 = [
		{<<":status">>, <<"200">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:22 GMT">>},
		{<<"location">>, <<"https://www.example.com">>},
		{<<"content-encoding">>, <<"gzip">>},
		{<<"set-cookie">>, <<"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1">>}
	],
	#state{size=215, dyn_table=[
		{98,{<<"set-cookie">>, <<"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1">>}},
		{52,{<<"content-encoding">>, <<"gzip">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:22 GMT">>}}]} = State3,
	ok.

table_update_decode_test() ->
	%% Use a max_size of 256 to trigger header evictions
	%% when the code is not updating the max size.
	State0 = init(256),
	%% First response (raw then huffman).
	{Headers1, State1} = decode(<< 16#4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d:560 >>, State0),
	{Headers1, State1} = decode(<< 16#488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3:432 >>, State0),
	Headers1 = [
		{<<":status">>, <<"302">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	#state{size=222, configured_max_size=256, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State1,
	%% Set a new configured max_size to avoid header evictions.
	State2 = set_max_size(512, State1),
	%% Second response with the table size update (raw then huffman).
	MaxSize = enc_big_int(512 - 31, <<>>),
	{Headers2, State3} = decode(
		iolist_to_binary([<< 2#00111111>>, MaxSize, <<16#4803333037c1c0bf:64>>]),
		State2),
	{Headers2, State3} = decode(
		iolist_to_binary([<< 2#00111111>>, MaxSize, <<16#4883640effc1c0bf:64>>]),
		State2),
	Headers2 = [
		{<<":status">>, <<"307">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	#state{size=264, configured_max_size=512, dyn_table=[
		{42,{<<":status">>, <<"307">>}},
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State3,
	ok.

table_update_decode_smaller_test() ->
	%% Use a max_size of 256 to trigger header evictions
	%% when the code is not updating the max size.
	State0 = init(256),
	%% First response (raw then huffman).
	{Headers1, State1} = decode(<< 16#4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d:560 >>, State0),
	{Headers1, State1} = decode(<< 16#488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3:432 >>, State0),
	Headers1 = [
		{<<":status">>, <<"302">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	#state{size=222, configured_max_size=256, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State1,
	%% Set a new configured max_size to avoid header evictions.
	State2 = set_max_size(512, State1),
	%% Second response with the table size update smaller than the limit (raw then huffman).
	MaxSize = enc_big_int(400 - 31, <<>>),
	{Headers2, State3} = decode(
		iolist_to_binary([<< 2#00111111>>, MaxSize, <<16#4803333037c1c0bf:64>>]),
		State2),
	{Headers2, State3} = decode(
		iolist_to_binary([<< 2#00111111>>, MaxSize, <<16#4883640effc1c0bf:64>>]),
		State2),
	Headers2 = [
		{<<":status">>, <<"307">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	#state{size=264, configured_max_size=512, dyn_table=[
		{42,{<<":status">>, <<"307">>}},
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State3,
	ok.

table_update_decode_too_large_test() ->
	%% Use a max_size of 256 to trigger header evictions
	%% when the code is not updating the max size.
	State0 = init(256),
	%% First response (raw then huffman).
	{Headers1, State1} = decode(<< 16#4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d:560 >>, State0),
	{Headers1, State1} = decode(<< 16#488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3:432 >>, State0),
	Headers1 = [
		{<<":status">>, <<"302">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	#state{size=222, configured_max_size=256, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State1,
	%% Set a new configured max_size to avoid header evictions.
	State2 = set_max_size(512, State1),
	%% Second response with the table size update (raw then huffman).
	MaxSize = enc_big_int(1024 - 31, <<>>),
	{'EXIT', _} = (catch decode(
		iolist_to_binary([<< 2#00111111>>, MaxSize, <<16#4803333037c1c0bf:64>>]),
		State2)),
	{'EXIT', _} = (catch decode(
		iolist_to_binary([<< 2#00111111>>, MaxSize, <<16#4883640effc1c0bf:64>>]),
		State2)),
	ok.

table_update_decode_zero_test() ->
	State0 = init(256),
	%% First response (raw then huffman).
	{Headers1, State1} = decode(<< 16#4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d:560 >>, State0),
	{Headers1, State1} = decode(<< 16#488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3:432 >>, State0),
	Headers1 = [
		{<<":status">>, <<"302">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	#state{size=222, configured_max_size=256, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State1,
	%% Set a new configured max_size to avoid header evictions.
	State2 = set_max_size(512, State1),
	%% Second response with the table size update (raw then huffman).
	%% We set the table size to 0 to evict all values before setting
	%% it to 512 so we only get the second request indexed.
	MaxSize = enc_big_int(512 - 31, <<>>),
	{Headers1, State3} = decode(iolist_to_binary([
		<<2#00100000, 2#00111111>>, MaxSize,
		<<16#4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d:560>>]),
		State2),
	{Headers1, State3} = decode(iolist_to_binary([
		<<2#00100000, 2#00111111>>, MaxSize,
		<<16#488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3:432>>]),
		State2),
	#state{size=222, configured_max_size=512, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State3,
	ok.

horse_decode_raw() ->
	horse:repeat(20000,
		do_horse_decode_raw()
	).

do_horse_decode_raw() ->
	{_, State1} = decode(<<16#828684410f7777772e6578616d706c652e636f6d:160>>),
	{_, State2} = decode(<<16#828684be58086e6f2d6361636865:112>>, State1),
	{_, _} = decode(<<16#828785bf400a637573746f6d2d6b65790c637573746f6d2d76616c7565:232>>, State2),
	ok.

horse_decode_huffman() ->
	horse:repeat(20000,
		do_horse_decode_huffman()
	).

do_horse_decode_huffman() ->
	{_, State1} = decode(<<16#828684418cf1e3c2e5f23a6ba0ab90f4ff:136>>),
	{_, State2} = decode(<<16#828684be5886a8eb10649cbf:96>>, State1),
	{_, _} = decode(<<16#828785bf408825a849e95ba97d7f8925a849e95bb8e8b4bf:192>>, State2),
	ok.
-endif.

%% Encoding.

-spec encode(cow_http:headers()) -> {iodata(), state()}.
encode(Headers) ->
	encode(Headers, init(), huffman, []).

-spec encode(cow_http:headers(), State) -> {iodata(), State} when State::state().
encode(Headers, State=#state{max_size=MaxSize, configured_max_size=MaxSize}) ->
	encode(Headers, State, huffman, []);
encode(Headers, State0=#state{configured_max_size=MaxSize}) ->
	State1 = table_update_size(MaxSize, State0),
	{Data, State} = encode(Headers, State1, huffman, []),
	{[enc_int5(MaxSize, 2#001)|Data], State}.

-spec encode(cow_http:headers(), State, encoder_opts())
	-> {iodata(), State} when State::state().
encode(Headers, State=#state{max_size=MaxSize, configured_max_size=MaxSize}, Opts) ->
	encode(Headers, State, huffman_opt(Opts), []);
encode(Headers, State0=#state{configured_max_size=MaxSize}, Opts) ->
	State1 = table_update_size(MaxSize, State0),
	{Data, State} = encode(Headers, State1, huffman_opt(Opts), []),
	{[enc_int5(MaxSize, 2#001)|Data], State}.

huffman_opt(#{huffman := false}) -> no_huffman;
huffman_opt(_) -> huffman.

%% @todo Handle cases where no/never indexing is expected.
encode([], State, _, Acc) ->
	{lists:reverse(Acc), State};
encode([{Name, Value0}|Tail], State, HuffmanOpt, Acc) ->
	%% We conditionally call iolist_to_binary/1 because a small
	%% but noticeable speed improvement happens when we do this.
	Value = if
		is_binary(Value0) -> Value0;
		true -> iolist_to_binary(Value0)
	end,
	Header = {Name, Value},
	case table_find(Header, State) of
		%% Indexed header field representation.
		{field, Index} ->
			encode(Tail, State, HuffmanOpt,
				[enc_int7(Index, 2#1)|Acc]);
		%% Literal header field representation: indexed name.
		{name, Index} ->
			State2 = table_insert(Header, State),
			encode(Tail, State2, HuffmanOpt,
				[[enc_int6(Index, 2#01)|enc_str(Value, HuffmanOpt)]|Acc]);
		%% Literal header field representation: new name.
		not_found ->
			State2 = table_insert(Header, State),
			encode(Tail, State2, HuffmanOpt,
				[[<< 0:1, 1:1, 0:6 >>|[enc_str(Name, HuffmanOpt)|enc_str(Value, HuffmanOpt)]]|Acc])
	end.

-ifdef(TEST).
req_encode_test() ->
	%% First request (raw then huffman).
	Headers1 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>},
		{<<":authority">>, <<"www.example.com">>}
	],
	{Raw1, State1} = encode(Headers1, init(), #{huffman => false}),
	<< 16#828684410f7777772e6578616d706c652e636f6d:160 >> = iolist_to_binary(Raw1),
	{Huff1, State1} = encode(Headers1),
	<< 16#828684418cf1e3c2e5f23a6ba0ab90f4ff:136 >> = iolist_to_binary(Huff1),
	#state{size=57, dyn_table=[{57,{<<":authority">>, <<"www.example.com">>}}]} = State1,
	%% Second request (raw then huffman).
	Headers2 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>},
		{<<":authority">>, <<"www.example.com">>},
		{<<"cache-control">>, <<"no-cache">>}
	],
	{Raw2, State2} = encode(Headers2, State1, #{huffman => false}),
	<< 16#828684be58086e6f2d6361636865:112 >> = iolist_to_binary(Raw2),
	{Huff2, State2} = encode(Headers2, State1),
	<< 16#828684be5886a8eb10649cbf:96 >> = iolist_to_binary(Huff2),
	#state{size=110, dyn_table=[
		{53,{<<"cache-control">>, <<"no-cache">>}},
		{57,{<<":authority">>, <<"www.example.com">>}}]} = State2,
	%% Third request (raw then huffman).
	Headers3 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/index.html">>},
		{<<":authority">>, <<"www.example.com">>},
		{<<"custom-key">>, <<"custom-value">>}
	],
	{Raw3, State3} = encode(Headers3, State2, #{huffman => false}),
	<< 16#828785bf400a637573746f6d2d6b65790c637573746f6d2d76616c7565:232 >> = iolist_to_binary(Raw3),
	{Huff3, State3} = encode(Headers3, State2),
	<< 16#828785bf408825a849e95ba97d7f8925a849e95bb8e8b4bf:192 >> = iolist_to_binary(Huff3),
	#state{size=164, dyn_table=[
		{54,{<<"custom-key">>, <<"custom-value">>}},
		{53,{<<"cache-control">>, <<"no-cache">>}},
		{57,{<<":authority">>, <<"www.example.com">>}}]} = State3,
	ok.

resp_encode_test() ->
	%% Use a max_size of 256 to trigger header evictions.
	State0 = init(256),
	%% First response (raw then huffman).
	Headers1 = [
		{<<":status">>, <<"302">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	{Raw1, State1} = encode(Headers1, State0, #{huffman => false}),
	<< 16#4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d:560 >> = iolist_to_binary(Raw1),
	{Huff1, State1} = encode(Headers1, State0),
	<< 16#488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3:432 >> = iolist_to_binary(Huff1),
	#state{size=222, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State1,
	%% Second response (raw then huffman).
	Headers2 = [
		{<<":status">>, <<"307">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	{Raw2, State2} = encode(Headers2, State1, #{huffman => false}),
	<< 16#4803333037c1c0bf:64 >> = iolist_to_binary(Raw2),
	{Huff2, State2} = encode(Headers2, State1),
	<< 16#4883640effc1c0bf:64 >> = iolist_to_binary(Huff2),
	#state{size=222, dyn_table=[
		{42,{<<":status">>, <<"307">>}},
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}}]} = State2,
	%% Third response (raw then huffman).
	Headers3 = [
		{<<":status">>, <<"200">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:22 GMT">>},
		{<<"location">>, <<"https://www.example.com">>},
		{<<"content-encoding">>, <<"gzip">>},
		{<<"set-cookie">>, <<"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1">>}
	],
	{Raw3, State3} = encode(Headers3, State2, #{huffman => false}),
	<< 16#88c1611d4d6f6e2c203231204f637420323031332032303a31333a323220474d54c05a04677a69707738666f6f3d4153444a4b48514b425a584f5157454f50495541585157454f49553b206d61782d6167653d333630303b2076657273696f6e3d31:784 >> = iolist_to_binary(Raw3),
	{Huff3, State3} = encode(Headers3, State2),
	<< 16#88c16196d07abe941054d444a8200595040b8166e084a62d1bffc05a839bd9ab77ad94e7821dd7f2e6c7b335dfdfcd5b3960d5af27087f3672c1ab270fb5291f9587316065c003ed4ee5b1063d5007:632 >> = iolist_to_binary(Huff3),
	#state{size=215, dyn_table=[
		{98,{<<"set-cookie">>, <<"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1">>}},
		{52,{<<"content-encoding">>, <<"gzip">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:22 GMT">>}}]} = State3,
	ok.

%% This test assumes that table updates work correctly when decoding.
table_update_encode_test() ->
	%% Use a max_size of 256 to trigger header evictions
	%% when the code is not updating the max size.
	DecState0 = EncState0 = init(256),
	%% First response.
	Headers1 = [
		{<<":status">>, <<"302">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	{Encoded1, EncState1} = encode(Headers1, EncState0),
	{Headers1, DecState1} = decode(iolist_to_binary(Encoded1), DecState0),
	#state{size=222, configured_max_size=256, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = DecState1,
	#state{size=222, configured_max_size=256, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = EncState1,
	%% Set a new configured max_size to avoid header evictions.
	DecState2 = set_max_size(512, DecState1),
	EncState2 = set_max_size(512, EncState1),
	%% Second response.
	Headers2 = [
		{<<":status">>, <<"307">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	{Encoded2, EncState3} = encode(Headers2, EncState2),
	{Headers2, DecState3} = decode(iolist_to_binary(Encoded2), DecState2),
	#state{size=264, max_size=512, dyn_table=[
		{42,{<<":status">>, <<"307">>}},
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = DecState3,
	#state{size=264, max_size=512, dyn_table=[
		{42,{<<":status">>, <<"307">>}},
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = EncState3,
	ok.

%% Check that encode/2 is using the new table size after calling
%% set_max_size/1 and that adding entries larger than the max size
%% results in an empty table.
table_update_encode_max_size_0_test() ->
	%% Encoding starts with default max size
	EncState0 = init(),
	%% Decoding starts with max size of 0
	DecState0 = init(0),
	%% First request.
	Headers1 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>},
		{<<":authority">>, <<"www.example.com">>}
	],
	{Encoded1, EncState1} = encode(Headers1, EncState0),
	{Headers1, DecState1} = decode(iolist_to_binary(Encoded1), DecState0),
	#state{size=57, dyn_table=[{57,{<<":authority">>, <<"www.example.com">>}}]} = EncState1,
	#state{size=0, dyn_table=[]} = DecState1,
	%% Settings received after the first request.
	EncState2 = set_max_size(0, EncState1),
	#state{configured_max_size=0, max_size=4096,
	       size=57, dyn_table=[{57,{<<":authority">>, <<"www.example.com">>}}]} = EncState2,
	%% Second request.
	Headers2 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>},
		{<<":authority">>, <<"www.example.com">>},
		{<<"cache-control">>, <<"no-cache">>}
	],
	{Encoded2, EncState3} = encode(Headers2, EncState2),
	{Headers2, DecState2} = decode(iolist_to_binary(Encoded2), DecState1),
	#state{configured_max_size=0, max_size=0, size=0, dyn_table=[]} = EncState3,
	#state{size=0, dyn_table=[]} = DecState2,
	ok.

encode_iolist_test() ->
	Headers = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>},
		{<<":authority">>, <<"www.example.com">>},
		{<<"content-type">>, [<<"image">>,<<"/">>,<<"png">>,<<>>]}
	],
	{_, _} = encode(Headers),
	ok.

horse_encode_raw() ->
	horse:repeat(20000,
		do_horse_encode_raw()
	).

do_horse_encode_raw() ->
	Headers1 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>},
		{<<":authority">>, <<"www.example.com">>}
	],
	{_, State1} = encode(Headers1, init(), #{huffman => false}),
	Headers2 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>},
		{<<":authority">>, <<"www.example.com">>},
		{<<"cache-control">>, <<"no-cache">>}
	],
	{_, State2} = encode(Headers2, State1, #{huffman => false}),
	Headers3 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/index.html">>},
		{<<":authority">>, <<"www.example.com">>},
		{<<"custom-key">>, <<"custom-value">>}
	],
	{_, _} = encode(Headers3, State2, #{huffman => false}),
	ok.

horse_encode_huffman() ->
	horse:repeat(20000,
		do_horse_encode_huffman()
	).

do_horse_encode_huffman() ->
	Headers1 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>},
		{<<":authority">>, <<"www.example.com">>}
	],
	{_, State1} = encode(Headers1),
	Headers2 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>},
		{<<":authority">>, <<"www.example.com">>},
		{<<"cache-control">>, <<"no-cache">>}
	],
	{_, State2} = encode(Headers2, State1),
	Headers3 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/index.html">>},
		{<<":authority">>, <<"www.example.com">>},
		{<<"custom-key">>, <<"custom-value">>}
	],
	{_, _} = encode(Headers3, State2),
	ok.
-endif.

%% Static and dynamic tables.

%% @todo There must be a more efficient way.
table_find(Header = {Name, _}, State) ->
	case table_find_field(Header, State) of
		not_found ->
			case table_find_name(Name, State) of
				NotFound = not_found ->
					NotFound;
				Found ->
					{name, Found}
			end;
		Found ->
			{field, Found}
	end.

table_find_field({<<":authority">>, <<>>}, _) -> 1;
table_find_field({<<":method">>, <<"GET">>}, _) -> 2;
table_find_field({<<":method">>, <<"POST">>}, _) -> 3;
table_find_field({<<":path">>, <<"/">>}, _) -> 4;
table_find_field({<<":path">>, <<"/index.html">>}, _) -> 5;
table_find_field({<<":scheme">>, <<"http">>}, _) -> 6;
table_find_field({<<":scheme">>, <<"https">>}, _) -> 7;
table_find_field({<<":status">>, <<"200">>}, _) -> 8;
table_find_field({<<":status">>, <<"204">>}, _) -> 9;
table_find_field({<<":status">>, <<"206">>}, _) -> 10;
table_find_field({<<":status">>, <<"304">>}, _) -> 11;
table_find_field({<<":status">>, <<"400">>}, _) -> 12;
table_find_field({<<":status">>, <<"404">>}, _) -> 13;
table_find_field({<<":status">>, <<"500">>}, _) -> 14;
table_find_field({<<"accept-charset">>, <<>>}, _) -> 15;
table_find_field({<<"accept-encoding">>, <<"gzip, deflate">>}, _) -> 16;
table_find_field({<<"accept-language">>, <<>>}, _) -> 17;
table_find_field({<<"accept-ranges">>, <<>>}, _) -> 18;
table_find_field({<<"accept">>, <<>>}, _) -> 19;
table_find_field({<<"access-control-allow-origin">>, <<>>}, _) -> 20;
table_find_field({<<"age">>, <<>>}, _) -> 21;
table_find_field({<<"allow">>, <<>>}, _) -> 22;
table_find_field({<<"authorization">>, <<>>}, _) -> 23;
table_find_field({<<"cache-control">>, <<>>}, _) -> 24;
table_find_field({<<"content-disposition">>, <<>>}, _) -> 25;
table_find_field({<<"content-encoding">>, <<>>}, _) -> 26;
table_find_field({<<"content-language">>, <<>>}, _) -> 27;
table_find_field({<<"content-length">>, <<>>}, _) -> 28;
table_find_field({<<"content-location">>, <<>>}, _) -> 29;
table_find_field({<<"content-range">>, <<>>}, _) -> 30;
table_find_field({<<"content-type">>, <<>>}, _) -> 31;
table_find_field({<<"cookie">>, <<>>}, _) -> 32;
table_find_field({<<"date">>, <<>>}, _) -> 33;
table_find_field({<<"etag">>, <<>>}, _) -> 34;
table_find_field({<<"expect">>, <<>>}, _) -> 35;
table_find_field({<<"expires">>, <<>>}, _) -> 36;
table_find_field({<<"from">>, <<>>}, _) -> 37;
table_find_field({<<"host">>, <<>>}, _) -> 38;
table_find_field({<<"if-match">>, <<>>}, _) -> 39;
table_find_field({<<"if-modified-since">>, <<>>}, _) -> 40;
table_find_field({<<"if-none-match">>, <<>>}, _) -> 41;
table_find_field({<<"if-range">>, <<>>}, _) -> 42;
table_find_field({<<"if-unmodified-since">>, <<>>}, _) -> 43;
table_find_field({<<"last-modified">>, <<>>}, _) -> 44;
table_find_field({<<"link">>, <<>>}, _) -> 45;
table_find_field({<<"location">>, <<>>}, _) -> 46;
table_find_field({<<"max-forwards">>, <<>>}, _) -> 47;
table_find_field({<<"proxy-authenticate">>, <<>>}, _) -> 48;
table_find_field({<<"proxy-authorization">>, <<>>}, _) -> 49;
table_find_field({<<"range">>, <<>>}, _) -> 50;
table_find_field({<<"referer">>, <<>>}, _) -> 51;
table_find_field({<<"refresh">>, <<>>}, _) -> 52;
table_find_field({<<"retry-after">>, <<>>}, _) -> 53;
table_find_field({<<"server">>, <<>>}, _) -> 54;
table_find_field({<<"set-cookie">>, <<>>}, _) -> 55;
table_find_field({<<"strict-transport-security">>, <<>>}, _) -> 56;
table_find_field({<<"transfer-encoding">>, <<>>}, _) -> 57;
table_find_field({<<"user-agent">>, <<>>}, _) -> 58;
table_find_field({<<"vary">>, <<>>}, _) -> 59;
table_find_field({<<"via">>, <<>>}, _) -> 60;
table_find_field({<<"www-authenticate">>, <<>>}, _) -> 61;
table_find_field(Header, #state{dyn_table=DynamicTable}) ->
	table_find_field_dyn(Header, DynamicTable, 62).

table_find_field_dyn(_, [], _) -> not_found;
table_find_field_dyn(Header, [{_, Header}|_], Index) -> Index;
table_find_field_dyn(Header, [_|Tail], Index) -> table_find_field_dyn(Header, Tail, Index + 1).

table_find_name(<<":authority">>, _) -> 1;
table_find_name(<<":method">>, _) -> 2;
table_find_name(<<":path">>, _) -> 4;
table_find_name(<<":scheme">>, _) -> 6;
table_find_name(<<":status">>, _) -> 8;
table_find_name(<<"accept-charset">>, _) -> 15;
table_find_name(<<"accept-encoding">>, _) -> 16;
table_find_name(<<"accept-language">>, _) -> 17;
table_find_name(<<"accept-ranges">>, _) -> 18;
table_find_name(<<"accept">>, _) -> 19;
table_find_name(<<"access-control-allow-origin">>, _) -> 20;
table_find_name(<<"age">>, _) -> 21;
table_find_name(<<"allow">>, _) -> 22;
table_find_name(<<"authorization">>, _) -> 23;
table_find_name(<<"cache-control">>, _) -> 24;
table_find_name(<<"content-disposition">>, _) -> 25;
table_find_name(<<"content-encoding">>, _) -> 26;
table_find_name(<<"content-language">>, _) -> 27;
table_find_name(<<"content-length">>, _) -> 28;
table_find_name(<<"content-location">>, _) -> 29;
table_find_name(<<"content-range">>, _) -> 30;
table_find_name(<<"content-type">>, _) -> 31;
table_find_name(<<"cookie">>, _) -> 32;
table_find_name(<<"date">>, _) -> 33;
table_find_name(<<"etag">>, _) -> 34;
table_find_name(<<"expect">>, _) -> 35;
table_find_name(<<"expires">>, _) -> 36;
table_find_name(<<"from">>, _) -> 37;
table_find_name(<<"host">>, _) -> 38;
table_find_name(<<"if-match">>, _) -> 39;
table_find_name(<<"if-modified-since">>, _) -> 40;
table_find_name(<<"if-none-match">>, _) -> 41;
table_find_name(<<"if-range">>, _) -> 42;
table_find_name(<<"if-unmodified-since">>, _) -> 43;
table_find_name(<<"last-modified">>, _) -> 44;
table_find_name(<<"link">>, _) -> 45;
table_find_name(<<"location">>, _) -> 46;
table_find_name(<<"max-forwards">>, _) -> 47;
table_find_name(<<"proxy-authenticate">>, _) -> 48;
table_find_name(<<"proxy-authorization">>, _) -> 49;
table_find_name(<<"range">>, _) -> 50;
table_find_name(<<"referer">>, _) -> 51;
table_find_name(<<"refresh">>, _) -> 52;
table_find_name(<<"retry-after">>, _) -> 53;
table_find_name(<<"server">>, _) -> 54;
table_find_name(<<"set-cookie">>, _) -> 55;
table_find_name(<<"strict-transport-security">>, _) -> 56;
table_find_name(<<"transfer-encoding">>, _) -> 57;
table_find_name(<<"user-agent">>, _) -> 58;
table_find_name(<<"vary">>, _) -> 59;
table_find_name(<<"via">>, _) -> 60;
table_find_name(<<"www-authenticate">>, _) -> 61;
table_find_name(Name, #state{dyn_table=DynamicTable}) ->
	table_find_name_dyn(Name, DynamicTable, 62).

table_find_name_dyn(_, [], _) -> not_found;
table_find_name_dyn(Name, [{Name, _}|_], Index) -> Index;
table_find_name_dyn(Name, [_|Tail], Index) -> table_find_name_dyn(Name, Tail, Index + 1).

table_get(1, _) -> {<<":authority">>, <<>>};
table_get(2, _) -> {<<":method">>, <<"GET">>};
table_get(3, _) -> {<<":method">>, <<"POST">>};
table_get(4, _) -> {<<":path">>, <<"/">>};
table_get(5, _) -> {<<":path">>, <<"/index.html">>};
table_get(6, _) -> {<<":scheme">>, <<"http">>};
table_get(7, _) -> {<<":scheme">>, <<"https">>};
table_get(8, _) -> {<<":status">>, <<"200">>};
table_get(9, _) -> {<<":status">>, <<"204">>};
table_get(10, _) -> {<<":status">>, <<"206">>};
table_get(11, _) -> {<<":status">>, <<"304">>};
table_get(12, _) -> {<<":status">>, <<"400">>};
table_get(13, _) -> {<<":status">>, <<"404">>};
table_get(14, _) -> {<<":status">>, <<"500">>};
table_get(15, _) -> {<<"accept-charset">>, <<>>};
table_get(16, _) -> {<<"accept-encoding">>, <<"gzip, deflate">>};
table_get(17, _) -> {<<"accept-language">>, <<>>};
table_get(18, _) -> {<<"accept-ranges">>, <<>>};
table_get(19, _) -> {<<"accept">>, <<>>};
table_get(20, _) -> {<<"access-control-allow-origin">>, <<>>};
table_get(21, _) -> {<<"age">>, <<>>};
table_get(22, _) -> {<<"allow">>, <<>>};
table_get(23, _) -> {<<"authorization">>, <<>>};
table_get(24, _) -> {<<"cache-control">>, <<>>};
table_get(25, _) -> {<<"content-disposition">>, <<>>};
table_get(26, _) -> {<<"content-encoding">>, <<>>};
table_get(27, _) -> {<<"content-language">>, <<>>};
table_get(28, _) -> {<<"content-length">>, <<>>};
table_get(29, _) -> {<<"content-location">>, <<>>};
table_get(30, _) -> {<<"content-range">>, <<>>};
table_get(31, _) -> {<<"content-type">>, <<>>};
table_get(32, _) -> {<<"cookie">>, <<>>};
table_get(33, _) -> {<<"date">>, <<>>};
table_get(34, _) -> {<<"etag">>, <<>>};
table_get(35, _) -> {<<"expect">>, <<>>};
table_get(36, _) -> {<<"expires">>, <<>>};
table_get(37, _) -> {<<"from">>, <<>>};
table_get(38, _) -> {<<"host">>, <<>>};
table_get(39, _) -> {<<"if-match">>, <<>>};
table_get(40, _) -> {<<"if-modified-since">>, <<>>};
table_get(41, _) -> {<<"if-none-match">>, <<>>};
table_get(42, _) -> {<<"if-range">>, <<>>};
table_get(43, _) -> {<<"if-unmodified-since">>, <<>>};
table_get(44, _) -> {<<"last-modified">>, <<>>};
table_get(45, _) -> {<<"link">>, <<>>};
table_get(46, _) -> {<<"location">>, <<>>};
table_get(47, _) -> {<<"max-forwards">>, <<>>};
table_get(48, _) -> {<<"proxy-authenticate">>, <<>>};
table_get(49, _) -> {<<"proxy-authorization">>, <<>>};
table_get(50, _) -> {<<"range">>, <<>>};
table_get(51, _) -> {<<"referer">>, <<>>};
table_get(52, _) -> {<<"refresh">>, <<>>};
table_get(53, _) -> {<<"retry-after">>, <<>>};
table_get(54, _) -> {<<"server">>, <<>>};
table_get(55, _) -> {<<"set-cookie">>, <<>>};
table_get(56, _) -> {<<"strict-transport-security">>, <<>>};
table_get(57, _) -> {<<"transfer-encoding">>, <<>>};
table_get(58, _) -> {<<"user-agent">>, <<>>};
table_get(59, _) -> {<<"vary">>, <<>>};
table_get(60, _) -> {<<"via">>, <<>>};
table_get(61, _) -> {<<"www-authenticate">>, <<>>};
table_get(Index, #state{dyn_table=DynamicTable}) ->
	{_, Header} = lists:nth(Index - 61, DynamicTable),
	Header.

table_get_name(1, _) -> <<":authority">>;
table_get_name(2, _) -> <<":method">>;
table_get_name(3, _) -> <<":method">>;
table_get_name(4, _) -> <<":path">>;
table_get_name(5, _) -> <<":path">>;
table_get_name(6, _) -> <<":scheme">>;
table_get_name(7, _) -> <<":scheme">>;
table_get_name(8, _) -> <<":status">>;
table_get_name(9, _) -> <<":status">>;
table_get_name(10, _) -> <<":status">>;
table_get_name(11, _) -> <<":status">>;
table_get_name(12, _) -> <<":status">>;
table_get_name(13, _) -> <<":status">>;
table_get_name(14, _) -> <<":status">>;
table_get_name(15, _) -> <<"accept-charset">>;
table_get_name(16, _) -> <<"accept-encoding">>;
table_get_name(17, _) -> <<"accept-language">>;
table_get_name(18, _) -> <<"accept-ranges">>;
table_get_name(19, _) -> <<"accept">>;
table_get_name(20, _) -> <<"access-control-allow-origin">>;
table_get_name(21, _) -> <<"age">>;
table_get_name(22, _) -> <<"allow">>;
table_get_name(23, _) -> <<"authorization">>;
table_get_name(24, _) -> <<"cache-control">>;
table_get_name(25, _) -> <<"content-disposition">>;
table_get_name(26, _) -> <<"content-encoding">>;
table_get_name(27, _) -> <<"content-language">>;
table_get_name(28, _) -> <<"content-length">>;
table_get_name(29, _) -> <<"content-location">>;
table_get_name(30, _) -> <<"content-range">>;
table_get_name(31, _) -> <<"content-type">>;
table_get_name(32, _) -> <<"cookie">>;
table_get_name(33, _) -> <<"date">>;
table_get_name(34, _) -> <<"etag">>;
table_get_name(35, _) -> <<"expect">>;
table_get_name(36, _) -> <<"expires">>;
table_get_name(37, _) -> <<"from">>;
table_get_name(38, _) -> <<"host">>;
table_get_name(39, _) -> <<"if-match">>;
table_get_name(40, _) -> <<"if-modified-since">>;
table_get_name(41, _) -> <<"if-none-match">>;
table_get_name(42, _) -> <<"if-range">>;
table_get_name(43, _) -> <<"if-unmodified-since">>;
table_get_name(44, _) -> <<"last-modified">>;
table_get_name(45, _) -> <<"link">>;
table_get_name(46, _) -> <<"location">>;
table_get_name(47, _) -> <<"max-forwards">>;
table_get_name(48, _) -> <<"proxy-authenticate">>;
table_get_name(49, _) -> <<"proxy-authorization">>;
table_get_name(50, _) -> <<"range">>;
table_get_name(51, _) -> <<"referer">>;
table_get_name(52, _) -> <<"refresh">>;
table_get_name(53, _) -> <<"retry-after">>;
table_get_name(54, _) -> <<"server">>;
table_get_name(55, _) -> <<"set-cookie">>;
table_get_name(56, _) -> <<"strict-transport-security">>;
table_get_name(57, _) -> <<"transfer-encoding">>;
table_get_name(58, _) -> <<"user-agent">>;
table_get_name(59, _) -> <<"vary">>;
table_get_name(60, _) -> <<"via">>;
table_get_name(61, _) -> <<"www-authenticate">>;
table_get_name(Index, #state{dyn_table=DynamicTable}) ->
	{_, {Name, _}} = lists:nth(Index - 61, DynamicTable),
	Name.

table_insert(Entry = {Name, Value}, State=#state{size=Size, max_size=MaxSize, dyn_table=DynamicTable}) ->
	EntrySize = byte_size(Name) + byte_size(Value) + 32,
	if
		EntrySize + Size =< MaxSize ->
			%% Add entry without eviction
			State#state{size=Size + EntrySize, dyn_table=[{EntrySize, Entry}|DynamicTable]};
		EntrySize =< MaxSize ->
			%% Evict, then add entry
			{DynamicTable2, Size2} = table_resize(DynamicTable, MaxSize - EntrySize, 0, []),
			State#state{size=Size2 + EntrySize, dyn_table=[{EntrySize, Entry}|DynamicTable2]};
		EntrySize > MaxSize ->
			%% "an attempt to add an entry larger than the
			%% maximum size causes the table to be emptied
			%% of all existing entries and results in an
			%% empty table" (RFC 7541, 4.4)
			State#state{size=0, dyn_table=[]}
	end.

table_resize([], _, Size, Acc) ->
	{lists:reverse(Acc), Size};
table_resize([{EntrySize, _}|_], MaxSize, Size, Acc) when Size + EntrySize > MaxSize ->
	{lists:reverse(Acc), Size};
table_resize([Entry = {EntrySize, _}|Tail], MaxSize, Size, Acc) ->
	table_resize(Tail, MaxSize, Size + EntrySize, [Entry|Acc]).

table_update_size(0, State) ->
	State#state{size=0, max_size=0, dyn_table=[]};
table_update_size(MaxSize, State=#state{size=CurrentSize})
		when CurrentSize =< MaxSize ->
	State#state{max_size=MaxSize};
table_update_size(MaxSize, State=#state{dyn_table=DynTable}) ->
	{DynTable2, Size} = table_resize(DynTable, MaxSize, 0, []),
	State#state{size=Size, max_size=MaxSize, dyn_table=DynTable2}.

-ifdef(TEST).
prop_str_raw() ->
	?FORALL(Str, binary(), begin
		{Str, <<>>} =:= dec_str(iolist_to_binary(enc_str(Str, no_huffman)))
	end).

prop_str_huffman() ->
	?FORALL(Str, binary(), begin
		{Str, <<>>} =:= dec_str(iolist_to_binary(enc_str(Str, huffman)))
	end).
-endif.
