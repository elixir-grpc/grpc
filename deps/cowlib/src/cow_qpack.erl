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

-module(cow_qpack).
-dialyzer(no_improper_lists).

-export([init/1]).
-export([init/3]).

-export([decode_field_section/3]).
-export([execute_encoder_instructions/2]).
-export([decoder_cancel_stream/1]). %% @todo Use it.

-export([encode_field_section/3]).
-export([encode_field_section/4]).
-export([execute_decoder_instructions/2]).
-export([encoder_set_settings/3]).

-record(state, {
	%% Configuration.
	%%
	%% For the encoder these values will be set to
	%% the lowest value between configuration and SETTINGS.

	%% Whether the configured values can be used. The
	%% decoder can always use the configured values.
	%% The encoder must wait for the SETTINGS frame.
	settings_received :: boolean(),

	%% Maximum size of the table.
	max_table_capacity = 0 :: non_neg_integer(),

	%% Maximum number of potentially blocked streams.
	max_blocked_streams = 0 :: non_neg_integer(),

	%% Dynamic table.

	%% The current max table capacity after the encoder
	%% sent an instruction to change the capacity.
	capacity = 0 :: non_neg_integer(),

	%% The size of each entry is len(Name) + len(Value) + 32.
	size = 0 :: non_neg_integer(),

	%% The number of entries ever inserted in the dynamic table.
	%% This value is used on the decoder's size to know whether
	%% it can decode a field section; and on both sides to find
	%% entries in the dynamic table.
	insert_count = 0 :: non_neg_integer(),

	%% The dynamic table. The first value is the size of the entry
	%% and the second value the entry (Name, Value tuple). The
	%% order of the entries is from newest to oldest.
	%%
	%% If 4 entries were inserted, the index of each entry would
	%% be [3, 2, 1, 0]. If 4 entries were inserted and 1 of them
	%% was later dropped, the index of each entry remaining would
	%% be [3, 2, 1] and the insert_count value would be 3, allowing
	%% us to know what index the newest entry is using.
	dyn_table = [] :: [{pos_integer(), {binary(), binary()}}],

	%% Decoder-specific state.

	%% We keep track of streams that are currently blocked
	%% in a map for easy counting and removal. A stream may
	%% be blocked at the beginning of the decoding process.
	%% A stream may be unblocked after encoder instructions
	%% have been processed.
	blocked_streams = #{} :: #{cow_http3:stream_id() => true},

	%% Encoder-specific state.

	%% We keep track of the known received count of the
	%% decoder (the insert_count it has that we know of)
	%% so that we know when we can evict an entry we
	%% inserted. We cannot evict an entry before it has
	%% been acknowledged. The known received count can
	%% also be used to avoid blocking.
	known_received_count = 0 :: non_neg_integer(),

	%% We keep track of the streams that have used the
	%% dynamic table in order to increase the known
	%% received count when the decoder acks a stream.
	%% We only keep the insert_count value for a stream's
	%% field section.
	%%
	%% Because a stream can send multiple field sections
	%% (informational response, final response, trailers),
	%% we use a list to keep track of the different sections.
	%% A FIFO structure would be more adequate but we do
	%% not expect a lot of field sections per stream.
	references = #{} :: #{cow_http3:stream_id() => [non_neg_integer()]},

	%% Smallest absolute index the encoder will reference.
	%% Indexes below may exist in the dynamic table but are
	%% in the process of being phased out and will eventually
	%% be evicted. Only duplicating these indexes is allowed.
	draining_index = 0 :: non_neg_integer(),

	%% Size of the dynamic table that is available for
	%% eviction during encoding. Both this value and the
	%% draining_index are computed at the start of encoding.
	%% Note that for the encoder this cannot reach negatives,
	%% but might for the decoder.
	draining_size = 0 :: integer()
}).

-opaque state() :: #state{}.
-export_type([state/0]).

-type error() :: qpack_decompression_failed
	| qpack_encoder_stream_error
	| qpack_decoder_stream_error.
-export_type([error/0]).

-type encoder_opts() :: #{
	huffman => boolean()
}.
-export_type([encoder_opts/0]).

%-ifdef(TEST).
%-include_lib("proper/include/proper.hrl").
%-endif.

-include("cow_hpack_common.hrl").

%% State initialization.

-spec init(decoder | encoder) -> state().

init(Role) ->
	init(Role, 4096, 0).

-spec init(decoder | encoder, non_neg_integer(), non_neg_integer()) -> state().

init(Role, MaxTableCapacity, MaxBlockedStreams) ->
	#state{
		settings_received=Role =:= decoder,
		max_table_capacity=MaxTableCapacity,
		max_blocked_streams=MaxBlockedStreams
	}.

%% Decoding.

-spec decode_field_section(binary(), cow_http3:stream_id(), State)
	-> {ok, cow_http:headers(), binary(), State}
	| {blocked, State}
	| {connection_error, error(), atom()}
	when State::state().

decode_field_section(Data, StreamID, State=#state{max_blocked_streams=MaxBlockedStreams,
		insert_count=InsertCount, blocked_streams=BlockedStreams}) ->
	{EncInsertCount, Rest} = dec_big_int(Data, 0, 0),
	ReqInsertCount = decode_req_insert_count(EncInsertCount, State),
	if
		ReqInsertCount =< InsertCount ->
			decode_field_section(Rest, StreamID, State, ReqInsertCount);
		%% The stream is blocked and we do not allow that;
		%% or there are already too many blocked streams.
		map_size(BlockedStreams) > MaxBlockedStreams ->
			{connection_error, qpack_decompression_failed,
				'More blocked streams than configuration allows. (RFC9204 2.1.2)'};
		%% The stream is blocked and we allow that.
		%% The caller must keep the data and retry after
		%% calling the execute_encoder_instructions function.
		true ->
			{blocked, State#state{blocked_streams=BlockedStreams#{StreamID => true}}}
	end.

decode_field_section(<<S:1,Rest0/bits>>, StreamID,
		State0=#state{blocked_streams=BlockedStreams}, ReqInsertCount) ->
	State1 = State0#state{
		%% The stream may have been blocked. Unblock it.
		blocked_streams=maps:remove(StreamID, BlockedStreams),
		%% Reset the draining_size. We don't use it, but don't
		%% want the value to unnecessarily become a big int.
		draining_size=0
	},
	{DeltaBase, Rest} = dec_int7(Rest0),
	Base = case S of
		0 -> ReqInsertCount + DeltaBase;
		1 -> ReqInsertCount - DeltaBase - 1
	end,
	case decode(Rest, State1, Base, []) of
		{ok, Headers, State} when ReqInsertCount =:= 0 ->
			{ok, Headers, <<>>, State};
		{ok, Headers, State} ->
			{ok, Headers, enc_int7(StreamID, 2#1), State}
	end.

decode_req_insert_count(0, _) ->
	0;
decode_req_insert_count(EncInsertCount, #state{
		max_table_capacity=MaxTableCapacity, insert_count=InsertCount}) ->
	MaxEntries = MaxTableCapacity div 32,
	FullRange = 2 * MaxEntries,
	if
		EncInsertCount > FullRange ->
			{connection_error, qpack_decompression_failed,
				'EncInsertCount larger than maximum possible value. (RFC9204 4.5.1.1)'};
		true ->
			MaxValue = InsertCount + MaxEntries,
			MaxWrapped = (MaxValue div FullRange) * FullRange,
			ReqInsertCount = MaxWrapped + EncInsertCount - 1,
			if
				ReqInsertCount > MaxValue ->
					if
						ReqInsertCount =< FullRange ->
							{connection_error, qpack_decompression_failed,
								'ReqInsertCount value larger than current maximum value. (RFC9204 4.5.1.1)'};
						true ->
							ReqInsertCount - FullRange
					end;
				ReqInsertCount =:= 0 ->
					{connection_error, qpack_decompression_failed,
						'ReqInsertCount value of 0 must be encoded as 0. (RFC9204 4.5.1.1)'};
				true ->
					ReqInsertCount
			end
	end.

decode(<<>>, State, _, Acc) ->
	{ok, lists:reverse(Acc), State};
%% Indexed field line.
decode(<<2#1:1,T:1,Rest0/bits>>, State, Base, Acc) ->
	{Index, Rest} = dec_int6(Rest0),
	Entry = case T of
		0 -> table_get_dyn_pre_base(Index, Base, State);
		1 -> table_get_static(Index)
	end,
	decode(Rest, State, Base, [Entry|Acc]);
%% Indexed field line with post-base index.
decode(<<2#0001:4,Rest0/bits>>, State, Base, Acc) ->
	{Index, Rest} = dec_int4(Rest0),
	Entry = table_get_dyn_post_base(Index, Base, State),
	decode(Rest, State, Base, [Entry|Acc]);
%% Literal field line with name reference.
decode(<<2#01:2,_N:1,T:1,Rest0/bits>>, State, Base, Acc) ->
	%% @todo N=1 the encoded field line MUST be encoded as literal, need to return metadata about this?
	{NameIndex, <<H:1,Rest1/bits>>} = dec_int4(Rest0),
	Name = case T of
		0 -> table_get_name_dyn_rel(NameIndex, State);
		1 -> table_get_name_static(NameIndex)
	end,
	{ValueLen, Rest2} = dec_int7(Rest1),
	{Value, Rest} = maybe_dec_huffman(Rest2, ValueLen, H),
	decode(Rest, State, Base, [{Name, Value}|Acc]);
%% Literal field line with post-base name reference.
decode(<<2#0000:4,_N:1,Rest0/bits>>, State, Base, Acc) ->
	%% @todo N=1 the encoded field line MUST be encoded as literal, need to return metadata about this?
	{NameIndex, <<H:1,Rest1/bits>>} = dec_int3(Rest0),
	Name = table_get_name_dyn_post_base(NameIndex, Base, State),
	{ValueLen, Rest2} = dec_int7(Rest1),
	{Value, Rest} = maybe_dec_huffman(Rest2, ValueLen, H),
	decode(Rest, State, Base, [{Name, Value}|Acc]);
%% Literal field line with literal name.
decode(<<2#001:3,_N:1,NameH:1,Rest0/bits>>, State, Base, Acc) ->
	%% @todo N=1 the encoded field line MUST be encoded as literal, need to return metadata about this?
	{NameLen, Rest1} = dec_int3(Rest0),
	<<NameStr:NameLen/binary,ValueH:1,Rest2/bits>> = Rest1,
	{Name, <<>>} = maybe_dec_huffman(NameStr, NameLen, NameH),
	{ValueLen, Rest3} = dec_int7(Rest2),
	{Value, Rest} = maybe_dec_huffman(Rest3, ValueLen, ValueH),
	decode(Rest, State, Base, [{Name, Value}|Acc]).

-spec execute_encoder_instructions(binary(), State)
	-> {ok, binary(), State}
	| {connection_error, qpack_encoder_stream_error, atom()}
	when State::state().

execute_encoder_instructions(Data, State) ->
	execute_encoder_instructions(Data, State, 0).

execute_encoder_instructions(<<>>, State, 0) ->
	{ok, <<>>, State};
execute_encoder_instructions(<<>>, State, Increment) ->
	{ok, enc_int6(Increment, 2#00), State};
%% Set dynamic table capacity.
execute_encoder_instructions(<<2#001:3,Rest0/bits>>, State=#state{
		max_table_capacity=MaxTableCapacity, capacity=Capacity0,
		dyn_table=DynamicTable0}, Increment) ->
	{Capacity, Rest} = dec_int5(Rest0),
	if
		%% Capacity larger than configured, or dynamic table
		%% disabled when max_table_capacity=0.
		Capacity > MaxTableCapacity ->
			{connection_error, qpack_encoder_stream_error,
				'New table capacity higher than SETTINGS_QPACK_MAX_TABLE_CAPACITY. (RFC9204 3.2.3, RFC9204 4.3.1)'};
		%% Table capacity was reduced. We must evict entries.
		Capacity < Capacity0 ->
			{DynamicTable, Size} = table_evict(DynamicTable0, Capacity, 0, []),
			execute_encoder_instructions(Rest, State#state{capacity=Capacity,
				size=Size, dyn_table=DynamicTable}, Increment);
		%% Table capacity equal or higher than previous.
		true ->
			execute_encoder_instructions(Rest,
				State#state{capacity=Capacity}, Increment)
	end;
%% Insert with name reference.
execute_encoder_instructions(<<2#1:1,T:1,Rest0/bits>>, State, Increment) ->
	{NameIndex, <<H:1,Rest1/bits>>} = dec_int6(Rest0),
	Name = case T of
		0 -> table_get_name_dyn_rel(NameIndex, State);
		1 -> table_get_name_static(NameIndex)
	end,
	{ValueLen, Rest2} = dec_int7(Rest1),
	{Value, Rest} = maybe_dec_huffman(Rest2, ValueLen, H),
	execute_insert_instruction(Rest, State, Increment, {Name, Value});
%% Insert with literal name.
execute_encoder_instructions(<<2#01:2,NameH:1,Rest0/bits>>, State, Increment) ->
	{NameLen, Rest1} = dec_int5(Rest0),
	{Name, <<ValueH:1,Rest2/bits>>} = maybe_dec_huffman(Rest1, NameLen, NameH),
	{ValueLen, Rest3} = dec_int7(Rest2),
	{Value, Rest} = maybe_dec_huffman(Rest3, ValueLen, ValueH),
	execute_insert_instruction(Rest, State, Increment, {Name, Value});
%% Duplicate.
execute_encoder_instructions(<<2#000:3,Rest0/bits>>, State, Increment) ->
	{Index, Rest} = dec_int5(Rest0),
	Entry = table_get_dyn_rel(Index, State),
	execute_insert_instruction(Rest, State, Increment, Entry).

execute_insert_instruction(Rest, State0, Increment, Entry) ->
	case table_insert(Entry, State0) of
		{ok, State} ->
			execute_encoder_instructions(Rest, State, Increment + 1);
		Error = {connection_error, _, _} ->
			Error
	end.

%% @todo Export / spec.

decoder_cancel_stream(StreamID) ->
	enc_int6(StreamID, 2#01).

dec_int3(<<2#111:3,Rest/bits>>) ->
	dec_big_int(Rest, 7, 0);
dec_int3(<<Int:3,Rest/bits>>) ->
	{Int, Rest}.

dec_int4(<<2#1111:4,Rest/bits>>) ->
	dec_big_int(Rest, 15, 0);
dec_int4(<<Int:4,Rest/bits>>) ->
	{Int, Rest}.

dec_int6(<<2#111111:6,Rest/bits>>) ->
	dec_big_int(Rest, 63, 0);
dec_int6(<<Int:6,Rest/bits>>) ->
	{Int, Rest}.

dec_int7(<<2#1111111:7,Rest/bits>>) ->
	dec_big_int(Rest, 127, 0);
dec_int7(<<Int:7,Rest/bits>>) ->
	{Int, Rest}.

maybe_dec_huffman(Data, ValueLen, 0) ->
	<<Value:ValueLen/binary,Rest/bits>> = Data,
	{Value, Rest};
maybe_dec_huffman(Data, ValueLen, 1) ->
	dec_huffman(Data, ValueLen, 0, <<>>).

-ifdef(TEST).
appendix_b_decoder_test() ->
	%% Stream: 0
	{ok, [
		{<<":path">>, <<"/index.html">>}
	], <<>>, DecState0} = decode_field_section(<<
		16#0000:16,
		16#510b:16, 16#2f69:16, 16#6e64:16, 16#6578:16,
		16#2e68:16, 16#746d:16, 16#6c
	>>, 0, init(decoder, 4096, 0)),
	#state{
		capacity=0,
		size=0,
		insert_count=0,
		dyn_table=[]
	} = DecState0,
	%% Stream: Encoder
	{ok, EncData1, DecState1} = execute_encoder_instructions(<<
		16#3fbd01:24,
		16#c00f:16, 16#7777:16, 16#772e:16, 16#6578:16,
		16#616d:16, 16#706c:16, 16#652e:16, 16#636f:16,
		16#6d,
		16#c10c:16, 16#2f73:16, 16#616d:16, 16#706c:16,
		16#652f:16, 16#7061:16, 16#7468:16
	>>, DecState0),
	<<2#00:2,2:6>> = EncData1,
	#state{
		capacity=220,
		size=106,
		insert_count=2,
		%% The dynamic table is in reverse order.
		dyn_table=[
			{49, {<<":path">>, <<"/sample/path">>}},
			{57, {<<":authority">>, <<"www.example.com">>}}
		]
	} = DecState1,
	%% Stream: 4
	{ok, [
		{<<":authority">>, <<"www.example.com">>},
		{<<":path">>, <<"/sample/path">>}
	], <<16#84>>, DecState2} = decode_field_section(<<
		16#0381:16,
		16#10,
		16#11
	>>, 4, DecState1),
	DecState1 = DecState2,
	%% Stream: Encoder
	{ok, EncData3, DecState3} = execute_encoder_instructions(<<
		16#4a63:16, 16#7573:16, 16#746f:16, 16#6d2d:16,
		16#6b65:16, 16#790c:16, 16#6375:16, 16#7374:16,
		16#6f6d:16, 16#2d76:16, 16#616c:16, 16#7565:16
	>>, DecState2),
	<<2#00:2,1:6>> = EncData3,
	#state{
		capacity=220,
		size=160,
		insert_count=3,
		dyn_table=[
			{54, {<<"custom-key">>, <<"custom-value">>}},
			{49, {<<":path">>, <<"/sample/path">>}},
			{57, {<<":authority">>, <<"www.example.com">>}}
		]
	} = DecState3,
	%% Stream: Encoder
	{ok, EncData4, DecState4} = execute_encoder_instructions(<<
		16#02
	>>, DecState3),
	<<2#00:2,1:6>> = EncData4,
	#state{
		capacity=220,
		size=217,
		insert_count=4,
		dyn_table=[
			{57, {<<":authority">>, <<"www.example.com">>}},
			{54, {<<"custom-key">>, <<"custom-value">>}},
			{49, {<<":path">>, <<"/sample/path">>}},
			{57, {<<":authority">>, <<"www.example.com">>}}
		]
	} = DecState4,
	%% Stream: 8
	%%
	%% Note that this one is not really received by the decoder
	%% so we will ignore the decoder state before we continue.
	{ok, [
		{<<":authority">>, <<"www.example.com">>},
		{<<":path">>, <<"/">>},
		{<<"custom-key">>, <<"custom-value">>}
	], <<16#88>>, IgnoredDecState} = decode_field_section(<<
		16#0500:16,
		16#80,
		16#c1,
		16#81
	>>, 8, DecState4),
	%% Note that the state did not change anyway.
	DecState4 = IgnoredDecState,
	%% Stream: Decoder - Stream Cancellation (Stream=8)
	<<16#48>> = decoder_cancel_stream(8),
	{ok, EncData5, DecState5} = execute_encoder_instructions(<<
		16#810d:16, 16#6375:16, 16#7374:16, 16#6f6d:16,
		16#2d76:16, 16#616c:16, 16#7565:16, 16#32
	>>, DecState4),
	<<2#00:2,1:6>> = EncData5,
	#state{
		capacity=220,
		size=215,
		insert_count=5,
		dyn_table=[
			{55, {<<"custom-key">>, <<"custom-value2">>}},
			{57, {<<":authority">>, <<"www.example.com">>}},
			{54, {<<"custom-key">>, <<"custom-value">>}},
			{49, {<<":path">>, <<"/sample/path">>}}
		]
	} = DecState5,
	ok.
-endif.

%% Encoding.

-spec encode_field_section(cow_http:headers(), cow_http3:stream_id(), State)
	-> {ok, iolist(), iolist(), State} when State::state().

%% @todo Would be good to know encoder stream flow control to avoid writing there. Opts?
encode_field_section(Headers, StreamID, State0) ->
	encode_field_section(Headers, StreamID, State0, #{}).

-spec encode_field_section(cow_http:headers(), cow_http3:stream_id(), State, encoder_opts())
	-> {ok, iolist(), iolist(), State} when State::state().

encode_field_section(Headers, StreamID, State0=#state{
		max_table_capacity=MaxTableCapacity, insert_count=InsertCount,
		references=Refs0}, Opts) ->
	State1 = encode_update_drain_info(State0),
	Base = InsertCount + 1,
	{ReqInsertCount, EncData, Data, State} = encode(
		Headers, StreamID, State1,
		huffman_opt(Opts), 0, Base, [], []),
	case ReqInsertCount of
		0 ->
			{ok, [<<0:16>>|Data], EncData, State};
		_ ->
			MaxEntries = MaxTableCapacity div 32,
			EncInsertCount = (ReqInsertCount rem (2 * MaxEntries)) + 1,
			{S, DeltaBase} = if
				%% We inserted new entries.
				ReqInsertCount > Base ->
					{2#1, ReqInsertCount - Base};
				%% We only used existing entries.
				ReqInsertCount =< Base ->
					{2#0, ReqInsertCount - Base}
			end,
			%% Save the reference to avoid draining entries too quickly.
			Refs = case Refs0 of
				#{StreamID := ICs} ->
					Refs0#{StreamID => [ReqInsertCount|ICs]};
				_ ->
					Refs0#{StreamID => [ReqInsertCount]}
			end,
			{ok, [enc_big_int(EncInsertCount, <<>>), enc_int7(DeltaBase, S)|Data], EncData,
				State#state{references=Refs}}
	end.

%% We check how many entries we can evict. The result
%% will take the form of a draining_index (the oldest
%% entry the encoder can reference) as well as a
%% draining_size (how much data can be gained by evicting).
%%
%% We first look at streams that have not been acknowledged
%% and find the smallest insert_count value from them. We
%% cannot evict any value that is newer than or equal to
%% that value.
%%
%% Then we also need to make sure we don't evict too much
%% from the table.
%%
%% Finally we go over the dynamic table to count how much
%% we can actually drain and what the draining index really is.
encode_update_drain_info(State=#state{max_table_capacity=MaxCapacity,
		insert_count=InsertCount, dyn_table=DynTable, references=Refs}) ->
	PendingInsertCount = if
		%% When we don't use the dynamic table, or we didn't insert
		%% anything yet, there are no references. We can drain
		%% everything but are still constrained by the max draining size.
		Refs =:= #{} ->
			InsertCount;
		true ->
			maps:fold(fun(_, ICs, V) ->
				IC = hd(lists:reverse(ICs)),
				case V of
					undefined -> IC;
					_ -> min(IC, V)
				end
			end, undefined, Refs)
	end,
	%% We use a simple formula for calculating the maximum
	%% draining size, found in nginx: we allow evicting
	%% between 1/8th of the current table capacity and
	%% 512 bytes, whichever is smaller. When the maximum
	%% table capacity is small this formula may get us
	%% a value that's too small to drain anything, so
	%% we use 64 bytes as a minimum.
	MaxDrainingSize0 = min(512, MaxCapacity div 8),
	MaxDrainingSize = if
		MaxDrainingSize0 < 64 -> 64;
		true -> MaxDrainingSize0
	end,
	{DrainingIndex, DrainingSize} =
		encode_update_drain_loop(lists:reverse(DynTable),
			InsertCount - length(DynTable), PendingInsertCount,
			0, MaxDrainingSize),
	State#state{
		draining_index=DrainingIndex,
		draining_size=DrainingSize
	}.

%% We go over the dynamic table in reverse order. We stop
%% when we either reach the PendingInsertCount value or get
%% above MaxDrainingSize. It's not possible to go over the
%% entire dynamic table because we have references.
encode_update_drain_loop(_, Index, PendingIndex, Size, _)
		when Index =:= PendingIndex ->
	{Index, Size};
encode_update_drain_loop([{EntrySize, _}|_], Index, _, Size, MaxSize)
		when Size + EntrySize > MaxSize ->
	{Index, Size};
encode_update_drain_loop([{EntrySize, _}|Tail], Index, PendingIndex, Size, MaxSize) ->
	encode_update_drain_loop(Tail, Index + 1, PendingIndex, Size + EntrySize, MaxSize).

encode([], _StreamID, State, _HuffmanOpt,
		ReqInsertCount, _Base, EncAcc, Acc) ->
	{ReqInsertCount, lists:reverse(EncAcc), lists:reverse(Acc), State};
encode([{Name, Value0}|Tail], StreamID, State, HuffmanOpt,
		ReqInsertCount, Base, EncAcc, Acc) ->
	%% We conditionally call iolist_to_binary/1 because a small
	%% but noticeable speed improvement happens when we do this.
	%% (Or at least it did for cow_hpack.)
	Value = if
		is_binary(Value0) -> Value0;
		true -> iolist_to_binary(Value0)
	end,
	Entry = {Name, Value},
	encode_static([Entry|Tail], StreamID, State, HuffmanOpt,
		ReqInsertCount, Base, EncAcc, Acc).

encode_static([Entry|Tail], StreamID, State, HuffmanOpt,
		ReqInsertCount, Base, EncAcc, Acc) ->
	case table_find_static(Entry) of
		not_found ->
			encode_dyn([Entry|Tail], StreamID, State, HuffmanOpt,
				ReqInsertCount, Base, EncAcc, Acc);
		StaticIndex ->
			encode(Tail, StreamID, State, HuffmanOpt,
				ReqInsertCount, Base, EncAcc,
				%% Indexed Field Line. T=1 (static).
				[enc_int6(StaticIndex, 2#11)|Acc])
	end.

encode_dyn([Entry|Tail], StreamID, State0=#state{draining_index=DrainingIndex},
		HuffmanOpt, ReqInsertCount0, Base, EncAcc, Acc) ->
	case table_find_dyn(Entry, State0) of
		not_found ->
			encode_static_name([Entry|Tail], StreamID, State0, HuffmanOpt,
				ReqInsertCount0, Base, EncAcc, Acc);
		%% When the index is below the drain index and there is enough
		%% space in the table for duplicating the value, we do that
		%% and use the duplicated index. If we can't then we must not
		%% use the dynamic index for the field.
		DynIndex when DynIndex < DrainingIndex ->
			case encode_can_insert(Entry, State0) of
				{true, EncInstr, State1} ->
					{ok, State} = table_insert(Entry, State1),
					#state{insert_count=ReqInsertCount} = State,
					%% We must reference the relative index of the entry we duplicated
					%% before we duplicated it. The newest entry starts at 0. If we
					%% have 3 entries in the table, the oldest one will have a relative
					%% index of 2. Because we already inserted the duplicate, our
					%% ReqInsertCount has 1 added, so for our previously 3 entries
					%% table, we end up with a ReqInsertCount of 4. This means we
					%% have to remove 2 from the difference to find the relative index.
					DynIndexRel = ReqInsertCount - DynIndex - 2,
					encode(Tail, StreamID, State, HuffmanOpt, ReqInsertCount, Base,
						%% Duplicate.
						[[EncInstr|enc_int5(DynIndexRel, 2#000)]|EncAcc],
						%% Indexed Field Line. T=0 (dynamic).
						[enc_int6(Base - ReqInsertCount, 2#10)|Acc]);
				false ->
					encode_static_name([Entry|Tail], StreamID, State0, HuffmanOpt,
						ReqInsertCount0, Base, EncAcc, Acc)
			end;
		DynIndex ->
			ReqInsertCount = max(ReqInsertCount0, DynIndex),
			encode(Tail, StreamID, State0, HuffmanOpt, ReqInsertCount, Base, EncAcc,
				%% Indexed Field Line. T=0 (dynamic).
				[enc_int6(Base - DynIndex - 1, 2#10)|Acc])
	end.

encode_static_name([Entry = {Name, Value}|Tail], StreamID, State0, HuffmanOpt,
		ReqInsertCount0, Base, EncAcc, Acc) ->
	case table_find_name_static(Name) of
		not_found ->
			encode_dyn_name([Entry|Tail], StreamID, State0, HuffmanOpt,
				ReqInsertCount0, Base, EncAcc, Acc);
		StaticNameIndex ->
			case encode_can_insert(Entry, State0) of
				{true, EncInstr, State1} ->
					{ok, State} = table_insert(Entry, State1),
					#state{insert_count=ReqInsertCount} = State,
					PostBaseIndex = length(EncAcc),
					encode(Tail, StreamID, State, HuffmanOpt, ReqInsertCount, Base,
						%% Insert with Name Reference. T=1 (static).
						[[EncInstr, enc_int6(StaticNameIndex, 2#11)|enc_str(Value, HuffmanOpt)]
							|EncAcc],
						%% Indexed Field Line with Post-Base Index.
						[enc_int4(PostBaseIndex, 2#0001)|Acc]);
				false ->
					encode(Tail, StreamID, State0, HuffmanOpt, ReqInsertCount0, Base, EncAcc,
						%% Literal Field Line with Name Reference. N=0. T=1 (static).
						[[enc_int4(StaticNameIndex, 2#0101)|enc_str(Value, HuffmanOpt)]|Acc])
			end
	end.

encode_dyn_name([Entry = {Name, Value}|Tail], StreamID,
		State0=#state{draining_index=DrainingIndex},
		HuffmanOpt, ReqInsertCount0, Base, EncAcc, Acc) ->
	case table_find_name_dyn(Name, State0) of
		%% We can reference the dynamic name.
		DynIndex when is_integer(DynIndex), DynIndex >= DrainingIndex ->
			case encode_can_insert(Entry, State0) of
				{true, EncInstr, State1} ->
					{ok, State} = table_insert(Entry, State1),
					#state{insert_count=ReqInsertCount} = State,
					%% See comment in encode_dyn for why we remove 2.
					DynIndexRel = ReqInsertCount - DynIndex - 2,
					PostBaseIndex = length(EncAcc),
					encode(Tail, StreamID, State, HuffmanOpt, ReqInsertCount, Base,
						%% Insert with Name Reference. T=0 (dynamic).
						[[EncInstr, enc_int6(DynIndexRel, 2#10)|enc_str(Value, HuffmanOpt)]
							|EncAcc],
						%% Indexed Field Line with Post-Base Index.
						[enc_int4(PostBaseIndex, 2#0001)|Acc]);
				false ->
					encode(Tail, StreamID, State0, HuffmanOpt, ReqInsertCount0, Base, EncAcc,
						%% Literal Field Line with Name Reference. N=0. T=0 (dynamic).
						[[enc_int4(DynIndex, 2#0100)|enc_str(Value, HuffmanOpt)]|Acc])
			end;
		%% When there are no name to reference, or the name
		%% is found below the drain index, we do not attempt
		%% to refer to it.
		_ ->
			case encode_can_insert(Entry, State0) of
				{true, EncInstr, State1} ->
					{ok, State} = table_insert(Entry, State1),
					#state{insert_count=ReqInsertCount} = State,
					PostBaseIndex = length(EncAcc),
					encode(Tail, StreamID, State, HuffmanOpt, ReqInsertCount, Base,
						%% Insert with Literal Name.
						[[EncInstr, enc_str6(Name, HuffmanOpt, 2#01)|enc_str(Value, HuffmanOpt)]
							|EncAcc],
						%% Indexed Field Line with Post-Base Index.
						[enc_int4(PostBaseIndex, 2#0001)|Acc]);
				false ->
					encode(Tail, StreamID, State0, HuffmanOpt, ReqInsertCount0, Base, EncAcc,
						%% Literal Field Line with Literal Name. N=0.
						[[enc_str4(Name, HuffmanOpt, 2#0010)|enc_str(Value, HuffmanOpt)]|Acc])
			end
	end.

%% @todo We should make sure we have a large enough flow control window.
%%
%% We can never insert before receiving the SETTINGS frame.
encode_can_insert(_, #state{settings_received=false}) ->
	false;
encode_can_insert({Name, Value}, State=#state{
		max_table_capacity=MaxCapacity, capacity=Capacity,
		size=Size, draining_size=DrainingSize}) ->
	EntrySize = byte_size(Name) + byte_size(Value) + 32,
	if
		%% We have enough space in the current capacity,
		%% without having to drain entries.
		EntrySize + Size =< Capacity ->
			{true, <<>>, State};
		%% We have enough space if we increase the capacity.
		%% We prefer to first increase the capacity to the
		%% maximum before we start draining entries.
		EntrySize + Size =< MaxCapacity ->
			{true, enc_int5(MaxCapacity, 2#001),
				State#state{capacity=MaxCapacity}};
		%% We are already at max capacity and have enough
		%% space if we drain entries.
		EntrySize + Size =< Capacity + DrainingSize, Capacity =:= MaxCapacity ->
			{true, <<>>, State};
		%% We are not at max capacity. We have enough space
		%% if we both increase the capacity and drain entries.
		EntrySize + Size =< MaxCapacity + DrainingSize ->
			{true, enc_int5(MaxCapacity, 2#001),
				State#state{capacity=MaxCapacity}};
		true ->
			false
	end.

-spec execute_decoder_instructions(binary(), State)
	-> {ok, State} | {connection_error, qpack_decoder_stream_error, atom()}
	when State::state().
execute_decoder_instructions(<<>>, State) ->
	{ok, State};
%% Section acknowledgement.
%% We remove one reference and if needed increase the known received count.
execute_decoder_instructions(<<2#1:1,Rest0/bits>>, State=#state{
		known_received_count=KnownReceivedCount0, references=Refs}) ->
	{StreamID, Rest} = dec_int7(Rest0),
	case Refs of
		#{StreamID := [InsertCount]} ->
			KnownReceivedCount = max(KnownReceivedCount0, InsertCount),
			execute_decoder_instructions(Rest, State#state{
				known_received_count=KnownReceivedCount,
				references=maps:remove(StreamID, Refs)});
		#{StreamID := InsertCounts} ->
			[InsertCount|InsertCountsTail] = lists:reverse(InsertCounts),
			KnownReceivedCount = max(KnownReceivedCount0, InsertCount),
			execute_decoder_instructions(Rest, State#state{
				known_received_count=KnownReceivedCount,
				references=Refs#{StreamID => lists:reverse(InsertCountsTail)}});
		_ ->
			{connection_error, qpack_decoder_stream_error,
				'Acknowledgement received for stream with no pending sections. (RFC9204 4.4.1)'}
	end;
%% Stream cancellation.
%% We drop all references for the given stream.
execute_decoder_instructions(<<2#01:2,Rest0/bits>>, State=#state{references=Refs}) ->
	{StreamID, Rest} = dec_int6(Rest0),
	case Refs of
		#{StreamID := _} ->
			execute_decoder_instructions(Rest, State#state{
				references=maps:remove(StreamID, Refs)});
		%% It is not an error for the reference to not exist.
		%% The dynamic table may not have been used for this
		%% stream.
		_ ->
			execute_decoder_instructions(Rest, State)
	end;
%% Insert count increment.
%% We increase the known received count.
execute_decoder_instructions(<<2#00:2,Rest0/bits>>, State=#state{
		known_received_count=KnownReceivedCount}) ->
	{Increment, Rest} = dec_int6(Rest0),
	execute_decoder_instructions(Rest, State#state{
		known_received_count=KnownReceivedCount + Increment}).

%% Inform the encoder of the relevant SETTINGS from the decoder.
%% The encoder will choose the smallest value between what it
%% has configured and what it received through SETTINGS. Should
%% there be no value in the SETTINGS then 0 must be given.

-spec encoder_set_settings(non_neg_integer(), non_neg_integer(), state()) -> state().

encoder_set_settings(MaxTableCapacity, MaxBlockedStreams, State=#state{
		max_table_capacity=MaxTableCapacityConfigured,
		max_blocked_streams=MaxBlockedStreamsConfigured}) ->
	State#state{
		settings_received=true,
		max_table_capacity=min(MaxTableCapacity, MaxTableCapacityConfigured),
		max_blocked_streams=min(MaxBlockedStreams, MaxBlockedStreamsConfigured)
	}.

huffman_opt(#{huffman := false}) -> no_huffman;
huffman_opt(_) -> huffman.

enc_int3(Int, Prefix) when Int < 7 ->
	<<Prefix:5, Int:3>>;
enc_int3(Int, Prefix) ->
	enc_big_int(Int - 7, <<Prefix:5, 2#111:3>>).

enc_int4(Int, Prefix) when Int < 15 ->
	<<Prefix:4, Int:4>>;
enc_int4(Int, Prefix) ->
	enc_big_int(Int - 15, <<Prefix:4, 2#1111:4>>).

enc_str4(Str, huffman, Prefix) ->
	Str2 = enc_huffman(Str, <<>>),
	[enc_int3(byte_size(Str2), Prefix * 2 + 2#1)|Str2];
enc_str4(Str, no_huffman, Prefix) ->
	[enc_int3(byte_size(Str), Prefix * 2 + 2#0)|Str].

enc_str6(Str, huffman, Prefix) ->
	Str2 = enc_huffman(Str, <<>>),
	[enc_int5(byte_size(Str2), Prefix * 2 + 2#1)|Str2];
enc_str6(Str, no_huffman, Prefix) ->
	[enc_int5(byte_size(Str), Prefix * 2 + 2#0)|Str].

-ifdef(TEST).
%% This function is a good starting point to let the calling
%% process insert entries in the dynamic table outside of
%% encoding a field section. To be usable more broadly
%% it would need to handle the case where a static name
%% is found, but also consider how it should be used:
%% do we have capacity in the table? We don't have
%% capacity before receiving the SETTINGS frame. Until
%% then it will be restricted to testing.
encoder_insert_entry(Entry={Name, Value}, State0, Opts) ->
	{ok, State} = table_insert(Entry, State0),
	HuffmanOpt = huffman_opt(Opts),
	case table_find_name_static(Name) of
		not_found ->
			case table_find_name_dyn(Name, State0) of
				not_found ->
					%% Insert with Literal Name.
					{ok, [enc_str6(Name, HuffmanOpt, 2#01)|enc_str(Value, HuffmanOpt)], State};
				DynNameIndex ->
					#state{insert_count=ReqInsertCount} = State,
					%% See comment in encode_dyn for why we remove 2.
					DynNameIndexRel = ReqInsertCount - DynNameIndex - 2,
					%% Insert with Name Reference. T=0 (dynamic).
					{ok, [enc_int6(DynNameIndexRel, 2#10)|enc_str(Value, HuffmanOpt)], State}
			end
	end.

appendix_b_encoder_test() ->
	%% We limit the encoder to 220 bytes for table capacity.
	EncState0 = init(encoder, 220, 0),
	%% Stream: 0
	{ok, Data1, EncData1, EncState1} = encode_field_section([
		{<<":path">>, <<"/index.html">>}
	], 0, EncState0, #{huffman => false}),
	<<>> = iolist_to_binary(EncData1),
	<<
		16#0000:16,
		16#510b:16, 16#2f69:16, 16#6e64:16, 16#6578:16,
		16#2e68:16, 16#746d:16, 16#6c
	>> = iolist_to_binary(Data1),
	#state{
		capacity=0,
		size=0,
		insert_count=0,
		dyn_table=[]
	} = EncState1,
	%% Simulate receiving of the SETTINGS frame enabling the dynamic table.
	EncState2 = encoder_set_settings(4096, 0, EncState1),
	#state{
		settings_received=true,
		max_table_capacity=220,
		capacity=0
	} = EncState2,
	%% Stream: 4 (and Encoder)
	{ok, Data3, EncData3, EncState3} = encode_field_section([
		{<<":authority">>, <<"www.example.com">>},
		{<<":path">>, <<"/sample/path">>}
	], 4, EncState2, #{huffman => false}),
	<<
		16#3fbd01:24,
		16#c00f:16, 16#7777:16, 16#772e:16, 16#6578:16,
		16#616d:16, 16#706c:16, 16#652e:16, 16#636f:16,
		16#6d,
		16#c10c:16, 16#2f73:16, 16#616d:16, 16#706c:16,
		16#652f:16, 16#7061:16, 16#7468:16
	>> = iolist_to_binary(EncData3),
	<<
		16#0381:16,
		16#10,
		16#11
	>> = iolist_to_binary(Data3),
	#state{
		capacity=220,
		size=106,
		insert_count=2,
		%% The dynamic table is in reverse order.
		dyn_table=[
			{49, {<<":path">>, <<"/sample/path">>}},
			{57, {<<":authority">>, <<"www.example.com">>}}
		]
	} = EncState3,
	%% Stream: Decoder
	{ok, EncState4} = execute_decoder_instructions(<<16#84>>, EncState3),
	#state{
		capacity=220,
		size=106,
		insert_count=2,
		%% The dynamic table is in reverse order.
		dyn_table=[
			{49, {<<":path">>, <<"/sample/path">>}},
			{57, {<<":authority">>, <<"www.example.com">>}}
		]
	} = EncState4,
	%% Stream: Encoder
	{ok, EncData5, EncState5} = encoder_insert_entry(
		{<<"custom-key">>, <<"custom-value">>},
		EncState4, #{huffman => false}),
	<<
		16#4a63:16, 16#7573:16, 16#746f:16, 16#6d2d:16,
		16#6b65:16, 16#790c:16, 16#6375:16, 16#7374:16,
		16#6f6d:16, 16#2d76:16, 16#616c:16, 16#7565:16
	>> = iolist_to_binary(EncData5),
	#state{
		capacity=220,
		size=160,
		insert_count=3,
		%% The dynamic table is in reverse order.
		dyn_table=[
			{54, {<<"custom-key">>, <<"custom-value">>}},
			{49, {<<":path">>, <<"/sample/path">>}},
			{57, {<<":authority">>, <<"www.example.com">>}}
		]
	} = EncState5,
	%% Stream: Decoder
	{ok, EncState6} = execute_decoder_instructions(<<16#01>>, EncState5),
	#state{
		capacity=220,
		size=160,
		insert_count=3,
		%% The dynamic table is in reverse order.
		dyn_table=[
			{54, {<<"custom-key">>, <<"custom-value">>}},
			{49, {<<":path">>, <<"/sample/path">>}},
			{57, {<<":authority">>, <<"www.example.com">>}}
		]
	} = EncState6,
	%% Stream: 8 (and Encoder)
	{ok, Data7, EncData7, EncState7} = encode_field_section([
		{<<":authority">>, <<"www.example.com">>},
		{<<":path">>, <<"/">>},
		{<<"custom-key">>, <<"custom-value">>}
	], 8, EncState6),
	<<16#02>> = iolist_to_binary(EncData7),
	<<
		16#0500:16,
		16#80,
		16#c1,
		16#81
	>> = iolist_to_binary(Data7),
	#state{
		capacity=220,
		size=217,
		insert_count=4,
		%% The dynamic table is in reverse order.
		dyn_table=[
			{57, {<<":authority">>, <<"www.example.com">>}},
			{54, {<<"custom-key">>, <<"custom-value">>}},
			{49, {<<":path">>, <<"/sample/path">>}},
			{57, {<<":authority">>, <<"www.example.com">>}}
		]
	} = EncState7,
	%% Stream: Decoder
	{ok, EncState8} = execute_decoder_instructions(<<16#48>>, EncState7),
	#state{
		capacity=220,
		size=217,
		insert_count=4,
		%% The dynamic table is in reverse order.
		dyn_table=[
			{57, {<<":authority">>, <<"www.example.com">>}},
			{54, {<<"custom-key">>, <<"custom-value">>}},
			{49, {<<":path">>, <<"/sample/path">>}},
			{57, {<<":authority">>, <<"www.example.com">>}}
		]
	} = EncState8,
	%% Stream: Encoder
	{ok, EncData9, EncState9} = encoder_insert_entry(
		{<<"custom-key">>, <<"custom-value2">>},
		EncState8, #{huffman => false}),
	<<
		16#810d:16, 16#6375:16, 16#7374:16, 16#6f6d:16,
		16#2d76:16, 16#616c:16, 16#7565:16, 16#32
	>> = iolist_to_binary(EncData9),
	#state{
		capacity=220,
		size=215,
		insert_count=5,
		%% The dynamic table is in reverse order.
		dyn_table=[
			{55, {<<"custom-key">>, <<"custom-value2">>}},
			{57, {<<":authority">>, <<"www.example.com">>}},
			{54, {<<"custom-key">>, <<"custom-value">>}},
			{49, {<<":path">>, <<"/sample/path">>}}
		]
	} = EncState9,
	ok.
-endif.

%% Static and dynamic tables.

table_find_static({<<":authority">>, <<>>}) -> 0;
table_find_static({<<":path">>, <<"/">>}) -> 1;
table_find_static({<<"age">>, <<"0">>}) -> 2;
table_find_static({<<"content-disposition">>, <<>>}) -> 3;
table_find_static({<<"content-length">>, <<"0">>}) -> 4;
table_find_static({<<"cookie">>, <<>>}) -> 5;
table_find_static({<<"date">>, <<>>}) -> 6;
table_find_static({<<"etag">>, <<>>}) -> 7;
table_find_static({<<"if-modified-since">>, <<>>}) -> 8;
table_find_static({<<"if-none-match">>, <<>>}) -> 9;
table_find_static({<<"last-modified">>, <<>>}) -> 10;
table_find_static({<<"link">>, <<>>}) -> 11;
table_find_static({<<"location">>, <<>>}) -> 12;
table_find_static({<<"referer">>, <<>>}) -> 13;
table_find_static({<<"set-cookie">>, <<>>}) -> 14;
table_find_static({<<":method">>, <<"CONNECT">>}) -> 15;
table_find_static({<<":method">>, <<"DELETE">>}) -> 16;
table_find_static({<<":method">>, <<"GET">>}) -> 17;
table_find_static({<<":method">>, <<"HEAD">>}) -> 18;
table_find_static({<<":method">>, <<"OPTIONS">>}) -> 19;
table_find_static({<<":method">>, <<"POST">>}) -> 20;
table_find_static({<<":method">>, <<"PUT">>}) -> 21;
table_find_static({<<":scheme">>, <<"http">>}) -> 22;
table_find_static({<<":scheme">>, <<"https">>}) -> 23;
table_find_static({<<":status">>, <<"103">>}) -> 24;
table_find_static({<<":status">>, <<"200">>}) -> 25;
table_find_static({<<":status">>, <<"304">>}) -> 26;
table_find_static({<<":status">>, <<"404">>}) -> 27;
table_find_static({<<":status">>, <<"503">>}) -> 28;
table_find_static({<<"accept">>, <<"*/*">>}) -> 29;
table_find_static({<<"accept">>, <<"application/dns-message">>}) -> 30;
table_find_static({<<"accept-encoding">>, <<"gzip, deflate, br">>}) -> 31;
table_find_static({<<"accept-ranges">>, <<"bytes">>}) -> 32;
table_find_static({<<"access-control-allow-headers">>, <<"cache-control">>}) -> 33;
table_find_static({<<"access-control-allow-headers">>, <<"content-type">>}) -> 34;
table_find_static({<<"access-control-allow-origin">>, <<"*">>}) -> 35;
table_find_static({<<"cache-control">>, <<"max-age=0">>}) -> 36;
table_find_static({<<"cache-control">>, <<"max-age=2592000">>}) -> 37;
table_find_static({<<"cache-control">>, <<"max-age=604800">>}) -> 38;
table_find_static({<<"cache-control">>, <<"no-cache">>}) -> 39;
table_find_static({<<"cache-control">>, <<"no-store">>}) -> 40;
table_find_static({<<"cache-control">>, <<"public, max-age=31536000">>}) -> 41;
table_find_static({<<"content-encoding">>, <<"br">>}) -> 42;
table_find_static({<<"content-encoding">>, <<"gzip">>}) -> 43;
table_find_static({<<"content-type">>, <<"application/dns-message">>}) -> 44;
table_find_static({<<"content-type">>, <<"application/javascript">>}) -> 45;
table_find_static({<<"content-type">>, <<"application/json">>}) -> 46;
table_find_static({<<"content-type">>, <<"application/x-www-form-urlencoded">>}) -> 47;
table_find_static({<<"content-type">>, <<"image/gif">>}) -> 48;
table_find_static({<<"content-type">>, <<"image/jpeg">>}) -> 49;
table_find_static({<<"content-type">>, <<"image/png">>}) -> 50;
table_find_static({<<"content-type">>, <<"text/css">>}) -> 51;
table_find_static({<<"content-type">>, <<"text/html; charset=utf-8">>}) -> 52;
table_find_static({<<"content-type">>, <<"text/plain">>}) -> 53;
table_find_static({<<"content-type">>, <<"text/plain;charset=utf-8">>}) -> 54;
table_find_static({<<"range">>, <<"bytes=0-">>}) -> 55;
table_find_static({<<"strict-transport-security">>, <<"max-age=31536000">>}) -> 56;
table_find_static({<<"strict-transport-security">>, <<"max-age=31536000; includesubdomains">>}) -> 57;
table_find_static({<<"strict-transport-security">>, <<"max-age=31536000; includesubdomains; preload">>}) -> 58;
table_find_static({<<"vary">>, <<"accept-encoding">>}) -> 59;
table_find_static({<<"vary">>, <<"origin">>}) -> 60;
table_find_static({<<"x-content-type-options">>, <<"nosniff">>}) -> 61;
table_find_static({<<"x-xss-protection">>, <<"1; mode=block">>}) -> 62;
table_find_static({<<":status">>, <<"100">>}) -> 63;
table_find_static({<<":status">>, <<"204">>}) -> 64;
table_find_static({<<":status">>, <<"206">>}) -> 65;
table_find_static({<<":status">>, <<"302">>}) -> 66;
table_find_static({<<":status">>, <<"400">>}) -> 67;
table_find_static({<<":status">>, <<"403">>}) -> 68;
table_find_static({<<":status">>, <<"421">>}) -> 69;
table_find_static({<<":status">>, <<"425">>}) -> 70;
table_find_static({<<":status">>, <<"500">>}) -> 71;
table_find_static({<<"accept-language">>, <<>>}) -> 72;
%% These two values are technically invalid. An errata has already
%% been submitted to the RFC. We must however continue to include
%% them in the table for compatibility.
table_find_static({<<"access-control-allow-credentials">>, <<"FALSE">>}) -> 73;
table_find_static({<<"access-control-allow-credentials">>, <<"TRUE">>}) -> 74;
table_find_static({<<"access-control-allow-headers">>, <<"*">>}) -> 75;
table_find_static({<<"access-control-allow-methods">>, <<"get">>}) -> 76;
table_find_static({<<"access-control-allow-methods">>, <<"get, post, options">>}) -> 77;
table_find_static({<<"access-control-allow-methods">>, <<"options">>}) -> 78;
table_find_static({<<"access-control-expose-headers">>, <<"content-length">>}) -> 79;
table_find_static({<<"access-control-request-headers">>, <<"content-type">>}) -> 80;
table_find_static({<<"access-control-request-method">>, <<"get">>}) -> 81;
table_find_static({<<"access-control-request-method">>, <<"post">>}) -> 82;
table_find_static({<<"alt-svc">>, <<"clear">>}) -> 83;
table_find_static({<<"authorization">>, <<>>}) -> 84;
table_find_static({<<"content-security-policy">>, <<"script-src 'none'; object-src 'none'; base-uri 'none'">>}) -> 85;
table_find_static({<<"early-data">>, <<"1">>}) -> 86;
table_find_static({<<"expect-ct">>, <<>>}) -> 87;
table_find_static({<<"forwarded">>, <<>>}) -> 88;
table_find_static({<<"if-range">>, <<>>}) -> 89;
table_find_static({<<"origin">>, <<>>}) -> 90;
table_find_static({<<"purpose">>, <<"prefetch">>}) -> 91;
table_find_static({<<"server">>, <<>>}) -> 92;
table_find_static({<<"timing-allow-origin">>, <<"*">>}) -> 93;
table_find_static({<<"upgrade-insecure-requests">>, <<"1">>}) -> 94;
table_find_static({<<"user-agent">>, <<>>}) -> 95;
table_find_static({<<"x-forwarded-for">>, <<>>}) -> 96;
table_find_static({<<"x-frame-options">>, <<"deny">>}) -> 97;
table_find_static({<<"x-frame-options">>, <<"sameorigin">>}) -> 98;
table_find_static(_) -> not_found.

table_find_name_static(<<":authority">>) -> 0;
table_find_name_static(<<":path">>) -> 1;
table_find_name_static(<<"age">>) -> 2;
table_find_name_static(<<"content-disposition">>) -> 3;
table_find_name_static(<<"content-length">>) -> 4;
table_find_name_static(<<"cookie">>) -> 5;
table_find_name_static(<<"date">>) -> 6;
table_find_name_static(<<"etag">>) -> 7;
table_find_name_static(<<"if-modified-since">>) -> 8;
table_find_name_static(<<"if-none-match">>) -> 9;
table_find_name_static(<<"last-modified">>) -> 10;
table_find_name_static(<<"link">>) -> 11;
table_find_name_static(<<"location">>) -> 12;
table_find_name_static(<<"referer">>) -> 13;
table_find_name_static(<<"set-cookie">>) -> 14;
table_find_name_static(<<":method">>) -> 15;
table_find_name_static(<<":scheme">>) -> 22;
table_find_name_static(<<":status">>) -> 24;
table_find_name_static(<<"accept">>) -> 29;
table_find_name_static(<<"accept-encoding">>) -> 31;
table_find_name_static(<<"accept-ranges">>) -> 32;
table_find_name_static(<<"access-control-allow-headers">>) -> 33;
table_find_name_static(<<"access-control-allow-origin">>) -> 35;
table_find_name_static(<<"cache-control">>) -> 36;
table_find_name_static(<<"content-encoding">>) -> 42;
table_find_name_static(<<"content-type">>) -> 44;
table_find_name_static(<<"range">>) -> 55;
table_find_name_static(<<"strict-transport-security">>) -> 56;
table_find_name_static(<<"vary">>) -> 59;
table_find_name_static(<<"x-content-type-options">>) -> 61;
table_find_name_static(<<"x-xss-protection">>) -> 62;
table_find_name_static(<<"accept-language">>) -> 72;
table_find_name_static(<<"access-control-allow-credentials">>) -> 73;
table_find_name_static(<<"access-control-allow-methods">>) -> 76;
table_find_name_static(<<"access-control-expose-headers">>) -> 79;
table_find_name_static(<<"access-control-request-headers">>) -> 80;
table_find_name_static(<<"access-control-request-method">>) -> 81;
table_find_name_static(<<"alt-svc">>) -> 83;
table_find_name_static(<<"authorization">>) -> 84;
table_find_name_static(<<"content-security-policy">>) -> 85;
table_find_name_static(<<"early-data">>) -> 86;
table_find_name_static(<<"expect-ct">>) -> 87;
table_find_name_static(<<"forwarded">>) -> 88;
table_find_name_static(<<"if-range">>) -> 89;
table_find_name_static(<<"origin">>) -> 90;
table_find_name_static(<<"purpose">>) -> 91;
table_find_name_static(<<"server">>) -> 92;
table_find_name_static(<<"timing-allow-origin">>) -> 93;
table_find_name_static(<<"upgrade-insecure-requests">>) -> 94;
table_find_name_static(<<"user-agent">>) -> 95;
table_find_name_static(<<"x-forwarded-for">>) -> 96;
table_find_name_static(<<"x-frame-options">>) -> 97;
table_find_name_static(_) -> not_found.

table_get_static(0) -> {<<":authority">>, <<>>};
table_get_static(1) -> {<<":path">>, <<"/">>};
table_get_static(2) -> {<<"age">>, <<"0">>};
table_get_static(3) -> {<<"content-disposition">>, <<>>};
table_get_static(4) -> {<<"content-length">>, <<"0">>};
table_get_static(5) -> {<<"cookie">>, <<>>};
table_get_static(6) -> {<<"date">>, <<>>};
table_get_static(7) -> {<<"etag">>, <<>>};
table_get_static(8) -> {<<"if-modified-since">>, <<>>};
table_get_static(9) -> {<<"if-none-match">>, <<>>};
table_get_static(10) -> {<<"last-modified">>, <<>>};
table_get_static(11) -> {<<"link">>, <<>>};
table_get_static(12) -> {<<"location">>, <<>>};
table_get_static(13) -> {<<"referer">>, <<>>};
table_get_static(14) -> {<<"set-cookie">>, <<>>};
table_get_static(15) -> {<<":method">>, <<"CONNECT">>};
table_get_static(16) -> {<<":method">>, <<"DELETE">>};
table_get_static(17) -> {<<":method">>, <<"GET">>};
table_get_static(18) -> {<<":method">>, <<"HEAD">>};
table_get_static(19) -> {<<":method">>, <<"OPTIONS">>};
table_get_static(20) -> {<<":method">>, <<"POST">>};
table_get_static(21) -> {<<":method">>, <<"PUT">>};
table_get_static(22) -> {<<":scheme">>, <<"http">>};
table_get_static(23) -> {<<":scheme">>, <<"https">>};
table_get_static(24) -> {<<":status">>, <<"103">>};
table_get_static(25) -> {<<":status">>, <<"200">>};
table_get_static(26) -> {<<":status">>, <<"304">>};
table_get_static(27) -> {<<":status">>, <<"404">>};
table_get_static(28) -> {<<":status">>, <<"503">>};
table_get_static(29) -> {<<"accept">>, <<"*/*">>};
table_get_static(30) -> {<<"accept">>, <<"application/dns-message">>};
table_get_static(31) -> {<<"accept-encoding">>, <<"gzip, deflate, br">>};
table_get_static(32) -> {<<"accept-ranges">>, <<"bytes">>};
table_get_static(33) -> {<<"access-control-allow-headers">>, <<"cache-control">>};
table_get_static(34) -> {<<"access-control-allow-headers">>, <<"content-type">>};
table_get_static(35) -> {<<"access-control-allow-origin">>, <<"*">>};
table_get_static(36) -> {<<"cache-control">>, <<"max-age=0">>};
table_get_static(37) -> {<<"cache-control">>, <<"max-age=2592000">>};
table_get_static(38) -> {<<"cache-control">>, <<"max-age=604800">>};
table_get_static(39) -> {<<"cache-control">>, <<"no-cache">>};
table_get_static(40) -> {<<"cache-control">>, <<"no-store">>};
table_get_static(41) -> {<<"cache-control">>, <<"public, max-age=31536000">>};
table_get_static(42) -> {<<"content-encoding">>, <<"br">>};
table_get_static(43) -> {<<"content-encoding">>, <<"gzip">>};
table_get_static(44) -> {<<"content-type">>, <<"application/dns-message">>};
table_get_static(45) -> {<<"content-type">>, <<"application/javascript">>};
table_get_static(46) -> {<<"content-type">>, <<"application/json">>};
table_get_static(47) -> {<<"content-type">>, <<"application/x-www-form-urlencoded">>};
table_get_static(48) -> {<<"content-type">>, <<"image/gif">>};
table_get_static(49) -> {<<"content-type">>, <<"image/jpeg">>};
table_get_static(50) -> {<<"content-type">>, <<"image/png">>};
table_get_static(51) -> {<<"content-type">>, <<"text/css">>};
table_get_static(52) -> {<<"content-type">>, <<"text/html; charset=utf-8">>};
table_get_static(53) -> {<<"content-type">>, <<"text/plain">>};
table_get_static(54) -> {<<"content-type">>, <<"text/plain;charset=utf-8">>};
table_get_static(55) -> {<<"range">>, <<"bytes=0-">>};
table_get_static(56) -> {<<"strict-transport-security">>, <<"max-age=31536000">>};
table_get_static(57) -> {<<"strict-transport-security">>, <<"max-age=31536000; includesubdomains">>};
table_get_static(58) -> {<<"strict-transport-security">>, <<"max-age=31536000; includesubdomains; preload">>};
table_get_static(59) -> {<<"vary">>, <<"accept-encoding">>};
table_get_static(60) -> {<<"vary">>, <<"origin">>};
table_get_static(61) -> {<<"x-content-type-options">>, <<"nosniff">>};
table_get_static(62) -> {<<"x-xss-protection">>, <<"1; mode=block">>};
table_get_static(63) -> {<<":status">>, <<"100">>};
table_get_static(64) -> {<<":status">>, <<"204">>};
table_get_static(65) -> {<<":status">>, <<"206">>};
table_get_static(66) -> {<<":status">>, <<"302">>};
table_get_static(67) -> {<<":status">>, <<"400">>};
table_get_static(68) -> {<<":status">>, <<"403">>};
table_get_static(69) -> {<<":status">>, <<"421">>};
table_get_static(70) -> {<<":status">>, <<"425">>};
table_get_static(71) -> {<<":status">>, <<"500">>};
table_get_static(72) -> {<<"accept-language">>, <<>>};
%% These two values are technically invalid. An errata has already
%% been submitted to the RFC. We must however continue to include
%% them in the table for compatibility.
table_get_static(73) -> {<<"access-control-allow-credentials">>, <<"FALSE">>};
table_get_static(74) -> {<<"access-control-allow-credentials">>, <<"TRUE">>};
table_get_static(75) -> {<<"access-control-allow-headers">>, <<"*">>};
table_get_static(76) -> {<<"access-control-allow-methods">>, <<"get">>};
table_get_static(77) -> {<<"access-control-allow-methods">>, <<"get, post, options">>};
table_get_static(78) -> {<<"access-control-allow-methods">>, <<"options">>};
table_get_static(79) -> {<<"access-control-expose-headers">>, <<"content-length">>};
table_get_static(80) -> {<<"access-control-request-headers">>, <<"content-type">>};
table_get_static(81) -> {<<"access-control-request-method">>, <<"get">>};
table_get_static(82) -> {<<"access-control-request-method">>, <<"post">>};
table_get_static(83) -> {<<"alt-svc">>, <<"clear">>};
table_get_static(84) -> {<<"authorization">>, <<>>};
table_get_static(85) -> {<<"content-security-policy">>, <<"script-src 'none'; object-src 'none'; base-uri 'none'">>};
table_get_static(86) -> {<<"early-data">>, <<"1">>};
table_get_static(87) -> {<<"expect-ct">>, <<>>};
table_get_static(88) -> {<<"forwarded">>, <<>>};
table_get_static(89) -> {<<"if-range">>, <<>>};
table_get_static(90) -> {<<"origin">>, <<>>};
table_get_static(91) -> {<<"purpose">>, <<"prefetch">>};
table_get_static(92) -> {<<"server">>, <<>>};
table_get_static(93) -> {<<"timing-allow-origin">>, <<"*">>};
table_get_static(94) -> {<<"upgrade-insecure-requests">>, <<"1">>};
table_get_static(95) -> {<<"user-agent">>, <<>>};
table_get_static(96) -> {<<"x-forwarded-for">>, <<>>};
table_get_static(97) -> {<<"x-frame-options">>, <<"deny">>};
table_get_static(98) -> {<<"x-frame-options">>, <<"sameorigin">>}.

table_get_name_static(0) -> <<":authority">>;
table_get_name_static(1) -> <<":path">>;
table_get_name_static(2) -> <<"age">>;
table_get_name_static(3) -> <<"content-disposition">>;
table_get_name_static(4) -> <<"content-length">>;
table_get_name_static(5) -> <<"cookie">>;
table_get_name_static(6) -> <<"date">>;
table_get_name_static(7) -> <<"etag">>;
table_get_name_static(8) -> <<"if-modified-since">>;
table_get_name_static(9) -> <<"if-none-match">>;
table_get_name_static(10) -> <<"last-modified">>;
table_get_name_static(11) -> <<"link">>;
table_get_name_static(12) -> <<"location">>;
table_get_name_static(13) -> <<"referer">>;
table_get_name_static(14) -> <<"set-cookie">>;
table_get_name_static(15) -> <<":method">>;
table_get_name_static(16) -> <<":method">>;
table_get_name_static(17) -> <<":method">>;
table_get_name_static(18) -> <<":method">>;
table_get_name_static(19) -> <<":method">>;
table_get_name_static(20) -> <<":method">>;
table_get_name_static(21) -> <<":method">>;
table_get_name_static(22) -> <<":scheme">>;
table_get_name_static(23) -> <<":scheme">>;
table_get_name_static(24) -> <<":status">>;
table_get_name_static(25) -> <<":status">>;
table_get_name_static(26) -> <<":status">>;
table_get_name_static(27) -> <<":status">>;
table_get_name_static(28) -> <<":status">>;
table_get_name_static(29) -> <<"accept">>;
table_get_name_static(30) -> <<"accept">>;
table_get_name_static(31) -> <<"accept-encoding">>;
table_get_name_static(32) -> <<"accept-ranges">>;
table_get_name_static(33) -> <<"access-control-allow-headers">>;
table_get_name_static(34) -> <<"access-control-allow-headers">>;
table_get_name_static(35) -> <<"access-control-allow-origin">>;
table_get_name_static(36) -> <<"cache-control">>;
table_get_name_static(37) -> <<"cache-control">>;
table_get_name_static(38) -> <<"cache-control">>;
table_get_name_static(39) -> <<"cache-control">>;
table_get_name_static(40) -> <<"cache-control">>;
table_get_name_static(41) -> <<"cache-control">>;
table_get_name_static(42) -> <<"content-encoding">>;
table_get_name_static(43) -> <<"content-encoding">>;
table_get_name_static(44) -> <<"content-type">>;
table_get_name_static(45) -> <<"content-type">>;
table_get_name_static(46) -> <<"content-type">>;
table_get_name_static(47) -> <<"content-type">>;
table_get_name_static(48) -> <<"content-type">>;
table_get_name_static(49) -> <<"content-type">>;
table_get_name_static(50) -> <<"content-type">>;
table_get_name_static(51) -> <<"content-type">>;
table_get_name_static(52) -> <<"content-type">>;
table_get_name_static(53) -> <<"content-type">>;
table_get_name_static(54) -> <<"content-type">>;
table_get_name_static(55) -> <<"range">>;
table_get_name_static(56) -> <<"strict-transport-security">>;
table_get_name_static(57) -> <<"strict-transport-security">>;
table_get_name_static(58) -> <<"strict-transport-security">>;
table_get_name_static(59) -> <<"vary">>;
table_get_name_static(60) -> <<"vary">>;
table_get_name_static(61) -> <<"x-content-type-options">>;
table_get_name_static(62) -> <<"x-xss-protection">>;
table_get_name_static(63) -> <<":status">>;
table_get_name_static(64) -> <<":status">>;
table_get_name_static(65) -> <<":status">>;
table_get_name_static(66) -> <<":status">>;
table_get_name_static(67) -> <<":status">>;
table_get_name_static(68) -> <<":status">>;
table_get_name_static(69) -> <<":status">>;
table_get_name_static(70) -> <<":status">>;
table_get_name_static(71) -> <<":status">>;
table_get_name_static(72) -> <<"accept-language">>;
table_get_name_static(73) -> <<"access-control-allow-credentials">>;
table_get_name_static(74) -> <<"access-control-allow-credentials">>;
table_get_name_static(75) -> <<"access-control-allow-headers">>;
table_get_name_static(76) -> <<"access-control-allow-methods">>;
table_get_name_static(77) -> <<"access-control-allow-methods">>;
table_get_name_static(78) -> <<"access-control-allow-methods">>;
table_get_name_static(79) -> <<"access-control-expose-headers">>;
table_get_name_static(80) -> <<"access-control-request-headers">>;
table_get_name_static(81) -> <<"access-control-request-method">>;
table_get_name_static(82) -> <<"access-control-request-method">>;
table_get_name_static(83) -> <<"alt-svc">>;
table_get_name_static(84) -> <<"authorization">>;
table_get_name_static(85) -> <<"content-security-policy">>;
table_get_name_static(86) -> <<"early-data">>;
table_get_name_static(87) -> <<"expect-ct">>;
table_get_name_static(88) -> <<"forwarded">>;
table_get_name_static(89) -> <<"if-range">>;
table_get_name_static(90) -> <<"origin">>;
table_get_name_static(91) -> <<"purpose">>;
table_get_name_static(92) -> <<"server">>;
table_get_name_static(93) -> <<"timing-allow-origin">>;
table_get_name_static(94) -> <<"upgrade-insecure-requests">>;
table_get_name_static(95) -> <<"user-agent">>;
table_get_name_static(96) -> <<"x-forwarded-for">>;
table_get_name_static(97) -> <<"x-frame-options">>;
table_get_name_static(98) -> <<"x-frame-options">>.

table_insert(Entry={Name, Value}, State=#state{capacity=Capacity,
		size=Size0, insert_count=InsertCount, dyn_table=DynamicTable0,
		draining_size=DrainingSize}) ->
	EntrySize = byte_size(Name) + byte_size(Value) + 32,
	if
		EntrySize + Size0 =< Capacity ->
			{ok, State#state{size=Size0 + EntrySize, insert_count=InsertCount + 1,
				dyn_table=[{EntrySize, Entry}|DynamicTable0]}};
		EntrySize =< Capacity ->
			{DynamicTable, Size} = table_evict(DynamicTable0,
				Capacity - EntrySize, 0, []),
			{ok, State#state{size=Size + EntrySize, insert_count=InsertCount + 1,
				dyn_table=[{EntrySize, Entry}|DynamicTable],
				%% We reduce the draining size by how much was gained from evicting.
				draining_size=DrainingSize - (Size0 - Size)}};
		true -> % EntrySize > Capacity ->
			{connection_error, qpack_encoder_stream_error,
				'Entry size larger than table capacity. (RFC9204 3.2.2)'}
	end.

table_evict([], _, Size, Acc) ->
	{lists:reverse(Acc), Size};
table_evict([{EntrySize, _}|_], MaxSize, Size, Acc)
		when Size + EntrySize > MaxSize ->
	{lists:reverse(Acc), Size};
table_evict([Entry = {EntrySize, _}|Tail], MaxSize, Size, Acc) ->
	table_evict(Tail, MaxSize, Size + EntrySize, [Entry|Acc]).

table_find_dyn(Entry, #state{insert_count=InsertCount, dyn_table=DynamicTable}) ->
	table_find_dyn(Entry, DynamicTable, InsertCount - 1).

table_find_dyn(_, [], _) ->
	not_found;
table_find_dyn(Entry, [{_, Entry}|_], Index) ->
	Index;
table_find_dyn(Entry, [_|Tail], Index) ->
	table_find_dyn(Entry, Tail, Index - 1).

table_find_name_dyn(Name, #state{insert_count=InsertCount, dyn_table=DynamicTable}) ->
	table_find_name_dyn(Name, DynamicTable, InsertCount - 1).

table_find_name_dyn(_, [], _) ->
	not_found;
table_find_name_dyn(Name, [{_, {Name, _}}|_], Index) ->
	Index;
table_find_name_dyn(Name, [_|Tail], Index) ->
	table_find_name_dyn(Name, Tail, Index - 1).

%% @todo These functions may error out if the encoder is invalid (2.2.3. Invalid References).
table_get_dyn_abs(Index, #state{insert_count=InsertCount, dyn_table=DynamicTable}) ->
	{_, Header} = lists:nth(InsertCount - Index, DynamicTable),
	Header.

table_get_dyn_rel(Index, #state{dyn_table=DynamicTable}) ->
	{_, Header} = lists:nth(1 + Index, DynamicTable),
	Header.

table_get_name_dyn_rel(Index, State) ->
	{Name, _} = table_get_dyn_rel(Index, State),
	Name.

table_get_dyn_pre_base(Index, Base, #state{insert_count=InsertCount, dyn_table=DynamicTable}) ->
	BaseOffset = InsertCount - Base,
	{_, Header} = lists:nth(1 + Index + BaseOffset, DynamicTable),
	Header.

table_get_dyn_post_base(Index, Base, State) ->
	table_get_dyn_abs(Base + Index, State).

table_get_name_dyn_post_base(Index, Base, State) ->
	{Name, _} = table_get_dyn_abs(Base + Index, State),
	Name.

-ifdef(TEST).
do_init() ->
	#state{
		settings_received=false,
		max_table_capacity=1000,
		capacity=1000
	}.

do_table_insert(Entry, State0) ->
	{ok, State} = table_insert(Entry, State0),
	State.

table_get_dyn_abs_test() ->
	State0 = do_init(),
	State1 = do_table_insert({<<"g">>, <<"h">>},
		do_table_insert({<<"e">>, <<"f">>},
		do_table_insert({<<"c">>, <<"d">>},
		do_table_insert({<<"a">>, <<"b">>},
		State0)))),
	{<<"a">>, <<"b">>} = table_get_dyn_abs(0, State1),
	{<<"c">>, <<"d">>} = table_get_dyn_abs(1, State1),
	{<<"e">>, <<"f">>} = table_get_dyn_abs(2, State1),
	{<<"g">>, <<"h">>} = table_get_dyn_abs(3, State1),
	%% Evict one member from the table.
	#state{dyn_table=DynamicTable} = State1,
	State2 = State1#state{dyn_table=lists:reverse(tl(lists:reverse(DynamicTable)))},
	{<<"c">>, <<"d">>} = table_get_dyn_abs(1, State2),
	{<<"e">>, <<"f">>} = table_get_dyn_abs(2, State2),
	{<<"g">>, <<"h">>} = table_get_dyn_abs(3, State2),
	ok.

table_get_dyn_rel_test() ->
	State0 = do_init(),
	State1 = do_table_insert({<<"g">>, <<"h">>},
		do_table_insert({<<"e">>, <<"f">>},
		do_table_insert({<<"c">>, <<"d">>},
		do_table_insert({<<"a">>, <<"b">>},
		State0)))),
	{<<"g">>, <<"h">>} = table_get_dyn_rel(0, State1),
	{<<"e">>, <<"f">>} = table_get_dyn_rel(1, State1),
	{<<"c">>, <<"d">>} = table_get_dyn_rel(2, State1),
	{<<"a">>, <<"b">>} = table_get_dyn_rel(3, State1),
	%% Evict one member from the table.
	#state{dyn_table=DynamicTable} = State1,
	State2 = State1#state{dyn_table=lists:reverse(tl(lists:reverse(DynamicTable)))},
	{<<"g">>, <<"h">>} = table_get_dyn_rel(0, State2),
	{<<"e">>, <<"f">>} = table_get_dyn_rel(1, State2),
	{<<"c">>, <<"d">>} = table_get_dyn_rel(2, State2),
	%% Add a member to the table.
	State3 = do_table_insert({<<"i">>, <<"j">>}, State2),
	{<<"i">>, <<"j">>} = table_get_dyn_rel(0, State3),
	{<<"g">>, <<"h">>} = table_get_dyn_rel(1, State3),
	{<<"e">>, <<"f">>} = table_get_dyn_rel(2, State3),
	{<<"c">>, <<"d">>} = table_get_dyn_rel(3, State3),
	ok.

table_get_dyn_pre_base_test() ->
	State0 = do_init(),
	State1 = do_table_insert({<<"g">>, <<"h">>},
		do_table_insert({<<"e">>, <<"f">>},
		do_table_insert({<<"c">>, <<"d">>},
		do_table_insert({<<"a">>, <<"b">>},
		State0)))),
	{<<"e">>, <<"f">>} = table_get_dyn_pre_base(0, 3, State1),
	{<<"c">>, <<"d">>} = table_get_dyn_pre_base(1, 3, State1),
	{<<"a">>, <<"b">>} = table_get_dyn_pre_base(2, 3, State1),
	%% Evict one member from the table.
	#state{dyn_table=DynamicTable} = State1,
	State2 = State1#state{dyn_table=lists:reverse(tl(lists:reverse(DynamicTable)))},
	{<<"e">>, <<"f">>} = table_get_dyn_pre_base(0, 3, State2),
	{<<"c">>, <<"d">>} = table_get_dyn_pre_base(1, 3, State2),
	%% Add a member to the table.
	State3 = do_table_insert({<<"i">>, <<"j">>}, State2),
	{<<"e">>, <<"f">>} = table_get_dyn_pre_base(0, 3, State3),
	{<<"c">>, <<"d">>} = table_get_dyn_pre_base(1, 3, State3),
	ok.

table_get_dyn_post_base_test() ->
	State0 = do_init(),
	State1 = do_table_insert({<<"g">>, <<"h">>},
		do_table_insert({<<"e">>, <<"f">>},
		do_table_insert({<<"c">>, <<"d">>},
		do_table_insert({<<"a">>, <<"b">>},
		State0)))),
	{<<"e">>, <<"f">>} = table_get_dyn_post_base(0, 2, State1),
	{<<"g">>, <<"h">>} = table_get_dyn_post_base(1, 2, State1),
	%% Evict one member from the table.
	#state{dyn_table=DynamicTable} = State1,
	State2 = State1#state{dyn_table=lists:reverse(tl(lists:reverse(DynamicTable)))},
	{<<"e">>, <<"f">>} = table_get_dyn_post_base(0, 2, State2),
	{<<"g">>, <<"h">>} = table_get_dyn_post_base(1, 2, State2),
	%% Add a member to the table.
	State3 = do_table_insert({<<"i">>, <<"j">>}, State2),
	{<<"e">>, <<"f">>} = table_get_dyn_post_base(0, 2, State3),
	{<<"g">>, <<"h">>} = table_get_dyn_post_base(1, 2, State3),
	{<<"i">>, <<"j">>} = table_get_dyn_post_base(2, 2, State3),
	ok.
-endif.
