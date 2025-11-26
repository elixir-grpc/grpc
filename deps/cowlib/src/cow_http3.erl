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

-module(cow_http3).

%% Parsing.
-export([parse/1]).
-export([parse_unidi_stream_header/1]).
-export([parse_datagram/1]).
-export([code_to_error/1]).
-export([parse_int/1]).

%% Building.
-export([data/1]).
-export([headers/1]).
-export([settings/1]).
-export([webtransport_stream_header/2]).
-export([datagram/2]).
-export([error_to_code/1]).
-export([encode_int/1]).

-type stream_id() :: non_neg_integer().
-export_type([stream_id/0]).

-type push_id() :: non_neg_integer().
-export_type([push_id/0]).

-type h3_non_neg_integer() :: 0..16#3fffffffffffffff.

-type settings() :: #{
	qpack_max_table_capacity => h3_non_neg_integer(),
	max_field_section_size => h3_non_neg_integer(),
	qpack_blocked_streams => h3_non_neg_integer(),
	enable_connect_protocol => boolean(),
	%% Extensions.
	h3_datagram => boolean(),
	wt_max_sessions => h3_non_neg_integer(),
	wt_initial_max_streams_uni => h3_non_neg_integer(),
	wt_initial_max_streams_bidi => h3_non_neg_integer(),
	wt_initial_max_data => h3_non_neg_integer()
}.
-export_type([settings/0]).

-type wt_app_error_code() :: 0..16#ffffffff.
-export_type([wt_app_error_code/0]).

-type error() :: h3_no_error
	| h3_general_protocol_error
	| h3_internal_error
	| h3_stream_creation_error
	| h3_closed_critical_stream
	| h3_frame_unexpected
	| h3_frame_error
	| h3_excessive_load
	| h3_id_error
	| h3_settings_error
	| h3_missing_settings
	| h3_request_rejected
	| h3_request_cancelled
	| h3_request_incomplete
	| h3_message_error
	| h3_connect_error
	| h3_version_fallback
	%% Extensions.
	| h3_datagram_error
	| wt_buffered_stream_rejected
	| wt_session_gone
	| {wt_application_error, wt_app_error_code()}.
-export_type([error/0]).

-type frame() :: {data, binary()}
	| {headers, binary()}
	| {cancel_push, push_id()}
	| {settings, settings()}
	| {push_promise, push_id(), binary()}
	| {goaway, stream_id() | push_id()}
	| {max_push_id, push_id()}.
-export_type([frame/0]).

%% Parsing.

-spec parse(binary())
	-> {ok, frame(), binary()}
	| {webtransport_stream_header, stream_id(), binary()}
	| {more, {data, binary()} | ignore, non_neg_integer()}
	| {ignore, binary()}
	| {connection_error, h3_frame_error | h3_frame_unexpected | h3_settings_error, atom()}
	| more.

%%
%% DATA frames.
%%
parse(<<0, 0:2, Len:6, Data:Len/binary, Rest/bits>>) ->
	{ok, {data, Data}, Rest};
parse(<<0, 1:2, Len:14, Data:Len/binary, Rest/bits>>) ->
	{ok, {data, Data}, Rest};
parse(<<0, 2:2, Len:30, Data:Len/binary, Rest/bits>>) ->
	{ok, {data, Data}, Rest};
parse(<<0, 3:2, Len:62, Data:Len/binary, Rest/bits>>) ->
	{ok, {data, Data}, Rest};
%% DATA frames may be split over multiple QUIC packets
%% but we want to process them immediately rather than
%% risk buffering a very large payload.
parse(<<0, 0:2, Len:6, Data/bits>>) when byte_size(Data) < Len ->
	{more, {data, Data}, Len - byte_size(Data)};
parse(<<0, 1:2, Len:14, Data/bits>>) when byte_size(Data) < Len ->
	{more, {data, Data}, Len - byte_size(Data)};
parse(<<0, 2:2, Len:30, Data/bits>>) when byte_size(Data) < Len ->
	{more, {data, Data}, Len - byte_size(Data)};
parse(<<0, 3:2, Len:62, Data/bits>>) when byte_size(Data) < Len ->
	{more, {data, Data}, Len - byte_size(Data)};
%%
%% HEADERS frames.
%%
parse(<<1, 0:2, 0:6, _/bits>>) ->
	{connection_error, h3_frame_error,
		'HEADERS frames payload CANNOT be 0 bytes wide. (RFC9114 7.1, RFC9114 7.2.2)'};
parse(<<1, 1:2, 0:14, _/bits>>) ->
	{connection_error, h3_frame_error,
		'HEADERS frames payload CANNOT be 0 bytes wide. (RFC9114 7.1, RFC9114 7.2.2)'};
parse(<<1, 2:2, 0:30, _/bits>>) ->
	{connection_error, h3_frame_error,
		'HEADERS frames payload CANNOT be 0 bytes wide. (RFC9114 7.1, RFC9114 7.2.2)'};
parse(<<1, 3:2, 0:62, _/bits>>) ->
	{connection_error, h3_frame_error,
		'HEADERS frames payload CANNOT be 0 bytes wide. (RFC9114 7.1, RFC9114 7.2.2)'};
parse(<<1, 0:2, Len:6, EncodedFieldSection:Len/binary, Rest/bits>>) ->
	{ok, {headers, EncodedFieldSection}, Rest};
parse(<<1, 1:2, Len:14, EncodedFieldSection:Len/binary, Rest/bits>>) ->
	{ok, {headers, EncodedFieldSection}, Rest};
parse(<<1, 2:2, Len:30, EncodedFieldSection:Len/binary, Rest/bits>>) ->
	{ok, {headers, EncodedFieldSection}, Rest};
parse(<<1, 3:2, Len:62, EncodedFieldSection:Len/binary, Rest/bits>>) ->
	{ok, {headers, EncodedFieldSection}, Rest};
%%
%% CANCEL_PUSH frames.
%%
parse(<<3, 0:2, 1:6, 0:2, PushID:6, Rest/bits>>) ->
	{ok, {cancel_push, PushID}, Rest};
parse(<<3, 0:2, 2:6, 1:2, PushID:14, Rest/bits>>) ->
	{ok, {cancel_push, PushID}, Rest};
parse(<<3, 0:2, 4:6, 2:2, PushID:30, Rest/bits>>) ->
	{ok, {cancel_push, PushID}, Rest};
parse(<<3, 0:2, 8:6, 3:2, PushID:62, Rest/bits>>) ->
	{ok, {cancel_push, PushID}, Rest};
parse(<<3, _/bits>>) ->
	{connection_error, h3_frame_error,
		'CANCEL_PUSH frames payload MUST be 1, 2, 4 or 8 bytes wide. (RFC9114 7.1, RFC9114 7.2.3)'};
%%
%% SETTINGS frames.
%%
parse(<<4, 0:2, Len:6, Rest/bits>>) when byte_size(Rest) >= Len ->
	parse_settings_id(Rest, Len, #{});
parse(<<4, 1:2, Len:14, Rest/bits>>) when byte_size(Rest) >= Len ->
	parse_settings_id(Rest, Len, #{});
parse(<<4, 2:2, Len:30, Rest/bits>>) when byte_size(Rest) >= Len ->
	parse_settings_id(Rest, Len, #{});
parse(<<4, 3:2, Len:62, Rest/bits>>) when byte_size(Rest) >= Len ->
	parse_settings_id(Rest, Len, #{});
%%
%% PUSH_PROMISE frames.
%%
parse(<<5, 0:2, Len:6, Rest/bits>>) when byte_size(Rest) >= Len ->
	parse_push_promise(Rest, Len);
parse(<<5, 1:2, Len:14, Rest/bits>>) when byte_size(Rest) >= Len ->
	parse_push_promise(Rest, Len);
parse(<<5, 2:2, Len:30, Rest/bits>>) when byte_size(Rest) >= Len ->
	parse_push_promise(Rest, Len);
parse(<<5, 3:2, Len:62, Rest/bits>>) when byte_size(Rest) >= Len ->
	parse_push_promise(Rest, Len);
%%
%% GOAWAY frames.
%%
parse(<<7, 0:2, 1:6, 0:2, StreamOrPushID:6, Rest/bits>>) ->
	{ok, {goaway, StreamOrPushID}, Rest};
parse(<<7, 0:2, 2:6, 1:2, StreamOrPushID:14, Rest/bits>>) ->
	{ok, {goaway, StreamOrPushID}, Rest};
parse(<<7, 0:2, 4:6, 2:2, StreamOrPushID:30, Rest/bits>>) ->
	{ok, {goaway, StreamOrPushID}, Rest};
parse(<<7, 0:2, 8:6, 3:2, StreamOrPushID:62, Rest/bits>>) ->
	{ok, {goaway, StreamOrPushID}, Rest};
parse(<<7, 0:2, N:6, _/bits>>) when N =:= 1; N =:= 2; N =:= 4; N =:= 8 ->
	more;
parse(<<7, _/bits>>) ->
	{connection_error, h3_frame_error,
		'GOAWAY frames payload MUST be 1, 2, 4 or 8 bytes wide. (RFC9114 7.1, RFC9114 7.2.6)'};
%%
%% MAX_PUSH_ID frames.
%%
parse(<<13, 0:2, 1:6, 0:2, PushID:6, Rest/bits>>) ->
	{ok, {max_push_id, PushID}, Rest};
parse(<<13, 0:2, 2:6, 1:2, PushID:14, Rest/bits>>) ->
	{ok, {max_push_id, PushID}, Rest};
parse(<<13, 0:2, 4:6, 2:2, PushID:30, Rest/bits>>) ->
	{ok, {max_push_id, PushID}, Rest};
parse(<<13, 0:2, 8:6, 3:2, PushID:62, Rest/bits>>) ->
	{ok, {max_push_id, PushID}, Rest};
parse(<<13, 0:2, N:6, _/bits>>) when N =:= 1; N =:= 2; N =:= 4; N =:= 8 ->
	more;
parse(<<13, _/bits>>) ->
	{connection_error, h3_frame_error,
		'MAX_PUSH_ID frames payload MUST be 1, 2, 4 or 8 bytes wide. (RFC9114 7.1, RFC9114 7.2.6)'};
%%
%% WebTransport stream header.
%%
parse(<<1:2, 16#41:14, 0:2, SessionID:6, Rest/bits>>) ->
	{webtransport_stream_header, SessionID, Rest};
parse(<<1:2, 16#41:14, 1:2, SessionID:14, Rest/bits>>) ->
	{webtransport_stream_header, SessionID, Rest};
parse(<<1:2, 16#41:14, 2:2, SessionID:30, Rest/bits>>) ->
	{webtransport_stream_header, SessionID, Rest};
parse(<<1:2, 16#41:14, 3:2, SessionID:62, Rest/bits>>) ->
	{webtransport_stream_header, SessionID, Rest};
parse(<<16#41, _/bits>>) ->
	more;
%%
%% HTTP/2 frame types must be rejected.
%%
parse(<<2, _/bits>>) ->
	{connection_error, h3_frame_unexpected,
		'HTTP/2 PRIORITY frame not defined for HTTP/3 must be rejected. (RFC9114 7.2.8)'};
parse(<<6, _/bits>>) ->
	{connection_error, h3_frame_unexpected,
		'HTTP/2 PING frame not defined for HTTP/3 must be rejected. (RFC9114 7.2.8)'};
parse(<<8, _/bits>>) ->
	{connection_error, h3_frame_unexpected,
		'HTTP/2 WINDOW_UPDATE frame not defined for HTTP/3 must be rejected. (RFC9114 7.2.8)'};
parse(<<9, _/bits>>) ->
	{connection_error, h3_frame_unexpected,
		'HTTP/2 CONTINUATION frame not defined for HTTP/3 must be rejected. (RFC9114 7.2.8)'};
%%
%% Unknown frames must be ignored.
parse(<<0:2, Type:6, 0:2, Len:6, Rest/bits>>)
		when Type =:= 10; Type =:= 11; Type =:= 12; Type > 13 ->
	parse_ignore(Rest, Len);
parse(<<0:2, Type:6, 1:2, Len:14, Rest/bits>>)
		when Type =:= 10; Type =:= 11; Type =:= 12; Type > 13 ->
	parse_ignore(Rest, Len);
parse(<<0:2, Type:6, 2:2, Len:30, Rest/bits>>)
		when Type =:= 10; Type =:= 11; Type =:= 12; Type > 13 ->
	parse_ignore(Rest, Len);
parse(<<0:2, Type:6, 3:2, Len:62, Rest/bits>>)
		when Type =:= 10; Type =:= 11; Type =:= 12; Type > 13 ->
	parse_ignore(Rest, Len);
parse(<<1:2, _:14, 0:2, Len:6, Rest/bits>>) ->
	parse_ignore(Rest, Len);
parse(<<1:2, _:14, 1:2, Len:14, Rest/bits>>) ->
	parse_ignore(Rest, Len);
parse(<<1:2, _:14, 2:2, Len:30, Rest/bits>>) ->
	parse_ignore(Rest, Len);
parse(<<1:2, _:14, 3:2, Len:62, Rest/bits>>) ->
	parse_ignore(Rest, Len);
parse(<<2:2, _:30, 0:2, Len:6, Rest/bits>>) ->
	parse_ignore(Rest, Len);
parse(<<2:2, _:30, 1:2, Len:14, Rest/bits>>) ->
	parse_ignore(Rest, Len);
parse(<<2:2, _:30, 2:2, Len:30, Rest/bits>>) ->
	parse_ignore(Rest, Len);
parse(<<2:2, _:30, 3:2, Len:62, Rest/bits>>) ->
	parse_ignore(Rest, Len);
parse(<<3:2, _:62, 0:2, Len:6, Rest/bits>>) ->
	parse_ignore(Rest, Len);
parse(<<3:2, _:62, 1:2, Len:14, Rest/bits>>) ->
	parse_ignore(Rest, Len);
parse(<<3:2, _:62, 2:2, Len:30, Rest/bits>>) ->
	parse_ignore(Rest, Len);
parse(<<3:2, _:62, 3:2, Len:62, Rest/bits>>) ->
	parse_ignore(Rest, Len);
%%
%% Incomplete frames for those we fully process only.
%%
parse(_) ->
	more.

parse_settings_id(Rest, 0, Settings) ->
	{ok, {settings, Settings}, Rest};
parse_settings_id(<<0:2, Identifier:6, Rest/bits>>, Len, Settings) when Len >= 1 ->
	parse_settings_val(Rest, Len - 1, Settings, Identifier);
parse_settings_id(<<1:2, Identifier:14, Rest/bits>>, Len, Settings) when Len >= 2 ->
	parse_settings_val(Rest, Len - 2, Settings, Identifier);
parse_settings_id(<<2:2, Identifier:30, Rest/bits>>, Len, Settings) when Len >= 4 ->
	parse_settings_val(Rest, Len - 4, Settings, Identifier);
parse_settings_id(<<3:2, Identifier:62, Rest/bits>>, Len, Settings) when Len >= 8 ->
	parse_settings_val(Rest, Len - 8, Settings, Identifier);
parse_settings_id(_, _, _) ->
	{connection_error, h3_frame_error,
		'SETTINGS payload size exceeds the length given. (RFC9114 7.1, RFC9114 7.2.4)'}.

parse_settings_val(<<0:2, Value:6, Rest/bits>>, Len, Settings, Identifier) when Len >= 1 ->
	parse_settings_id_val(Rest, Len - 1, Settings, Identifier, Value);
parse_settings_val(<<1:2, Value:14, Rest/bits>>, Len, Settings, Identifier) when Len >= 2 ->
	parse_settings_id_val(Rest, Len - 2, Settings, Identifier, Value);
parse_settings_val(<<2:2, Value:30, Rest/bits>>, Len, Settings, Identifier) when Len >= 4 ->
	parse_settings_id_val(Rest, Len - 4, Settings, Identifier, Value);
parse_settings_val(<<3:2, Value:62, Rest/bits>>, Len, Settings, Identifier) when Len >= 8 ->
	parse_settings_id_val(Rest, Len - 8, Settings, Identifier, Value);
parse_settings_val(_, _, _, _) ->
	{connection_error, h3_frame_error,
		'SETTINGS payload size exceeds the length given. (RFC9114 7.1, RFC9114 7.2.4)'}.

parse_settings_id_val(Rest, Len, Settings, Identifier, Value) ->
	case Identifier of
		%% SETTINGS_QPACK_MAX_TABLE_CAPACITY (RFC9204).
		1 ->
			parse_settings_key_val(Rest, Len, Settings, qpack_max_table_capacity, Value);
		%% SETTINGS_MAX_FIELD_SECTION_SIZE (RFC9114).
		6 ->
			parse_settings_key_val(Rest, Len, Settings, max_field_section_size, Value);
		%% SETTINGS_QPACK_BLOCKED_STREAMS (RFC9204).
		7 ->
			parse_settings_key_val(Rest, Len, Settings, qpack_blocked_streams, Value);
		%% SETTINGS_ENABLE_CONNECT_PROTOCOL (RFC9220).
		8 when Value =:= 0 ->
			parse_settings_key_val(Rest, Len, Settings, enable_connect_protocol, false);
		8 when Value =:= 1 ->
			parse_settings_key_val(Rest, Len, Settings, enable_connect_protocol, true);
		8 ->
			{connection_error, h3_settings_error,
				'The SETTINGS_ENABLE_CONNECT_PROTOCOL value MUST be 0 or 1. (RFC9220 3, RFC8441 3)'};
		%% SETTINGS_H3_DATAGRAM (RFC9297).
		16#33 when Value =:= 0 ->
			parse_settings_key_val(Rest, Len, Settings, h3_datagram, false);
		16#33 when Value =:= 1 ->
			parse_settings_key_val(Rest, Len, Settings, h3_datagram, true);
		16#33 ->
			{connection_error, h3_settings_error,
				'The SETTINGS_H3_DATAGRAM value MUST be 0 or 1. (RFC9297 2.1.1)'};
		%% SETTINGS_WT_MAX_SESSIONS (draft-ietf-webtrans-http3).
		16#c671706a ->
			parse_settings_key_val(Rest, Len, Settings, wt_max_sessions, Value);
		%% SETTINGS_WT_INITIAL_MAX_STREAMS_UNI (draft-ietf-webtrans-http3).
		16#2b64 ->
			parse_settings_key_val(Rest, Len, Settings, wt_initial_max_streams_uni, Value);
		%% SETTINGS_WT_INITIAL_MAX_STREAMS_BIDI (draft-ietf-webtrans-http3).
		16#2b65 ->
			parse_settings_key_val(Rest, Len, Settings, wt_initial_max_streams_bidi, Value);
		%% SETTINGS_WT_INITIAL_MAX_DATA (draft-ietf-webtrans-http3).
		16#2b61 ->
			parse_settings_key_val(Rest, Len, Settings, wt_initial_max_data, Value);
		_ when Identifier < 6 ->
			{connection_error, h3_settings_error,
				'HTTP/2 setting not defined for HTTP/3 must be rejected. (RFC9114 7.2.4.1)'};
		%% Unknown settings must be ignored.
		_ ->
			parse_settings_id(Rest, Len, Settings)
	end.

parse_settings_key_val(Rest, Len, Settings, Key, Value) ->
	case Settings of
		#{Key := _} ->
			{connection_error, h3_settings_error,
				'A duplicate setting identifier was found. (RFC9114 7.2.4)'};
		_ ->
			parse_settings_id(Rest, Len, Settings#{Key => Value})
	end.

parse_push_promise(<<0:2, PushID:6, Data/bits>>, Len) ->
	<<EncodedFieldSection:(Len - 1)/bytes, Rest/bits>> = Data,
	{ok, {push_promise, PushID, EncodedFieldSection}, Rest};
parse_push_promise(<<1:2, PushID:14, Data/bits>>, Len) ->
	<<EncodedFieldSection:(Len - 2)/bytes, Rest/bits>> = Data,
	{ok, {push_promise, PushID, EncodedFieldSection}, Rest};
parse_push_promise(<<2:2, PushID:30, Data/bits>>, Len) ->
	<<EncodedFieldSection:(Len - 4)/bytes, Rest/bits>> = Data,
	{ok, {push_promise, PushID, EncodedFieldSection}, Rest};
parse_push_promise(<<3:2, PushID:62, Data/bits>>, Len) ->
	<<EncodedFieldSection:(Len - 8)/bytes, Rest/bits>> = Data,
	{ok, {push_promise, PushID, EncodedFieldSection}, Rest}.

%% Large ignored frames could lead to DoS. Users of
%% this module must limit the size of such frames.
parse_ignore(Data, Len) ->
	case Data of
		<<_:Len/binary, Rest/bits>> ->
			{ignore, Rest};
		_ ->
			{more, ignore, Len - byte_size(Data)}
	end.

-spec parse_unidi_stream_header(binary())
	-> {ok, control | push | encoder | decoder | {webtransport, stream_id()}, binary()}
	| {undefined, binary()}
	| more.

parse_unidi_stream_header(<<0, Rest/bits>>) ->
	{ok, control, Rest};
parse_unidi_stream_header(<<1, Rest/bits>>) ->
	{ok, push, Rest};
parse_unidi_stream_header(<<2, Rest/bits>>) ->
	{ok, encoder, Rest};
parse_unidi_stream_header(<<3, Rest/bits>>) ->
	{ok, decoder, Rest};
%% WebTransport unidi streams.
parse_unidi_stream_header(<<1:2, 16#54:14, 0:2, SessionID:6, Rest/bits>>) ->
	{ok, {webtransport, SessionID}, Rest};
parse_unidi_stream_header(<<1:2, 16#54:14, 1:2, SessionID:14, Rest/bits>>) ->
	{ok, {webtransport, SessionID}, Rest};
parse_unidi_stream_header(<<1:2, 16#54:14, 2:2, SessionID:30, Rest/bits>>) ->
	{ok, {webtransport, SessionID}, Rest};
parse_unidi_stream_header(<<1:2, 16#54:14, 3:2, SessionID:62, Rest/bits>>) ->
	{ok, {webtransport, SessionID}, Rest};
parse_unidi_stream_header(<<1:2, 16#54:14, _/bits>>) ->
	more;
%% Unknown unidi streams.
parse_unidi_stream_header(<<0:2, _:6, Rest/bits>>) ->
	{undefined, Rest};
parse_unidi_stream_header(<<1:2, _:14, Rest/bits>>) ->
	{undefined, Rest};
parse_unidi_stream_header(<<2:2, _:30, Rest/bits>>) ->
	{undefined, Rest};
parse_unidi_stream_header(<<3:2, _:62, Rest/bits>>) ->
	{undefined, Rest}.

-spec parse_datagram(binary()) -> {stream_id(), binary()}.

parse_datagram(Data) ->
	{QuarterID, Rest} = parse_int(Data),
	SessionID = QuarterID * 4,
	{SessionID, Rest}.

-spec code_to_error(non_neg_integer()) -> error().

code_to_error(16#0100) -> h3_no_error;
code_to_error(16#0101) -> h3_general_protocol_error;
code_to_error(16#0102) -> h3_internal_error;
code_to_error(16#0103) -> h3_stream_creation_error;
code_to_error(16#0104) -> h3_closed_critical_stream;
code_to_error(16#0105) -> h3_frame_unexpected;
code_to_error(16#0106) -> h3_frame_error;
code_to_error(16#0107) -> h3_excessive_load;
code_to_error(16#0108) -> h3_id_error;
code_to_error(16#0109) -> h3_settings_error;
code_to_error(16#010a) -> h3_missing_settings;
code_to_error(16#010b) -> h3_request_rejected;
code_to_error(16#010c) -> h3_request_cancelled;
code_to_error(16#010d) -> h3_request_incomplete;
code_to_error(16#010e) -> h3_message_error;
code_to_error(16#010f) -> h3_connect_error;
code_to_error(16#0110) -> h3_version_fallback;
%% Extensions.
code_to_error(16#33) -> h3_datagram_error;
code_to_error(16#3994bd84) -> wt_buffered_stream_rejected;
code_to_error(16#170d7b68) -> wt_session_gone;
code_to_error(Code) when Code >= 16#52e4a40fa8db, Code =< 16#52e5ac983162 ->
	case (Code - 16#21) rem 16#1f of
		0 -> h3_no_error;
		_ ->
			%% @todo We need tests for this.
			Shifted = Code - 16#52e4a40fa8db,
			{wt_application_error,
				Shifted - Shifted div 16#1f}
	end;
%% Unknown/reserved error codes must be treated
%% as equivalent to H3_NO_ERROR.
code_to_error(_) -> h3_no_error.

-spec parse_int(binary()) -> {non_neg_integer(), binary()} | more.

parse_int(<<0:2, Int:6, Rest/bits>>) ->
	{Int, Rest};
parse_int(<<1:2, Int:14, Rest/bits>>) ->
	{Int, Rest};
parse_int(<<2:2, Int:30, Rest/bits>>) ->
	{Int, Rest};
parse_int(<<3:2, Int:62, Rest/bits>>) ->
	{Int, Rest};
parse_int(_) ->
	more.

%% Building.

-spec data(iodata()) -> iolist().

data(Data) ->
	Len = encode_int(iolist_size(Data)),
	[<<0:8>>, Len, Data].

-spec headers(iodata()) -> iolist().

headers(HeaderBlock) ->
	Len = encode_int(iolist_size(HeaderBlock)),
	[<<1:8>>, Len, HeaderBlock].

-spec settings(settings()) -> iolist().

settings(Settings) when Settings =:= #{} ->
	<<4:8, 0:8>>;
settings(Settings) ->
	Payload = settings_payload(Settings),
	Len = encode_int(iolist_size(Payload)),
	[<<4:8>>, Len, Payload].

settings_payload(Settings) ->
	Payload = [case Key of
		%% SETTINGS_QPACK_MAX_TABLE_CAPACITY (RFC9204).
		qpack_max_table_capacity when Value =:= 0 -> <<>>;
		qpack_max_table_capacity -> [encode_int(1), encode_int(Value)];
		%% SETTINGS_MAX_FIELD_SECTION_SIZE (RFC9114).
		max_header_list_size when Value =:= infinity -> <<>>;
		max_header_list_size -> [encode_int(6), encode_int(Value)];
		%% SETTINGS_QPACK_BLOCKED_STREAMS (RFC9204).
		qpack_blocked_streams when Value =:= 0 -> <<>>;
		qpack_blocked_streams -> [encode_int(1), encode_int(Value)];
		%% SETTINGS_ENABLE_CONNECT_PROTOCOL (RFC9220).
		enable_connect_protocol when Value -> [encode_int(8), encode_int(1)];
		enable_connect_protocol -> [encode_int(8), encode_int(0)];
		%% SETTINGS_H3_DATAGRAM (RFC9297).
		h3_datagram when Value -> [encode_int(16#33), encode_int(1)];
		h3_datagram -> [encode_int(16#33), encode_int(0)];
		%% SETTINGS_ENABLE_WEBTRANSPORT (draft-ietf-webtrans-http3-02, for compatibility).
		enable_webtransport when Value -> [encode_int(16#2b603742), encode_int(1)];
		enable_webtransport -> [encode_int(16#2b603742), encode_int(0)];
		%% SETTINGS_WT_MAX_SESSIONS (draft-ietf-webtrans-http3).
		wt_max_sessions when Value =:= 0 -> <<>>;
		wt_max_sessions -> [encode_int(16#c671706a), encode_int(Value)];
		%% SETTINGS_WT_INITIAL_MAX_STREAMS_UNI (draft-ietf-webtrans-http3).
		wt_initial_max_streams_uni when Value =:= 0 -> <<>>;
		wt_initial_max_streams_uni -> [encode_int(16#2b64), encode_int(Value)];
		%% SETTINGS_WT_INITIAL_MAX_STREAMS_BIDI (draft-ietf-webtrans-http3).
		wt_initial_max_streams_bidi when Value =:= 0 -> <<>>;
		wt_initial_max_streams_bidi -> [encode_int(16#2b65), encode_int(Value)];
		%% SETTINGS_WT_INITIAL_MAX_DATA (draft-ietf-webtrans-http3).
		wt_initial_max_data when Value =:= 0 -> <<>>;
		wt_initial_max_data -> [encode_int(16#2b61), encode_int(Value)]
	end || {Key, Value} <- maps:to_list(Settings)],
	%% Include one reserved identifier in addition.
	ReservedType = 16#1f * (rand:uniform(148764065110560900) - 1) + 16#21,
	[encode_int(ReservedType), encode_int(rand:uniform(15384) - 1)|Payload].

-spec webtransport_stream_header(stream_id(), unidi | bidi) -> iolist().

webtransport_stream_header(SessionID, StreamType) ->
	Signal = case StreamType of
		unidi -> 16#54;
		bidi -> 16#41
	end,
	[encode_int(Signal), encode_int(SessionID)].

-spec datagram(stream_id(), iodata()) -> iolist().

datagram(SessionID, Data) ->
	QuarterID = SessionID div 4,
	[encode_int(QuarterID), Data].

-spec error_to_code(error()) -> non_neg_integer().

error_to_code(h3_no_error) ->
	%% Implementations should select a reserved error code
	%% with some probability when they would have sent H3_NO_ERROR. (RFC9114 8.1)
	case rand:uniform(2) of
		1 -> 16#0100;
		2 -> 16#1f * (rand:uniform(148764065110560900) - 1) + 16#21
	end;
error_to_code(h3_general_protocol_error) -> 16#0101;
error_to_code(h3_internal_error) -> 16#0102;
error_to_code(h3_stream_creation_error) -> 16#0103;
error_to_code(h3_closed_critical_stream) -> 16#0104;
error_to_code(h3_frame_unexpected) -> 16#0105;
error_to_code(h3_frame_error) -> 16#0106;
error_to_code(h3_excessive_load) -> 16#0107;
error_to_code(h3_id_error) -> 16#0108;
error_to_code(h3_settings_error) -> 16#0109;
error_to_code(h3_missing_settings) -> 16#010a;
error_to_code(h3_request_rejected) -> 16#010b;
error_to_code(h3_request_cancelled) -> 16#010c;
error_to_code(h3_request_incomplete) -> 16#010d;
error_to_code(h3_message_error) -> 16#010e;
error_to_code(h3_connect_error) -> 16#010f;
error_to_code(h3_version_fallback) -> 16#0110;
%% Extensions.
error_to_code(h3_datagram_error) -> 16#33;
error_to_code(wt_buffered_stream_rejected) -> 16#3994bd84;
error_to_code(wt_session_gone) -> 16#170d7b68;
error_to_code({wt_application_error, AppErrorCode}) ->
	16#52e4a40fa8db + AppErrorCode + AppErrorCode div 16#1e.

-spec encode_int(h3_non_neg_integer()) -> binary().

encode_int(I) when I < 64 ->
	<<0:2, I:6>>;
encode_int(I) when I < 16384 ->
	<<1:2, I:14>>;
encode_int(I) when I < 1073741824 ->
	<<2:2, I:30>>;
encode_int(I) when I < 4611686018427387904 ->
	<<3:2, I:62>>.
