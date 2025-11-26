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

-module(cow_http3_machine).

-export([init/2]).
-export([init_unidi_local_streams/4]).
-export([init_unidi_stream/3]).
-export([set_unidi_remote_stream_type/3]).
-export([init_bidi_stream/2]).
-export([init_bidi_stream/3]).
-export([become_webtransport_session/2]).
-export([become_webtransport_stream/3]).
-export([close_webtransport_session/2]).
-export([close_bidi_stream_for_sending/2]).
-export([close_stream/2]).
-export([unidi_data/4]).
-export([frame/4]).
-export([ignored_frame/2]).
-export([prepare_headers/5]).
-export([prepare_trailers/3]).
-export([reset_stream/2]).
-export([get_bidi_stream_local_state/2]).
-export([get_bidi_stream_remote_state/2]).

-type opts() :: #{
	enable_connect_protocol => boolean(),
	max_decode_blocked_streams => 0..16#3fffffffffffffff,
	max_decode_table_size => 0..16#3fffffffffffffff,
	max_encode_blocked_streams => 0..16#3fffffffffffffff,
	max_encode_table_size => 0..16#3fffffffffffffff
}.
-export_type([opts/0]).

-type unidi_stream_dir() :: unidi_local | unidi_remote.
-type unidi_stream_type() :: control | push | encoder | decoder.

%% All stream types must have `id` as the first element
%% of the record as the more general functions require it there.

-record(unidi_stream, {
	id :: cow_http3:stream_id(),

	%% Unidi stream direction (local = we initiated).
	dir :: unidi_stream_dir(),

	%% Unidi stream type.
	type :: undefined | unidi_stream_type()
}).

-record(bidi_stream, {
	id :: cow_http3:stream_id(),

	%% Request method.
	method = undefined :: undefined | binary(),

	%% Whether we finished sending data.
	local = idle :: idle | cow_http:fin(),

	%% Whether we finished receiving data.
	remote = idle :: idle | cow_http:fin(),

	%% Size expected and read from the request body.
	remote_expected_size = undefined :: undefined | non_neg_integer(),
	remote_read_size = 0 :: non_neg_integer(),

	%% Unparsed te header. Used to know if we can send trailers.
	%% Note that we can always send trailers to the server.
	te :: undefined | binary()
}).

-record(wt_session, {
	id :: cow_http3:stream_id()
}).

-record(wt_stream, {
	id :: cow_http3:stream_id(),

	%% All WT streams belong to a single WT session.
	session_id :: cow_http3:stream_id(),

	%% Unidi stream direction (local = we initiated) or bidi.
	dir :: unidi_stream_dir() | bidi
}).

-type stream() :: #unidi_stream{} | #bidi_stream{} | #wt_session{} | #wt_stream{}.

-record(http3_machine, {
	%% Whether the HTTP/3 endpoint is a client or a server.
	mode :: client | server,

	%% Current state of the supported unidi streams:
	%% * the control stream must send SETTINGS as its first frame
	%% * none of these streams can be closed once they are open
	peer_control_state = no_stream :: no_stream | no_settings | ready,
	peer_decode_state = no_stream :: no_stream | ready,
	peer_encode_state = no_stream :: no_stream | ready,

	%% Maximum Push ID.
	max_push_id = -1 :: -1 | cow_http3:push_id(),

	%% Settings are separate for each endpoint. They are sent once
	%% at the beginning of the control stream.
	local_settings = #{
%		enable_connect_protocol => false
%		max_decode_blocked_streams => 0,
%		max_decode_table_size => 0,
%		max_encode_blocked_streams => 0,
%		max_encode_table_size => 4096
	} :: map(),

	%% Currently active HTTP/3 streams. Streams may be initiated either
	%% by the client or by the server through PUSH_PROMISE frames.
	streams = #{} :: #{cow_http3:stream_id() => stream()},

	%% QPACK decoding and encoding state.
	decode_state :: cow_qpack:state(),
	encode_state :: cow_qpack:state()
}).

-opaque http3_machine() :: #http3_machine{}.
-export_type([http3_machine/0]).

-type instructions() :: undefined
	| {decoder_instructions | encoder_instructions, iodata()}.

-spec init(client | server, opts())
	-> {ok, iolist(), http3_machine()}.

init(Mode, Opts) ->
	Settings = init_settings(Opts),
	{ok, cow_http3:settings(Settings), #http3_machine{
		mode=Mode, local_settings=Settings,
		decode_state=init_decode_state(Opts),
		encode_state=init_encode_state(Opts)
	}}.

init_settings(Opts) ->
	S0 = setting_from_opt(#{}, Opts, max_decode_table_size,
		qpack_max_table_capacity, 0),
	S1 = setting_from_opt(S0, Opts, max_decode_blocked_streams,
		qpack_blocked_streams, 0),
	%% @todo max_field_section_size
	S2 = setting_from_opt(S1, Opts, enable_connect_protocol,
		enable_connect_protocol, false),
	S3 = setting_from_opt(S2, Opts, h3_datagram,
		h3_datagram, false),
	%% For compatibility with draft-02.
	S4 = setting_from_opt(S3, Opts, enable_webtransport,
		enable_webtransport, false),
	setting_from_opt(S4, Opts, wt_max_sessions,
		wt_max_sessions, 0).

setting_from_opt(Settings, Opts, OptName, SettingName, Default) ->
	case maps:get(OptName, Opts, Default) of
		Default -> Settings;
		Value -> Settings#{SettingName => Value}
	end.

%% Note that only the decoder sends them as SETTINGS.
init_decode_state(Opts) ->
	MaxTableCapacity = maps:get(max_decode_table_size, Opts, 0),
	MaxBlockedStreams = maps:get(max_decode_blocked_streams, Opts, 0),
	cow_qpack:init(decoder, MaxTableCapacity, MaxBlockedStreams).

%% We want to use the dynamic table by default to improve
%% compression ratio, but we do not allow blocked streams
%% by default because they could lead to the latency being
%% worse than otherwise.
init_encode_state(Opts) ->
	MaxTableCapacity = maps:get(max_encode_table_size, Opts, 4096),
	MaxBlockedStreams = maps:get(max_encode_blocked_streams, Opts, 0),
	cow_qpack:init(encoder, MaxTableCapacity, MaxBlockedStreams).

-spec init_unidi_local_streams(cow_http3:stream_id(), cow_http3:stream_id(),
	cow_http3:stream_id(), State) -> State when State::http3_machine().

init_unidi_local_streams(ControlID, EncoderID, DecoderID,
		State=#http3_machine{streams=Streams}) ->
	State#http3_machine{
		streams=Streams#{
			ControlID => #unidi_stream{id=ControlID, dir=unidi_local, type=control},
			EncoderID => #unidi_stream{id=EncoderID, dir=unidi_local, type=encoder},
			DecoderID => #unidi_stream{id=DecoderID, dir=unidi_local, type=decoder}
	}}.

-spec init_unidi_stream(cow_http3:stream_id(), unidi_stream_dir(), State)
	-> State when State::http3_machine().

init_unidi_stream(StreamID, StreamDir, State=#http3_machine{streams=Streams}) ->
	State#http3_machine{streams=Streams#{StreamID => #unidi_stream{
		id=StreamID, dir=StreamDir, type=undefined}}}.

-spec set_unidi_remote_stream_type(cow_http3:stream_id(), unidi_stream_type(), State)
	-> {ok, State}
	| {error, {connection_error, h3_stream_creation_error, atom()}, State}
	when State::http3_machine().

set_unidi_remote_stream_type(StreamID, Type=control,
		State=#http3_machine{peer_control_state=no_stream}) ->
	Stream = stream_get(StreamID, State),
	{ok, stream_store(Stream#unidi_stream{type=Type},
		State#http3_machine{peer_control_state=no_settings})};
set_unidi_remote_stream_type(_, control, State) ->
	{error, {connection_error, h3_stream_creation_error,
		'A peer cannot open two control streams. (RFC9114 6.2.1)'},
		State};
set_unidi_remote_stream_type(StreamID, Type=decoder,
		State=#http3_machine{peer_decode_state=no_stream}) ->
	Stream = stream_get(StreamID, State),
	{ok, stream_store(Stream#unidi_stream{type=Type},
		State#http3_machine{peer_decode_state=ready})};
set_unidi_remote_stream_type(StreamID, Type=encoder,
		State=#http3_machine{peer_encode_state=no_stream}) ->
	Stream = stream_get(StreamID, State),
	{ok, stream_store(Stream#unidi_stream{type=Type},
		State#http3_machine{peer_encode_state=ready})};
set_unidi_remote_stream_type(_, decoder, State) ->
	{error, {connection_error, h3_stream_creation_error,
		'A peer cannot open two decoder streams. (RFC9204 4.2)'},
		State};
set_unidi_remote_stream_type(_, encoder, State) ->
	{error, {connection_error, h3_stream_creation_error,
		'A peer cannot open two encoder streams. (RFC9204 4.2)'},
		State}.

%% All bidi streams are request/response.
%% We only need to know the method when in client mode.

-spec init_bidi_stream(cow_http3:stream_id(), State)
	-> State when State::http3_machine().

init_bidi_stream(StreamID, State=#http3_machine{streams=Streams}) ->
	State#http3_machine{streams=Streams#{
		StreamID => #bidi_stream{id=StreamID}
	}}.

-spec init_bidi_stream(cow_http3:stream_id(), binary(), State)
	-> State when State::http3_machine().

init_bidi_stream(StreamID, Method, State=#http3_machine{streams=Streams}) ->
	State#http3_machine{streams=Streams#{
		StreamID => #bidi_stream{id=StreamID, method=Method}
	}}.

-spec become_webtransport_session(cow_http3:stream_id(), State)
	-> State when State::http3_machine().

become_webtransport_session(StreamID, State=#http3_machine{streams=Streams}) ->
	#{StreamID := #bidi_stream{}} = Streams,
	stream_store(#wt_session{id=StreamID}, State).

-spec become_webtransport_stream(cow_http3:stream_id(), cow_http3:stream_id(), State)
	-> {ok, State} when State::http3_machine().

become_webtransport_stream(StreamID, SessionID, State0) ->
	%% First we check whether SessionID really exists and is a WT session.
	case stream_get(SessionID, State0) of
		#wt_session{} ->
			%% The stream becomes a WT stream tied to SessionID.
			Dir = case stream_get(StreamID, State0) of
				#unidi_stream{dir=Dir0} -> Dir0;
				%% @todo The bidi stream must be in idle state.
				#bidi_stream{} -> bidi
			end,
			State = stream_store(#wt_stream{
				id=StreamID, session_id=SessionID, dir=Dir},
				State0),
			{ok, State}
		%% @todo Error conditions.
	end.

-spec close_webtransport_session(cow_http3:stream_id(), State)
	-> State when State::http3_machine().

close_webtransport_session(SessionID, State=#http3_machine{streams=Streams0}) ->
	#{SessionID := #wt_session{}} = Streams0,
	%% Remove all streams belonging to the session.
	Streams = maps:filtermap(fun
		(_, #wt_session{id=StreamID}) when StreamID =:= SessionID ->
			false;
		(_, #wt_stream{session_id=StreamID}) when StreamID =:= SessionID ->
			false;
		(_, _) ->
			true
	end, Streams0),
	State#http3_machine{streams=Streams}.

-spec close_bidi_stream_for_sending(cow_http3:stream_id(), State)
	-> State when State::http3_machine().

close_bidi_stream_for_sending(StreamID, State=#http3_machine{streams=Streams}) ->
	#{StreamID := Stream} = Streams,
	stream_store(Stream#bidi_stream{local=fin}, State).

-spec close_stream(cow_http3:stream_id(), State)
	-> {ok, State}
	| {error, {connection_error, h3_closed_critical_stream, atom()}, State}
	when State::http3_machine().

close_stream(StreamID, State=#http3_machine{streams=Streams0}) ->
	case maps:take(StreamID, Streams0) of
		{#unidi_stream{type=control}, Streams} ->
			{error, {connection_error, h3_closed_critical_stream,
				'A control stream was closed. (RFC9114 6.2.1)'},
				State#http3_machine{streams=Streams}};
		{#unidi_stream{type=decoder}, Streams} ->
			{error, {connection_error, h3_closed_critical_stream,
				'A decoder stream was closed. (RFC9204 4.2)'},
				State#http3_machine{streams=Streams}};
		{#unidi_stream{type=encoder}, Streams} ->
			{error, {connection_error, h3_closed_critical_stream,
				'An encoder stream was closed. (RFC9204 4.2)'},
				State#http3_machine{streams=Streams}};
		{_, Streams} ->
			{ok, State#http3_machine{streams=Streams}}
	end.

-spec unidi_data(binary(), cow_http:fin(), cow_http3:stream_id(), State)
	-> {ok, instructions(), State}
	| {error, {connection_error, cow_qpack:error(), atom()}, State}
	when State::http3_machine().

%% All currently supported unidi streams are critical.
unidi_data(_, fin, _, State) ->
	{error, {connection_error, h3_closed_critical_stream,
		'The FIN flag was set on an encoder or decoder stream. (RFC9204 4.2)'},
		State};
unidi_data(Data, nofin, StreamID, State=#http3_machine{
		decode_state=DecState0, encode_state=EncState0}) ->
	case stream_get(StreamID, State) of
		#unidi_stream{type=decoder} ->
			case cow_qpack:execute_decoder_instructions(Data, EncState0) of
				{ok, EncState} ->
					{ok, undefined, State#http3_machine{encode_state=EncState}};
				Error = {connection_error, _, _} ->
					{error, Error, State}
			end;
		#unidi_stream{type=encoder} ->
			case cow_qpack:execute_encoder_instructions(Data, DecState0) of
				{ok, <<>>, DecState} ->
					{ok, undefined, State#http3_machine{decode_state=DecState}};
				{ok, DecData, DecState} ->
					{ok, {decoder_instructions, DecData},
						State#http3_machine{decode_state=DecState}};
				Error = {connection_error, _, _} ->
					{error, Error, State}
			end
	end.

-spec frame(cow_http3:frame(), cow_http:fin(), cow_http3:stream_id(), State)
	-> {ok, State}
	| {ok, {data, binary()}, State}
	| {ok, {headers, cow_http:headers(), cow_http:pseudo_headers(),
		non_neg_integer() | undefined}, instructions(), State}
	| {ok, {trailers, cow_http:headers()}, instructions(), State}
	| {ok, {goaway, cow_http3:stream_id() | cow_http3:push_id()}, State}
	| {error, {stream_error, h3_message_error, atom()}, instructions(), State}
	| {error, {connection_error, cow_http3:error() | cow_qpack:error(), atom()}, State}
	when State::http3_machine().

frame(Frame, IsFin, StreamID, State) ->
	case element(1, Frame) of
		data -> data_frame(Frame, IsFin, StreamID, State);
		headers -> headers_frame(Frame, IsFin, StreamID, State);
		cancel_push -> cancel_push_frame(Frame, IsFin, StreamID, State);
		settings -> settings_frame(Frame, IsFin, StreamID, State);
		push_promise -> push_promise_frame(Frame, IsFin, StreamID, State);
		goaway -> goaway_frame(Frame, IsFin, StreamID, State);
		max_push_id -> max_push_id_frame(Frame, IsFin, StreamID, State)
	end.

%% DATA frame.

data_frame(Frame={data, Data}, IsFin, StreamID, State) ->
	DataLen = byte_size(Data),
	case stream_get(StreamID, State) of
		Stream = #bidi_stream{remote=nofin} ->
			data_frame(Frame, IsFin, Stream, State, DataLen);
		#bidi_stream{remote=idle} ->
			{error, {connection_error, h3_frame_unexpected,
				'DATA frame received before a HEADERS frame. (RFC9114 4.1)'},
				State};
		#bidi_stream{remote=fin} ->
			{error, {connection_error, h3_frame_unexpected,
				'DATA frame received after trailer HEADERS frame. (RFC9114 4.1)'},
				State};
		#unidi_stream{type=control} ->
			control_frame(Frame, State)
	end.

data_frame(Frame, IsFin, Stream0=#bidi_stream{remote_read_size=StreamRead}, State0, DataLen) ->
	Stream = Stream0#bidi_stream{remote=IsFin,
		remote_read_size=StreamRead + DataLen},
	State = stream_store(Stream, State0),
	case is_body_size_valid(Stream) of
		true ->
			{ok, Frame, State}%;
%% @todo Implement and update error type/message.
%		false ->
%			stream_reset(StreamID, State, protocol_error,
%				'The total size of DATA frames is different than the content-length. (RFC7540 8.1.2.6)')
	end.

%% It's always valid when no content-length header was specified.
is_body_size_valid(#bidi_stream{remote_expected_size=undefined}) ->
	true;
%% We didn't finish reading the body but the size is already larger than expected.
is_body_size_valid(#bidi_stream{remote=nofin, remote_expected_size=Expected,
		remote_read_size=Read}) when Read > Expected ->
	false;
is_body_size_valid(#bidi_stream{remote=nofin}) ->
	true;
is_body_size_valid(#bidi_stream{remote=fin, remote_expected_size=Expected,
		remote_read_size=Expected}) ->
	true;
%% We finished reading the body and the size read is not the one expected.
is_body_size_valid(_) ->
	false.

%% HEADERS frame.

headers_frame(Frame, IsFin, StreamID, State=#http3_machine{mode=Mode}) ->
	case stream_get(StreamID, State) of
		%% Headers.
		Stream=#bidi_stream{remote=idle} ->
			headers_decode(Frame, IsFin, Stream, State, case Mode of
				server -> request;
				client -> response
			end);
		%% Trailers.
		Stream=#bidi_stream{remote=nofin} ->
			headers_decode(Frame, IsFin, Stream, State, trailers);
		%% Additional frame received after trailers.
		#bidi_stream{remote=fin} ->
			{error, {connection_error, h3_frame_unexpected,
				'HEADERS frame received after trailer HEADERS frame. (RFC9114 4.1)'},
				State};
		#unidi_stream{type=control} ->
			control_frame(Frame, State)
	end.

headers_decode({headers, EncodedFieldSection}, IsFin, Stream=#bidi_stream{id=StreamID},
		State=#http3_machine{decode_state=DecodeState0}, Type) ->
	try cow_qpack:decode_field_section(EncodedFieldSection, StreamID, DecodeState0) of
		{ok, Headers, DecData, DecodeState} ->
			headers_process(Stream,
				State#http3_machine{decode_state=DecodeState}, IsFin, Type, DecData, Headers);
		Error = {connection_error, _, _} ->
			{error, Error, State}
	catch _:_ ->
		{error, {connection_error, qpack_decompression_failed,
			'Exception while trying to decode QPACK-encoded header block. (RFC9204 2.2)'},
			State}
	end.

headers_process(Stream=#bidi_stream{method=ReqMethod},
		State=#http3_machine{local_settings=LocalSettings},
		IsFin, Type, DecData, Headers0) ->
	case cow_http:process_headers(Headers0, Type, ReqMethod, IsFin, LocalSettings) of
		%% @todo If this is a WebTransport request we also need to check a few
		%% other things such as h3_datagram, max_sessions and QUIC's max_datagram_size options.
		%% @todo So cow_http3_machine needs to know about at least some QUIC options.
		{headers, Headers, PseudoHeaders, Len} ->
			headers_frame(Stream, State, IsFin, Type, DecData, Headers, PseudoHeaders, Len);
%		{push_promise, Headers, PseudoHeaders} -> %% @todo Implement push promises.
		{trailers, Headers} ->
			trailers_frame(Stream, State, DecData, Headers);
		{error, Reason} ->
			{error, {stream_error, h3_message_error, format_error(Reason)},
				%% We decoded the headers so must send the instructions if any.
				case DecData of
					<<>> -> undefined;
					_ -> {decoder_instructions, DecData}
				end,
				State}
	end.

headers_frame(Stream0, State0, IsFin, Type, DecData, Headers, PseudoHeaders, Len) ->
	Stream = case Type of
		request ->
			TE = case lists:keyfind(<<"te">>, 1, Headers) of
				{_, TE0} -> TE0;
				false -> undefined
			end,
			Stream0#bidi_stream{method=maps:get(method, PseudoHeaders),
				remote=IsFin, remote_expected_size=Len, te=TE};
		response ->
			case PseudoHeaders of
				#{status := Status} when Status >= 100, Status =< 199 -> Stream0;
				_ -> Stream0#bidi_stream{remote=IsFin, remote_expected_size=Len}
			end
	end,
	State = stream_store(Stream, State0),
	{ok, {headers, Headers, PseudoHeaders, Len},
		case DecData of
			<<>> -> undefined;
			_ -> {decoder_instructions, DecData}
		end,
		State}.

trailers_frame(Stream0, State0, DecData, Headers) ->
	Stream = Stream0#bidi_stream{remote=fin},
	State = stream_store(Stream, State0),
	%% @todo Error out if we didn't get the full body.
	case is_body_size_valid(Stream) of
		true ->
			{ok, {trailers, Headers},
				case DecData of
					<<>> -> undefined;
					_ -> {decoder_instructions, DecData}
				end,
				State}%;
%% @todo Implement and update error type/message.
%		false ->
%			stream_reset(StreamID, State, protocol_error,
%				'The total size of DATA frames is different than the content-length. (RFC7540 8.1.2.6)')
	end.

format_error(connect_invalid_pseudo_header) ->
	'CONNECT requests only use the :method and :authority pseudo-headers. (RFC9114 4.4)';
format_error(connect_missing_authority) ->
	'CONNECT requests must include the :authority pseudo-header. (RFC9114 4.4)';
format_error(empty_header_name) ->
	'Empty header names are not valid regular headers. (CVE-2019-9516)';
format_error(extended_connect_missing_protocol) ->
	'Extended CONNECT requests must include the :protocol pseudo-header. (RFC9220, RFC8441 4)';
format_error(invalid_connection_header) ->
	'The connection header is not allowed. (RFC9114 4.2)';
format_error(invalid_keep_alive_header) ->
	'The keep-alive header is not allowed. (RFC9114 4.2)';
format_error(invalid_protocol_pseudo_header) ->
	'The :protocol pseudo-header is only defined for the extended CONNECT. (RFC9220, RFC8441 4)';
format_error(invalid_proxy_authenticate_header) ->
	'The proxy-authenticate header is not allowed. (RFC9114 4.2)';
format_error(invalid_proxy_authorization_header) ->
	'The proxy-authorization header is not allowed. (RFC9114 4.2)';
format_error(invalid_pseudo_header) ->
	'An unknown or invalid pseudo-header was found. (RFC9114 4.3)';
format_error(invalid_status_pseudo_header) ->
	'The :status pseudo-header value is invalid. (RFC9114 4.3, RFC9114 4.3.2)';
format_error(invalid_te_header) ->
	'The te header is only allowed in request headers. (RFC9114 4.2)';
format_error(invalid_te_value) ->
	'The te header with a value other than "trailers" is not allowed. (RFC9114 4.2)';
format_error(invalid_transfer_encoding_header) ->
	'The transfer-encoding header is not allowed. (RFC9114 4.1)';
format_error(invalid_upgrade_header) ->
	'The upgrade header is not allowed. (RFC9114 4.2)';
format_error(missing_pseudo_header) ->
	'A required pseudo-header was not found. (RFC9114 4.3.1, RFC9114 4.3.2)';
format_error(multiple_authority_pseudo_headers) ->
	'Multiple :authority pseudo-headers were found. (RFC9114 4.3.1)';
format_error(multiple_method_pseudo_headers) ->
	'Multiple :method pseudo-headers were found. (RFC9114 4.3.1)';
format_error(multiple_path_pseudo_headers) ->
	'Multiple :path pseudo-headers were found. (RFC9114 4.3.1)';
format_error(multiple_protocol_pseudo_headers) ->
	'Multiple :protocol pseudo-headers were found. (RFC9114 4.3.1)';
format_error(multiple_scheme_pseudo_headers) ->
	'Multiple :scheme pseudo-headers were found. (RFC9114 4.3.1)';
format_error(multiple_status_pseudo_headers) ->
	'Multiple :status pseudo-headers were found. (RFC9114 4.3.2)';
format_error(non_zero_length_with_fin_flag) ->
	'HEADERS frame with the FIN flag contains a non-zero content-length. (RFC9114 4.1.2)';
format_error(pseudo_header_after_regular) ->
	'Pseudo-headers were found after regular headers. (RFC9114 4.3)';
format_error(trailer_invalid_pseudo_header) ->
	'Trailer header blocks must not contain pseudo-headers. (RFC9114 4.3)';
format_error(uppercase_header_name) ->
	'Header names must be lowercase. (RFC9114 4.1.2, RFC9114 4.2)';
format_error(Reason) ->
	cow_http:format_semantic_error(Reason).

cancel_push_frame(Frame, _IsFin, StreamID, State) ->
	case stream_get(StreamID, State) of
		#unidi_stream{type=control} ->
			control_frame(Frame, State)
	end.

settings_frame(Frame, _IsFin, StreamID, State) ->
	case stream_get(StreamID, State) of
		#unidi_stream{type=control} ->
			control_frame(Frame, State);
		#bidi_stream{} ->
			{error, {connection_error, h3_frame_unexpected,
				'The SETTINGS frame is not allowed on a bidi stream. (RFC9114 7.2.4)'},
				State}
	end.

push_promise_frame(Frame, _IsFin, StreamID, State) ->
	case stream_get(StreamID, State) of
		#unidi_stream{type=control} ->
			control_frame(Frame, State)
	end.

goaway_frame(Frame, _IsFin, StreamID, State) ->
	case stream_get(StreamID, State) of
		#unidi_stream{type=control} ->
			control_frame(Frame, State);
		#bidi_stream{} ->
			{error, {connection_error, h3_frame_unexpected,
				'The GOAWAY frame is not allowed on a bidi stream. (RFC9114 7.2.6)'},
				State}
	end.

max_push_id_frame(Frame, _IsFin, StreamID, State) ->
	case stream_get(StreamID, State) of
		#unidi_stream{type=control} ->
			control_frame(Frame, State);
		#bidi_stream{} ->
			{error, {connection_error, h3_frame_unexpected,
				'The MAX_PUSH_ID frame is not allowed on a bidi stream. (RFC9114 7.2.7)'},
				State}
	end.

control_frame({settings, Settings}, State=#http3_machine{
		peer_control_state=no_settings, encode_state=EncState0}) ->
	%% @todo max_field_section_size
	%% Send the QPACK values to the encoder.
	MaxTableCapacity = maps:get(qpack_max_table_capacity, Settings, 0),
	MaxBlockedStreams = maps:get(qpack_blocked_streams, Settings, 0),
	EncState = cow_qpack:encoder_set_settings(MaxTableCapacity, MaxBlockedStreams, EncState0),
	{ok, State#http3_machine{peer_control_state=ready, encode_state=EncState}};
control_frame({settings, _}, State) ->
	{error, {connection_error, h3_frame_unexpected,
		'The SETTINGS frame cannot be sent more than once. (RFC9114 7.2.4)'},
		State};
control_frame(_Frame, State=#http3_machine{peer_control_state=no_settings}) ->
	{error, {connection_error, h3_missing_settings,
		'The first frame on the control stream must be a SETTINGS frame. (RFC9114 6.2.1)'},
		State};
control_frame(Frame = {goaway, _}, State) ->
	{ok, Frame, State};
%% @todo Implement server push.
control_frame({max_push_id, PushID}, State=#http3_machine{max_push_id=MaxPushID}) ->
	if
		PushID >= MaxPushID ->
			{ok, State#http3_machine{max_push_id=PushID}};
		true ->
			{error, {connection_error, h3_id_error,
				'MAX_PUSH_ID must not be lower than previously received. (RFC9114 7.2.7)'},
				State}
	end;
control_frame(ignored_frame, State) ->
	{ok, State};
control_frame(_Frame, State) ->
	{error, {connection_error, h3_frame_unexpected,
		'DATA and HEADERS frames are not allowed on the control stream. (RFC9114 7.2.1, RFC9114 7.2.2)'},
		State}.

%% Ignored frames.

-spec ignored_frame(cow_http3:stream_id(), State)
	-> {ok, State}
	| {error, {connection_error, cow_http3:error(), atom()}, State}
	when State::http3_machine().

ignored_frame(StreamID, State) ->
	case stream_get(StreamID, State) of
		#unidi_stream{type=control} ->
			control_frame(ignored_frame, State);
		_ ->
			{ok, State}
	end.

%% Functions for sending a message header or body. Note that
%% this module does not send data directly, instead it returns
%% a value that can then be used to send the frames.

-spec prepare_headers(cow_http3:stream_id(), State,
	idle | cow_http:fin(), cow_http:pseudo_headers(), cow_http:headers())
	-> {ok, cow_http:fin(), iodata(), instructions(), State} when State::http3_machine().

prepare_headers(StreamID, State=#http3_machine{encode_state=EncodeState0},
		IsFin0, PseudoHeaders, Headers0) ->
	Stream = #bidi_stream{method=Method, local=idle} = stream_get(StreamID, State),
	IsFin = case {IsFin0, Method} of
		{idle, _} -> nofin;
		{_, <<"HEAD">>} -> fin;
		_ -> IsFin0
	end,
	%% With QUIC we don't have a data queue so the local state
	%% can be updated immediately.
	LocalIsFin = case IsFin0 of
		idle -> idle;
		_ -> IsFin
	end,
	Headers = cow_http:merge_pseudo_headers(PseudoHeaders,
		cow_http:remove_http1_headers(Headers0)),
	{ok, HeaderBlock, EncData, EncodeState}
		= cow_qpack:encode_field_section(Headers, StreamID, EncodeState0),
	{ok, IsFin, HeaderBlock,
		case EncData of
			[] -> undefined;
			_ -> {encoder_instructions, EncData}
		end,
		stream_store(Stream#bidi_stream{local=LocalIsFin},
			State#http3_machine{encode_state=EncodeState})}.

-spec prepare_trailers(cow_http3:stream_id(), State, cow_http:headers())
	-> {trailers, iodata(), instructions(), State}
	| {no_trailers, State}
	when State::http3_machine().

prepare_trailers(StreamID, State=#http3_machine{encode_state=EncodeState0}, Trailers) ->
	Stream = #bidi_stream{local=nofin, te=TE0} = stream_get(StreamID, State),
	TE = try cow_http_hd:parse_te(TE0) of
		{trailers, []} -> trailers;
		_ -> no_trailers
	catch _:_ ->
		%% If we can't parse the TE header, assume we can't send trailers.
		no_trailers
	end,
	case TE of
		trailers ->
			{ok, HeaderBlock, EncData, EncodeState}
				= cow_qpack:encode_field_section(Trailers, StreamID, EncodeState0),
			{trailers, HeaderBlock,
				case EncData of
					[] -> undefined;
					_ -> {encoder_instructions, EncData}
				end,
				stream_store(Stream#bidi_stream{local=fin},
					State#http3_machine{encode_state=EncodeState})};
		no_trailers ->
			{no_trailers, stream_store(Stream#bidi_stream{local=fin}, State)}
	end.

%% Public interface to reset streams.

-spec reset_stream(cow_http3:stream_id(), State)
	-> {ok, State} | {error, not_found} when State::http3_machine().

reset_stream(StreamID, State=#http3_machine{streams=Streams0}) ->
	case maps:take(StreamID, Streams0) of
		{_, Streams} ->
			{ok, State#http3_machine{streams=Streams}};
		error ->
			{error, not_found}
	end.

%% Retrieve the local state for a bidi stream.

-spec get_bidi_stream_local_state(cow_http3:stream_id(), http3_machine())
	-> {ok, idle | cow_http:fin()} | {error, not_found}.

get_bidi_stream_local_state(StreamID, State) ->
	case stream_get(StreamID, State) of
		#bidi_stream{local=IsFin} ->
			{ok, IsFin};
		%% Stream may never have been opened, or could have
		%% already been closed.
		undefined ->
			{error, not_found}
	end.

%% Retrieve the remote state for a bidi stream.

-spec get_bidi_stream_remote_state(cow_http3:stream_id(), http3_machine())
	-> {ok, idle | cow_http:fin()} | {error, not_found}.

get_bidi_stream_remote_state(StreamID, State) ->
	case stream_get(StreamID, State) of
		#bidi_stream{remote=IsFin} ->
			{ok, IsFin};
		%% Stream may never have been opened, or could have
		%% already been closed.
		undefined ->
			{error, not_found}
	end.

%% Stream-related functions.

stream_get(StreamID, #http3_machine{streams=Streams}) ->
	maps:get(StreamID, Streams, undefined).

stream_store(Stream, State=#http3_machine{streams=Streams}) ->
	StreamID = element(2, Stream),
	State#http3_machine{streams=Streams#{StreamID => Stream}}.
