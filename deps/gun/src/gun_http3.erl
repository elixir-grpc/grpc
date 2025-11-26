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

%% @todo Tunneling has not been implemented for HTTP/3.
-module(gun_http3).

-export([check_options/1]).
-export([name/0]).
-export([opts_name/0]).
-export([has_keepalive/0]).
-export([default_keepalive/0]).
-export([init/4]).
-export([switch_transport/3]).
-export([handle/5]).
-export([handle_continue/6]).
-export([update_flow/4]).
-export([closing/4]).
-export([close/4]).
-export([keepalive/3]).
-export([ping/4]).
-export([headers/12]).
-export([request/13]).
-export([data/7]).
-export([connect/10]).
-export([cancel/5]).
-export([timeout/3]).
-export([stream_info/2]).
-export([down/1]).
-export([ws_upgrade/11]).
-export([ws_send/6]).

-record(stream, {
	%% Stream ID.
	id :: non_neg_integer(),

	%% Reference used by the user of Gun to refer to this stream.
	%% This may be only a part of a stream_ref() for tunneled streams.
	%% For unidirectional streams this is a dummy that is never sent to the user.
	ref :: reference(),

	%% Process to send messages to.
	reply_to :: undefined | gun:reply_to(),

	%% Whether the stream is currently in a special state.
	status :: header | {unidi, control | encoder | decoder}
		| normal | {data, non_neg_integer()} | discard,

	%% Stream buffer.
	buffer = <<>> :: binary(),

	%% Request target URI (request stream only).
	authority :: undefined | iodata(),
	path :: undefined | iodata(),

	%% Content handlers state.
	handler_state :: undefined | gun_content_handler:state()
}).

-record(http3_state, {
	reply_to :: gun:reply_to(),
	conn :: gun_quicer:quicer_connection_handle(),
	transport :: module(),
	opts = #{} :: gun:http2_opts(),
	content_handlers :: gun_content_handler:opt(),

	%% HTTP/3 state machine.
	http3_machine :: cow_http3_machine:http3_machine(),

	%% Specially handled local unidi streams.
	%% @todo Maybe move the control stream to streams map.
	local_control_id = undefined :: undefined | cow_http3:stream_id(),
	local_encoder_id = undefined :: undefined | cow_http3:stream_id(),
	local_decoder_id = undefined :: undefined | cow_http3:stream_id(),

	%% Bidirectional streams used for requests and responses,
	%% as well as unidirectional streams initiated by the server.
	%%
	%% Streams can be found by Ref or by StreamID. The most
	%% common should be the StreamID so we use it as key. We also have
	%% a Ref->StreamID index for faster lookup when we only have the Ref.
	streams = #{} :: #{reference() => #stream{}},
	stream_refs = #{} :: #{reference() => reference()}
}).

check_options(_) ->
	ok. %% @todo

name() -> http3.
opts_name() -> http3_opts.
has_keepalive() -> true.
default_keepalive() -> infinity.

init(ReplyTo, Conn, Transport, Opts) ->
	Handlers = maps:get(content_handlers, Opts, [gun_data_h]),
	{ok, SettingsBin, HTTP3Machine0} = cow_http3_machine:init(client, Opts),
	%% @todo We may get a TLS 1.3 error/alert here in mTLS scenarios.
	{ok, ControlID} = Transport:start_unidi_stream(Conn, [<<0>>, SettingsBin]),
	{ok, EncoderID} = Transport:start_unidi_stream(Conn, [<<2>>]),
	{ok, DecoderID} = Transport:start_unidi_stream(Conn, [<<3>>]),
	%% Set the control, encoder and decoder streams in the machine.
	HTTP3Machine = cow_http3_machine:init_unidi_local_streams(
		ControlID, EncoderID, DecoderID, HTTP3Machine0),
	{ok, connected, #http3_state{reply_to=ReplyTo, conn=Conn, transport=Transport,
		opts=Opts, content_handlers=Handlers, http3_machine=HTTP3Machine,
		local_control_id=ControlID, local_encoder_id=EncoderID,
		local_decoder_id=DecoderID}}.

-spec switch_transport(_, _, _) -> no_return().

switch_transport(_Transport, _Conn, _State) ->
	error(unimplemented).

handle(Msg, State0=#http3_state{transport=Transport},
		CookieStore, EvHandler, EvHandlerState) ->
	case Transport:handle(Msg) of
		{data, StreamID, IsFin, Data} ->
			parse(Data, State0, StreamID, IsFin,
				CookieStore, EvHandler, EvHandlerState);
		{stream_started, StreamID, StreamType} ->
			State = stream_new_remote(State0, StreamID, StreamType),
			{{state, State}, CookieStore, EvHandlerState};
		{stream_closed, _StreamID, _ErrorCode} ->
			%% @todo Clean up the stream.
			{{state, State0}, CookieStore, EvHandlerState};
		{stream_peer_send_aborted, StreamID, ErrorCode} ->
			Reason = cow_http3:code_to_error(ErrorCode),
			{StateOrError, EvHandlerStateRet} = stream_aborted(
				State0, StreamID, Reason, EvHandler, EvHandlerState),
			{StateOrError, CookieStore, EvHandlerStateRet};
		closed ->
			%% @todo Terminate the connection.
			{{state, State0}, CookieStore, EvHandlerState};
		ok ->
			{{state, State0}, CookieStore, EvHandlerState};
		unknown ->
			%% @todo Log a warning.
			{{state, State0}, CookieStore, EvHandlerState}
	end.

parse(Data, State, StreamID, IsFin, CookieStore, EvHandler, EvHandlerState) ->
	case stream_get(State, StreamID) of
		Stream=#stream{buffer= <<>>} ->
			parse1(Data, State, Stream, IsFin,
				CookieStore, EvHandler, EvHandlerState);
		Stream=#stream{buffer=Buffer} ->
			%% @todo OK we should only keep the StreamRef forward
			%%	   and update the stream in the state here.
			Stream1 = Stream#stream{buffer= <<>>},
			parse1(<<Buffer/binary, Data/binary>>,
				stream_update(State, Stream1), Stream1, IsFin,
				CookieStore, EvHandler, EvHandlerState);
		%% Pending data for a stream that has been reset. Ignore.
		error ->
			{{state, State}, CookieStore, EvHandlerState}
	end.

parse1(Data, State, Stream=#stream{status=header}, IsFin,
		CookieStore, EvHandler, EvHandlerState) ->
	parse_unidirectional_stream_header(Data, State, Stream, IsFin,
		CookieStore, EvHandler, EvHandlerState);
parse1(Data, State0=#http3_state{http3_machine=HTTP3Machine0},
		#stream{status={unidi, Type}, id=StreamID}, IsFin,
		CookieStore, _EvHandler, EvHandlerState)
		when Type =:= encoder; Type =:= decoder ->
	case cow_http3_machine:unidi_data(Data, IsFin, StreamID, HTTP3Machine0) of
		{ok, Instrs, HTTP3Machine} ->
			State = send_instructions(State0#http3_state{http3_machine=HTTP3Machine}, Instrs),
			{{state, State}, CookieStore, EvHandlerState};
		{error, Error={connection_error, _, _}, HTTP3Machine} ->
			{connection_error(State0#http3_state{http3_machine=HTTP3Machine}, Error),
				CookieStore, EvHandlerState}
	end;
parse1(Data, State, Stream=#stream{status={data, Len}, id=StreamID}, IsFin,
		CookieStore, EvHandler, EvHandlerState) ->
	DataLen = byte_size(Data),
	if
		DataLen < Len ->
			%% We don't have the full frame but this is the end of the
			%% data we have. So FrameIsFin is equivalent to IsFin here.
			frame(State, Stream#stream{status={data, Len - DataLen}},
				{data, Data}, IsFin, CookieStore, EvHandler, EvHandlerState);
		true ->
			<<Data1:Len/binary, Rest/bits>> = Data,
			FrameIsFin = is_fin(IsFin, Rest),
			case frame(State, Stream#stream{status=normal}, {data, Data1}, FrameIsFin,
					CookieStore, EvHandler, EvHandlerState) of
				%% @todo {error, _}.
				{{state, State1}, CookieStore1, EvHandlerState1} ->
					parse(Rest, State1, StreamID, IsFin,
						CookieStore1, EvHandler, EvHandlerState1)
			end
	end;
%% @todo Clause that discards receiving data for aborted streams.
parse1(Data, State, Stream=#stream{id=StreamID}, IsFin,
		CookieStore, EvHandler, EvHandlerState) ->
	case cow_http3:parse(Data) of
		{ok, Frame, Rest} ->
			FrameIsFin = is_fin(IsFin, Rest),
			case frame(State, Stream, Frame, FrameIsFin,
					CookieStore, EvHandler, EvHandlerState) of
				%% @todo {error, _}.
				{{state, State1}, CookieStore1, EvHandlerState1} ->
					parse(Rest, State1, StreamID, IsFin,
						CookieStore1, EvHandler, EvHandlerState1)
			end;
		{more, Frame = {data, _}, Len} ->
			%% We're at the end of the data so FrameIsFin is equivalent to IsFin.
			case IsFin of
				nofin ->
					frame(State, Stream#stream{status={data, Len}}, Frame, nofin,
						CookieStore, EvHandler, EvHandlerState);
				fin ->
					{connection_error(State, {connection_error, h3_frame_error,
						'Last frame on stream was truncated. (RFC9114 7.1)'}),
						CookieStore, EvHandlerState}
			end;
		%% @todo {more, ignore, Len}
		{ignore, Rest} ->
			case ignored_frame(State, Stream) of
				{state, State1} ->
					parse(Rest, State1, StreamID, IsFin,
						CookieStore, EvHandler, EvHandlerState)
			end;
		Error = {connection_error, _, _} ->
			{connection_error(State, Error), CookieStore, EvHandlerState};
		more when Data =:= <<>> ->
			{{state, stream_update(State, Stream#stream{buffer=Data})},
				CookieStore, EvHandlerState};
		more ->
			%% We're at the end of the data so FrameIsFin is equivalent to IsFin.
			case IsFin of
				nofin ->
					{{state, stream_update(State, Stream#stream{buffer=Data})},
						CookieStore, EvHandlerState};
				fin ->
					{connection_error(State, {connection_error, h3_frame_error,
						'Last frame on stream was truncated. (RFC9114 7.1)'}),
						CookieStore, EvHandlerState}
			end
	end.

%% We may receive multiple frames in a single QUIC packet.
%% The FIN flag applies to the QUIC packet, not to the frame.
%% We must therefore only consider the frame to have a FIN
%% flag if there's no data remaining to be read.
is_fin(fin, <<>>) -> fin;
is_fin(_, _) -> nofin.

parse_unidirectional_stream_header(Data, State0=#http3_state{http3_machine=HTTP3Machine0},
		Stream0=#stream{id=StreamID}, IsFin, CookieStore, EvHandler, EvHandlerState) ->
	case cow_http3:parse_unidi_stream_header(Data) of
		{ok, Type, Rest} when Type =:= control; Type =:= encoder; Type =:= decoder ->
			case cow_http3_machine:set_unidi_remote_stream_type(
					StreamID, Type, HTTP3Machine0) of
				{ok, HTTP3Machine} ->
					State = State0#http3_state{http3_machine=HTTP3Machine},
					Stream = Stream0#stream{status={unidi, Type}},
					parse(Rest, stream_update(State, Stream), StreamID, IsFin,
						CookieStore, EvHandler, EvHandlerState);
				{error, Error={connection_error, _, _}, HTTP3Machine} ->
					{connection_error(State0#http3_state{http3_machine=HTTP3Machine}, Error),
						CookieStore, EvHandlerState}
			end;
%% @todo Implement server push.
%%       Note that we shouldn't receive this frame until we set MAX_PUSH_ID.
%		{ok, push, _} ->
%			{connection_error(State0, {connection_error, h3_stream_creation_error,
%				'Only servers can push. (RFC9114 6.2.2)'}),
%				CookieStore, EvHandlerState};
		%% Unknown stream types must be ignored. We choose to abort the
		%% stream instead of reading and discarding the incoming data.
		{undefined, _} ->
			{{state, (stream_abort_receive(State0, Stream0, h3_stream_creation_error))},
				CookieStore, EvHandlerState}
	end.

%% @todo Cookie/events.
frame(State=#http3_state{http3_machine=HTTP3Machine0},
		Stream=#stream{id=StreamID}, Frame, IsFin,
		CookieStore, EvHandler, EvHandlerState) ->
	case cow_http3_machine:frame(Frame, IsFin, StreamID, HTTP3Machine0) of
		{ok, HTTP3Machine} ->
			{{state, State#http3_state{http3_machine=HTTP3Machine}},
				CookieStore, EvHandlerState};
		{ok, {data, Data}, HTTP3Machine} ->
			data_frame(State#http3_state{http3_machine=HTTP3Machine}, Stream, IsFin, Data,
				CookieStore, EvHandler, EvHandlerState);
		{ok, {headers, Headers, PseudoHeaders, BodyLen}, Instrs, HTTP3Machine} ->
			headers_frame(
				send_instructions(State#http3_state{http3_machine=HTTP3Machine}, Instrs),
				Stream, IsFin, Headers, PseudoHeaders, BodyLen,
				CookieStore, EvHandler, EvHandlerState);
		{ok, {trailers, Trailers}, Instrs, HTTP3Machine} ->
			{StateOrError, EvHandlerStateRet} = trailers_frame(
				send_instructions(State#http3_state{http3_machine=HTTP3Machine}, Instrs),
				Stream, Trailers, EvHandler, EvHandlerState),
			{StateOrError, CookieStore, EvHandlerStateRet};
		{ok, GoAway={goaway, _}, HTTP3Machine} ->
			goaway(State#http3_state{http3_machine=HTTP3Machine}, GoAway);
		{error, Error={stream_error, _Reason, _Human}, Instrs, HTTP3Machine} ->
			State1 = send_instructions(State#http3_state{http3_machine=HTTP3Machine}, Instrs),
			reset_stream(State1, StreamID, Error);
		{error, Error={connection_error, _, _}, HTTP3Machine} ->
			{connection_error(State#http3_state{http3_machine=HTTP3Machine}, Error),
				CookieStore, EvHandlerState}
	end.

data_frame(State0, Stream, IsFin, Data, CookieStore0, EvHandler, EvHandlerState0) ->
	case Stream of
		#stream{} -> %tunnel=undefined} ->
			{StateOrError, EvHandlerState} = data_frame1(State0,
				Stream, IsFin, Data, EvHandler, EvHandlerState0),
			{StateOrError, CookieStore0, EvHandlerState}%;
%		Stream=#stream{tunnel=#tunnel{protocol=Proto, protocol_state=ProtoState0}} ->
%%			%% @todo What about IsFin?
%			{Commands, CookieStore, EvHandlerState1} = Proto:handle(Data,
%				ProtoState0, CookieStore0, EvHandler, EvHandlerState0),
%			%% The frame/parse functions only handle state or error commands.
%			{ResCommands, EvHandlerState} = tunnel_commands(Commands,
%				Stream, State0, EvHandler, EvHandlerState1),
%			{ResCommands, CookieStore, EvHandlerState}
	end.

data_frame1(State0, Stream=#stream{ref=StreamRef, reply_to=ReplyTo,
		%flow=Flow0,
		handler_state=Handlers0}, IsFin, Data, EvHandler, EvHandlerState0) ->
	{ok, _Dec, Handlers} = gun_content_handler:handle(IsFin, Data, Handlers0),
%	Flow = case Flow0 of
%		infinity -> infinity;
%		_ -> Flow0 - Dec
%	end,
	State1 = stream_update(State0, Stream#stream{%flow=Flow,
		handler_state=Handlers}),
	{StateOrError, EvHandlerState} = case IsFin of
		fin ->
			EvHandlerState1 = EvHandler:response_end(#{
				stream_ref => StreamRef, %% @todo stream_ref(State1, StreamRef),
				reply_to => ReplyTo
			}, EvHandlerState0),
			{{state, State1}, EvHandlerState1};
		nofin ->
			{{state, State1}, EvHandlerState0}
	end,
	case StateOrError of
		{state, State} ->
			%% We do not remove the stream immediately. We will only when
			%% the QUIC stream gets closed.
			{{state, State}, EvHandlerState}%;
%		Error={error, _} ->
%			%% @todo Delete stream and return new state and error commands.
%			{Error, EvHandlerState}
	end.

headers_frame(State0=#http3_state{opts=Opts}, Stream, IsFin, Headers,
		#{status := Status}, _BodyLen, CookieStore0, EvHandler, EvHandlerState0) ->
	#stream{
		authority=Authority,
		path=Path%,
%		tunnel=Tunnel
	} = Stream,
	CookieStore = gun_cookies:set_cookie_header(<<"https">>,
		Authority, Path, Status, Headers, CookieStore0, Opts),
	{StateOrError, EvHandlerState} = if
		Status >= 100, Status =< 199 ->
			headers_frame_inform(State0, Stream, Status, Headers, EvHandler, EvHandlerState0);
%		Status >= 200, Status =< 299, element(#tunnel.state, Tunnel) =:= requested, IsFin =:= nofin ->
%			headers_frame_connect(State0, Stream, Status, Headers, EvHandler, EvHandlerState0);
		true ->
			headers_frame_response(State0, Stream, IsFin, Status, Headers, EvHandler, EvHandlerState0)
	end,
	{StateOrError, CookieStore, EvHandlerState}.

headers_frame_inform(State, #stream{ref=StreamRef, reply_to=ReplyTo},
		Status, Headers, EvHandler, EvHandlerState0) ->
	RealStreamRef = StreamRef, %% @todo stream_ref(State, StreamRef),
	gun:reply(ReplyTo, {gun_inform, self(), RealStreamRef, Status, Headers}),
	EvHandlerState = EvHandler:response_inform(#{
		stream_ref => RealStreamRef,
		reply_to => ReplyTo,
		status => Status,
		headers => Headers
	}, EvHandlerState0),
	{{state, State}, EvHandlerState}.

headers_frame_response(State=#http3_state{content_handlers=Handlers0},
		Stream=#stream{ref=StreamRef, reply_to=ReplyTo},
		IsFin, Status, Headers, EvHandler, EvHandlerState0) ->
	RealStreamRef = StreamRef, %% @todo stream_ref(State, StreamRef),
	gun:reply(ReplyTo, {gun_response, self(), RealStreamRef, IsFin, Status, Headers}),
	EvHandlerState1 = EvHandler:response_headers(#{
		stream_ref => RealStreamRef,
		reply_to => ReplyTo,
		status => Status,
		headers => Headers
	}, EvHandlerState0),
	{Handlers, EvHandlerState} = case IsFin of
		fin ->
			EvHandlerState2 = EvHandler:response_end(#{
				stream_ref => RealStreamRef,
				reply_to => ReplyTo
			}, EvHandlerState1),
			{undefined, EvHandlerState2};
		nofin ->
			{gun_content_handler:init(ReplyTo, RealStreamRef,
				Status, Headers, Handlers0), EvHandlerState1}
	end,
	%% @todo Uncomment when tunnel is added.
	%% We disable the tunnel, if any, when receiving any non 2xx response.
	%%
	%% We do not remove the stream immediately. We will only when
	%% the QUIC stream gets closed.
	{{state, stream_update(State,
		Stream#stream{handler_state=Handlers})},%, tunnel=undefined})},
		EvHandlerState}.

trailers_frame(State, #stream{ref=StreamRef, reply_to=ReplyTo},
		Trailers, EvHandler, EvHandlerState0) ->
	%% @todo We probably want to pass this to gun_content_handler?
	RealStreamRef = StreamRef, %% @todo stream_ref(State, StreamRef),
	gun:reply(ReplyTo, {gun_trailers, self(), RealStreamRef, Trailers}),
	ResponseEvent = #{
		stream_ref => RealStreamRef,
		reply_to => ReplyTo
	},
	EvHandlerState1 = EvHandler:response_trailers(ResponseEvent#{headers => Trailers}, EvHandlerState0),
	EvHandlerState = EvHandler:response_end(ResponseEvent, EvHandlerState1),
	%% We do not remove the stream immediately. We will only when
	%% the QUIC stream gets closed.
	{{state, State}, EvHandlerState}.

-spec goaway(_, _) -> no_return().

goaway(_State, _GoAway) ->
	error(todo).

-spec reset_stream(_, _, _) -> no_return().

reset_stream(_State, _StreamID, _Error) ->
	error(todo).

-spec ignored_frame(_, _) -> no_return().

%% @todo Cookie/events.
ignored_frame(_State, _Stream) ->
	error(todo).

stream_abort_receive(State=#http3_state{conn=Conn, transport=Transport},
		Stream=#stream{id=StreamID}, Reason) ->
	Transport:shutdown_stream(Conn, StreamID, receiving, cow_http3:error_to_code(Reason)),
	stream_update(State, Stream#stream{status=discard}).

-spec connection_error(_, _) -> no_return().

connection_error(_State, _Error) ->
	error(todo).

-spec handle_continue(_, _, _, _, _, _) -> no_return().

handle_continue(_ContinueStreamRef, _Msg, _State, _CookieStore, _EvHandler, _EvHandlerState) ->
	error(unimplemented).

-spec update_flow(_, _, _, _) -> no_return().

update_flow(_State, _ReplyTo, _StreamRef, _Inc) ->
	error(unimplemented).

closing(_Reason, _State, _, EvHandlerState) ->
	%% @todo Implement graceful shutdown.
	{[], EvHandlerState}.

close(_Reason, _State, _, EvHandlerState) ->
	%% @todo Implement.
	EvHandlerState.

-spec keepalive(_, _, _) -> no_return().

keepalive(_State, _, _EvHandlerState) ->
	error(todo).

-spec ping(_, _, _, _) -> {error, {ping_not_implemented, reference()}}.

ping(_State, _Tunnel, _ReplyTo, PingRef) ->
	{error, {ping_not_implemented, PingRef}}.

headers(State0=#http3_state{conn=Conn, transport=Transport,
		http3_machine=HTTP3Machine0}, StreamRef, ReplyTo, Method, Host, Port,
		Path, Headers0, _InitialFlow0, CookieStore0, EvHandler, EvHandlerState0)
		when is_reference(StreamRef) ->
	{ok, StreamID} = Transport:start_bidi_stream(Conn),
	HTTP3Machine1 = cow_http3_machine:init_bidi_stream(StreamID,
		iolist_to_binary(Method), HTTP3Machine0),
	{ok, PseudoHeaders, Headers, CookieStore} = prepare_headers(
		Method, Host, Port, Path, Headers0, CookieStore0),
	Authority = maps:get(authority, PseudoHeaders),
	RealStreamRef = StreamRef, %% @todo stream_ref(State0, StreamRef),
	RequestEvent = #{
		stream_ref => RealStreamRef,
		reply_to => ReplyTo,
		function => ?FUNCTION_NAME,
		method => Method,
		authority => Authority,
		path => Path,
		headers => Headers
	},
	EvHandlerState1 = EvHandler:request_start(RequestEvent, EvHandlerState0),
	{ok, nofin, HeaderBlock, Instrs, HTTP3Machine} = cow_http3_machine:prepare_headers(
		StreamID, HTTP3Machine1, nofin, PseudoHeaders, Headers),
	State1 = send_instructions(State0#http3_state{http3_machine=HTTP3Machine}, Instrs),
	ok = Transport:send(Conn, StreamID, cow_http3:headers(HeaderBlock)),
	EvHandlerState = EvHandler:request_headers(RequestEvent, EvHandlerState1),
	%% @todo InitialFlow = initial_flow(InitialFlow0, Opts),
	Stream = #stream{id=StreamID, ref=StreamRef, reply_to=ReplyTo,
		status=normal, authority=Authority, path=Path},
	State = stream_new_local(State1, Stream),
	{{state, State}, CookieStore, EvHandlerState}.

%% @todo We need to configure the initial flow control for the stream.
request(State0=#http3_state{conn=Conn, transport=Transport,
		http3_machine=HTTP3Machine0}, StreamRef, ReplyTo, Method, Host, Port,
		Path, Headers0, Body, _InitialFlow0, CookieStore0, EvHandler, EvHandlerState0)
		when is_reference(StreamRef) ->
	Headers1 = lists:keystore(<<"content-length">>, 1, Headers0,
		{<<"content-length">>, integer_to_binary(iolist_size(Body))}),
	%% @todo InitialFlow = initial_flow(InitialFlow0, Opts),
	{ok, StreamID} = Transport:start_bidi_stream(Conn),
	HTTP3Machine1 = cow_http3_machine:init_bidi_stream(StreamID,
		iolist_to_binary(Method), HTTP3Machine0),
	{ok, PseudoHeaders, Headers, CookieStore} = prepare_headers(
		Method, Host, Port, Path, Headers1, CookieStore0),
	Authority = maps:get(authority, PseudoHeaders),
	RealStreamRef = StreamRef, %% @todo stream_ref(State0, StreamRef),
	RequestEvent = #{
		stream_ref => RealStreamRef,
		reply_to => ReplyTo,
		function => ?FUNCTION_NAME,
		method => Method,
		authority => Authority,
		path => Path,
		headers => Headers
	},
	EvHandlerState1 = EvHandler:request_start(RequestEvent, EvHandlerState0),
	{ok, fin, HeaderBlock, Instrs, HTTP3Machine} = cow_http3_machine:prepare_headers(
		StreamID, HTTP3Machine1, fin, PseudoHeaders, Headers),
	State1 = send_instructions(State0#http3_state{http3_machine=HTTP3Machine}, Instrs),
	%% @todo Handle send errors.
	ok = Transport:send(Conn, StreamID, [
		cow_http3:headers(HeaderBlock),
		%% Only send a DATA frame if we have a body.
		case iolist_size(Body) of
			0 -> <<>>;
			_ -> cow_http3:data(Body)
		end
	], fin),
	EvHandlerState = EvHandler:request_headers(RequestEvent, EvHandlerState1),
	Stream = #stream{id=StreamID, ref=StreamRef, reply_to=ReplyTo,
		status=normal, authority=Authority, path=Path},
	State = stream_new_local(State1, Stream),
	RequestEndEvent = #{
		stream_ref => RealStreamRef,
		reply_to => ReplyTo
	},
	{{state, State}, CookieStore, EvHandler:request_end(RequestEndEvent, EvHandlerState)}.

prepare_headers(Method, Host0, Port, Path, Headers0, CookieStore0) ->
	Scheme = <<"https">>,
	Authority = case lists:keyfind(<<"host">>, 1, Headers0) of
		{_, Host} -> Host;
		_ -> gun_http:host_header(tls, Host0, Port)
	end,
	%% @todo We also must remove any header found in the connection header.
	%% @todo Much of this is duplicated in cow_http2, cow_http2_machine
	%%       and cow_http3_machine; sort things out.
	Headers1 =
		lists:keydelete(<<"host">>, 1,
		lists:keydelete(<<"connection">>, 1,
		lists:keydelete(<<"keep-alive">>, 1,
		lists:keydelete(<<"proxy-connection">>, 1,
		lists:keydelete(<<"transfer-encoding">>, 1,
		lists:keydelete(<<"upgrade">>, 1, Headers0)))))),
	{Headers, CookieStore} = gun_cookies:add_cookie_header(
		Scheme, Authority, Path, Headers1, CookieStore0),
	PseudoHeaders = #{
		method => Method,
		scheme => Scheme,
		authority => Authority,
		path => Path
	},
	{ok, PseudoHeaders, Headers, CookieStore}.

%% @todo We would open unidi streams here if we only open on-demand.
%% No instructions.
send_instructions(State, undefined) ->
	State;
%% Decoder instructions.
send_instructions(State=#http3_state{conn=Conn, transport=Transport,
		local_decoder_id=DecoderID}, {decoder_instructions, DecData}) ->
	%% @todo Handle send errors.
	ok = Transport:send(Conn, DecoderID, DecData),
	State;
%% Encoder instructions.
send_instructions(State=#http3_state{conn=Conn, transport=Transport,
		local_encoder_id=EncoderID}, {encoder_instructions, EncData}) ->
	%% @todo Handle send errors.
	ok = Transport:send(Conn, EncoderID, EncData),
	State.

data(State=#http3_state{conn=Conn, transport=Transport}, StreamRef, _ReplyTo, IsFin, Data,
		_EvHandler, EvHandlerState) when is_reference(StreamRef) ->
	case stream_get_by_ref(State, StreamRef) of
		#stream{id=StreamID} -> %, tunnel=Tunnel} ->
			ok = Transport:send(Conn, StreamID, cow_http3:data(Data), IsFin),
			{[], EvHandlerState} %% @todo Probably wrong, need to update/keep states?
%% @todo
%			case cow_http2_machine:get_stream_local_state(StreamID, HTTP2Machine) of
%				{ok, fin, _} ->
%					error_stream_closed(State, StreamRef, ReplyTo),
%					{[], EvHandlerState};
%				{ok, _, fin} ->
%					error_stream_closed(State, StreamRef, ReplyTo),
%					{[], EvHandlerState};
%				{ok, _, _} when Tunnel =:= undefined ->
%					maybe_send_data(State,
%						StreamID, IsFin, Data, EvHandler, EvHandlerState);
%				{ok, _, _} ->
%					#tunnel{protocol=Proto, protocol_state=ProtoState0} = Tunnel,
%					{Commands, EvHandlerState1} = Proto:data(ProtoState0, StreamRef,
%						ReplyTo, IsFin, Data, EvHandler, EvHandlerState),
%					tunnel_commands(Commands, Stream, State, EvHandler, EvHandlerState1)
%			end%;
%% @todo
%		error ->
%			error_stream_not_found(State, StreamRef, ReplyTo),
%			{[], EvHandlerState}
	end.

-spec connect(_, _, _, _, _, _, _, _, _, _) -> no_return().

connect(_State, StreamRef, _ReplyTo, _Destination, _TunnelInfo, _Headers0,
		_InitialFlow0, _CookieStore, _EvHandler, _EvHandlerState0)
		when is_reference(StreamRef) ->
	error(unimplemented).

-spec cancel(_, _, _, _, _) -> no_return().

cancel(_State, StreamRef, _ReplyTo, _EvHandler, _EvHandlerState0)
		when is_reference(StreamRef) ->
	error(unimplemented).

-spec timeout(_, _, _) -> no_return().

timeout(_State, _Timeout, _TRef) ->
	error(todo).

-spec stream_info(_, _) -> no_return().

stream_info(_State, StreamRef) when is_reference(StreamRef) ->
	error(unimplemented).

-spec down(_) -> no_return().

down(_State) ->
	error(todo).

-spec ws_upgrade(_, _, _, _, _, _, _, _, _, _, _) -> no_return().

ws_upgrade(_State, StreamRef, _ReplyTo,
		_Host, _Port, _Path, _Headers0, _WsOpts,
		_CookieStore0, _EvHandler, _EvHandlerState0)
		when is_reference(StreamRef) ->
	error(todo).

-spec ws_send(_, _, _, _, _, _) -> no_return().

ws_send(_Frames, _State, _RealStreamRef, _ReplyTo, _EvHandler, _EvHandlerState0) ->
	error(todo).

%% Streams.

stream_get(#http3_state{streams=Streams}, StreamID) ->
	maps:get(StreamID, Streams, error).

stream_get_by_ref(State=#http3_state{stream_refs=Refs}, StreamRef) ->
	case maps:get(StreamRef, Refs, error) of
		error -> error;
		StreamID -> stream_get(State, StreamID)
	end.

stream_new_remote(State=#http3_state{http3_machine=HTTP3Machine0,
		streams=Streams, stream_refs=Refs}, StreamID, StreamType) ->
	%% All remote streams to the client are expected to be unidirectional.
	%% @todo Handle errors instead of crashing.
	unidi = StreamType,
	HTTP3Machine = cow_http3_machine:init_unidi_stream(StreamID,
		unidi_remote, HTTP3Machine0),
	StreamRef = make_ref(),
	Stream = #stream{id=StreamID, ref=StreamRef, status=header},
	State#http3_state{
		http3_machine=HTTP3Machine,
		streams=Streams#{StreamID => Stream},
		stream_refs=Refs#{StreamRef => StreamID}
	}.

stream_new_local(State=#http3_state{streams=Streams, stream_refs=Refs},
		Stream=#stream{id=StreamID, ref=StreamRef}) ->
	State#http3_state{
		streams=Streams#{StreamID => Stream},
		stream_refs=Refs#{StreamRef => StreamID}
	}.

stream_update(State=#http3_state{streams=Streams},
		Stream=#stream{id=StreamID}) ->
	State#http3_state{
		streams=Streams#{StreamID => Stream}
	}.

stream_aborted(State0, StreamID, Reason, EvHandler, EvHandlerState0) ->
	case stream_take(State0, StreamID) of
		{#stream{ref=StreamRef, reply_to=ReplyTo}, State} ->
			gun:reply(ReplyTo, {gun_error, self(), StreamRef, %% @todo stream_ref(State0, StreamRef),
				{stream_error, Reason, 'Stream reset by server.'}}),
			EvHandlerState = EvHandler:cancel(#{
				stream_ref => StreamRef, %% @todo stream_ref(State, StreamRef),
				reply_to => ReplyTo,
				endpoint => remote,
				reason => Reason
			}, EvHandlerState0),
			{{state, State}, EvHandlerState};
		error ->
			{{state, State0}, EvHandlerState0}
	end.

stream_take(State=#http3_state{streams=Streams0, stream_refs=Refs}, StreamID) ->
	case maps:take(StreamID, Streams0) of
		{Stream=#stream{ref=StreamRef}, Streams} ->
			{Stream, State#http3_state{
				streams=Streams,
				stream_refs=maps:remove(StreamRef, Refs)
			}};
		error ->
			error
	end.
