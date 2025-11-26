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

-module(gun_ws).

-export([check_options/1]).
-export([select_extensions/3]).
-export([select_protocol/2]).
-export([name/0]).
-export([opts_name/0]).
-export([has_keepalive/0]).
-export([default_keepalive/0]).
-export([init/4]).
-export([handle/5]).
-export([handle_continue/6]).
-export([update_flow/4]).
-export([closing/4]).
-export([close/4]).
-export([keepalive/3]).
-export([ping/4]).
-export([ws_send/6]).
-export([down/1]).

-record(payload, {
	type = undefined :: cow_ws:frame_type(),
	rsv = undefined :: cow_ws:rsv(),
	len = undefined :: non_neg_integer(),
	mask_key = undefined :: cow_ws:mask_key(),
	close_code = undefined :: undefined | cow_ws:close_code(),
	unmasked = <<>> :: binary(),
	unmasked_len = 0 :: non_neg_integer()
}).

-record(ws_state, {
	reply_to :: gun:reply_to(),
	stream_ref :: reference(),
	socket :: inet:socket() | ssl:sslsocket(),
	transport :: module(),
	opts = #{} :: gun:ws_opts(),
	buffer = <<>> :: binary(),
	in = head :: head | #payload{} | close,
	out = head :: head | close,
	frag_state = undefined :: cow_ws:frag_state(),
	utf8_state = 0 :: cow_ws:utf8_state(),
	extensions = #{} :: cow_ws:extensions(),
	flow :: integer() | infinity,
	handler :: module(),
	handler_state :: any()
}).

check_options(Opts) ->
	do_check_options(maps:to_list(Opts)).

do_check_options([]) ->
	ok;
do_check_options([{closing_timeout, infinity}|Opts]) ->
	do_check_options(Opts);
do_check_options([{closing_timeout, T}|Opts]) when is_integer(T), T > 0 ->
	do_check_options(Opts);
do_check_options([{compress, B}|Opts]) when is_boolean(B) ->
	do_check_options(Opts);
do_check_options([{default_protocol, M}|Opts]) when is_atom(M) ->
	do_check_options(Opts);
do_check_options([{flow, InitialFlow}|Opts]) when is_integer(InitialFlow), InitialFlow > 0 ->
	do_check_options(Opts);
do_check_options([{keepalive, infinity}|Opts]) ->
	do_check_options(Opts);
do_check_options([{keepalive, K}|Opts]) when is_integer(K), K > 0 ->
	do_check_options(Opts);
do_check_options([Opt={protocols, L}|Opts]) when is_list(L) ->
	case lists:usort(lists:flatten([[is_binary(B), is_atom(M)] || {B, M} <- L])) of
		[true] -> do_check_options(Opts);
		_ -> {error, {options, {ws, Opt}}}
	end;
do_check_options([{reply_to, P}|Opts]) when is_pid(P) ->
	do_check_options(Opts);
do_check_options([{reply_to, F}|Opts]) when is_function(F, 1) ->
	do_check_options(Opts);
do_check_options([{reply_to, {F, A}}|Opts]) when is_function(F, 1 + length(A)) ->
	do_check_options(Opts);
do_check_options([{reply_to, {M, F, A}}|Opts]) when is_atom(M), is_atom(F), is_list(A) ->
	do_check_options(Opts);
do_check_options([{silence_pings, B}|Opts]) when is_boolean(B) ->
	do_check_options(Opts);
do_check_options([{user_opts, _}|Opts]) ->
	do_check_options(Opts);
do_check_options([Opt|_]) ->
	{error, {options, {ws, Opt}}}.

select_extensions(Headers, Extensions0, Opts) ->
	case lists:keyfind(<<"sec-websocket-extensions">>, 1, Headers) of
		false ->
			#{};
		{_, ExtHd} ->
			ParsedExtHd = cow_http_hd:parse_sec_websocket_extensions(ExtHd),
			validate_extensions(ParsedExtHd, Extensions0, Opts, #{})
	end.

validate_extensions([], _, _, Acc) ->
	Acc;
validate_extensions([{Name = <<"permessage-deflate">>, Params}|Tail], Extensions, Opts, Acc0) ->
	case lists:member(Name, Extensions) of
		true ->
			case cow_ws:validate_permessage_deflate(Params, Acc0, Opts) of
				{ok, Acc} -> validate_extensions(Tail, Extensions, Opts, Acc);
				error -> close
			end;
		%% Fail the connection if extension was not requested.
		false ->
			close
	end;
%% Fail the connection on unknown extension.
validate_extensions(_, _, _, _) ->
	close.

%% @todo Validate protocols.
select_protocol(Headers, Opts) ->
	case lists:keyfind(<<"sec-websocket-protocol">>, 1, Headers) of
		false ->
			maps:get(default_protocol, Opts, gun_ws_h);
		{_, Proto} ->
			ProtoOpt = maps:get(protocols, Opts, []),
			case lists:keyfind(Proto, 1, ProtoOpt) of
				{_, Handler} ->
					Handler;
				false ->
					close
			end
	end.

name() -> ws.
opts_name() -> ws_opts.
has_keepalive() -> true.
default_keepalive() -> infinity.

init(ReplyTo, Socket, Transport, #{stream_ref := StreamRef, headers := Headers,
		extensions := Extensions, flow := InitialFlow, handler := Handler, opts := Opts}) ->
	{ok, HandlerState} = Handler:init(ReplyTo, StreamRef, Headers, Opts),
	{ok, connected_ws_only, #ws_state{reply_to=ReplyTo, stream_ref=StreamRef,
		socket=Socket, transport=Transport, opts=Opts, extensions=Extensions,
		flow=InitialFlow, handler=Handler, handler_state=HandlerState}}.

handle(Data, State, CookieStore, EvHandler, EvHandlerState0) ->
	{Commands, EvHandlerState} = handle(Data, State, EvHandler, EvHandlerState0),
	{Commands, CookieStore, EvHandlerState}.

%% Do not handle anything if we received a close frame.
%% Initiate or terminate the closing state depending on whether we sent a close yet.
handle(_, State=#ws_state{in=close, out=close}, _, EvHandlerState) ->
	{[{state, State}, close], EvHandlerState};
handle(_, State=#ws_state{in=close}, EvHandler, EvHandlerState) ->
	closing(normal, State, EvHandler, EvHandlerState);
%% Shortcut for common case when Data is empty after processing a frame.
handle(<<>>, State=#ws_state{in=head}, _, EvHandlerState) ->
	maybe_active(State, EvHandlerState);
handle(Data, State=#ws_state{reply_to=ReplyTo, stream_ref=StreamRef, buffer=Buffer,
		in=head, frag_state=FragState, extensions=Extensions},
		EvHandler, EvHandlerState0) ->
	%% Send the event only if there was no data in the buffer.
	%% If there is data in the buffer then we already sent the event.
	EvHandlerState1 = case Buffer of
		<<>> ->
			EvHandler:ws_recv_frame_start(#{
				stream_ref => StreamRef,
				reply_to => ReplyTo,
				frag_state => FragState,
				extensions => Extensions
			}, EvHandlerState0);
		_ ->
			EvHandlerState0
	end,
	Data2 = << Buffer/binary, Data/binary >>,
	case cow_ws:parse_header(Data2, Extensions, FragState) of
		{Type, FragState2, Rsv, Len, MaskKey, Rest} ->
			EvHandlerState = EvHandler:ws_recv_frame_header(#{
				stream_ref => StreamRef,
				reply_to => ReplyTo,
				frag_state => FragState2,
				extensions => Extensions,
				type => Type,
				rsv => Rsv,
				len => Len,
				mask_key => MaskKey
			}, EvHandlerState1),
			handle(Rest, State#ws_state{buffer= <<>>,
				in=#payload{type=Type, rsv=Rsv, len=Len, mask_key=MaskKey},
				frag_state=FragState2}, EvHandler, EvHandlerState);
		more ->
			maybe_active(State#ws_state{buffer=Data2}, EvHandlerState1);
		error ->
			closing({error, badframe}, State, EvHandler, EvHandlerState1)
	end;
handle(Data, State=#ws_state{in=In=#payload{type=Type, rsv=Rsv, len=Len, mask_key=MaskKey,
		close_code=CloseCode, unmasked=Unmasked, unmasked_len=UnmaskedLen}, frag_state=FragState,
		utf8_state=Utf8State, extensions=Extensions}, EvHandler, EvHandlerState) ->
	case cow_ws:parse_payload(Data, MaskKey, Utf8State, UnmaskedLen, Type, Len, FragState, Extensions, Rsv) of
		{ok, CloseCode2, Payload, Utf8State2, Rest} ->
			dispatch(Rest, State#ws_state{in=head, utf8_state=Utf8State2}, Type,
				<<Unmasked/binary, Payload/binary>>, CloseCode2,
				EvHandler, EvHandlerState);
		{ok, Payload, Utf8State2, Rest} ->
			dispatch(Rest, State#ws_state{in=head, utf8_state=Utf8State2}, Type,
				<<Unmasked/binary, Payload/binary>>, CloseCode,
				EvHandler, EvHandlerState);
		{more, CloseCode2, Payload, Utf8State2} ->
			maybe_active(State#ws_state{in=In#payload{close_code=CloseCode2,
				unmasked= <<Unmasked/binary, Payload/binary>>,
				len=Len - byte_size(Data), unmasked_len=2 + byte_size(Data)}, utf8_state=Utf8State2},
				EvHandlerState);
		{more, Payload, Utf8State2} ->
			maybe_active(State#ws_state{in=In#payload{unmasked= <<Unmasked/binary, Payload/binary>>,
				len=Len - byte_size(Data), unmasked_len=UnmaskedLen + byte_size(Data)}, utf8_state=Utf8State2},
				EvHandlerState);
		Error = {error, _Reason} ->
			closing(Error, State, EvHandler, EvHandlerState)
	end.

handle_continue(ContinueStreamRef, {data, _ReplyTo, _StreamRef, IsFin, Data},
		#ws_state{}, CookieStore, _EvHandler, EvHandlerState)
		when is_reference(ContinueStreamRef) ->
	{{send, IsFin, Data}, CookieStore, EvHandlerState}.

maybe_active(State=#ws_state{flow=Flow}, EvHandlerState) ->
	{[
		{state, State},
		{active, Flow > 0}
	], EvHandlerState}.

dispatch(Rest, State0=#ws_state{reply_to=ReplyTo, stream_ref=StreamRef,
		frag_state=FragState, extensions=Extensions, flow=Flow0,
		handler=Handler, handler_state=HandlerState0},
		Type, Payload, CloseCode, EvHandler, EvHandlerState0) ->
	EvHandlerState1 = EvHandler:ws_recv_frame_end(#{
		stream_ref => StreamRef,
		reply_to => ReplyTo,
		extensions => Extensions,
		close_code => CloseCode,
		payload => Payload
	}, EvHandlerState0),
	case cow_ws:make_frame(Type, Payload, CloseCode, FragState) of
		Frame ->
			{ok, Dec, HandlerState} = Handler:handle(Frame, HandlerState0),
			Flow = case Flow0 of
				infinity -> infinity;
				_ -> Flow0 - Dec
			end,
			State1 = State0#ws_state{flow=Flow, handler_state=HandlerState},
			case Frame of
				ping ->
					case send(pong, State1, ReplyTo, EvHandler, EvHandlerState1) of
						{[], EvHandlerState2} ->
							handle(Rest, State1, EvHandler, EvHandlerState2);
						{Error={error, _}, EvHandlerState2} ->
							{[{state, State1}, Error], EvHandlerState2}
					end;
				{ping, Payload} ->
					case send({pong, Payload}, State1, ReplyTo, EvHandler, EvHandlerState1) of
						{[], EvHandlerState2} ->
							handle(Rest, State1, EvHandler, EvHandlerState2);
						{Error={error, _}, EvHandlerState2} ->
							{[{state, State1}, Error], EvHandlerState2}
					end;
				close ->
					State = State1#ws_state{in=close},
					handle(Rest, State, EvHandler, EvHandlerState1);
				{close, _, _} ->
					State = State1#ws_state{in=close},
					handle(Rest, State, EvHandler, EvHandlerState1);
				{fragment, fin, _, _} ->
					State = State1#ws_state{frag_state=undefined},
					handle(Rest, State, EvHandler, EvHandlerState1);
				_ ->
					handle(Rest, State1, EvHandler, EvHandlerState1)
			end
	end.

update_flow(State=#ws_state{flow=Flow0}, _ReplyTo, _StreamRef, Inc) ->
	Flow = case Flow0 of
		infinity -> infinity;
		_ -> Flow0 + Inc
	end,
	[
		{state, State#ws_state{flow=Flow}},
		{active, Flow > 0}
	].

%% The user already sent the close frame; do nothing.
closing(_, State=#ws_state{out=close}, _, EvHandlerState) ->
	{closing(State), EvHandlerState};
closing(Reason, State=#ws_state{reply_to=ReplyTo}, EvHandler, EvHandlerState) ->
	Code = case Reason of
		normal -> 1000;
		owner_down -> 1001;
		shutdown -> 1001;
		{error, badframe} -> 1002;
		{error, badencoding} -> 1007
	end,
	send({close, Code, <<>>}, State, ReplyTo, EvHandler, EvHandlerState).

closing(#ws_state{opts=Opts}) ->
	Timeout = maps:get(closing_timeout, Opts, 15000),
	{closing, Timeout}.

close(_, _, _, EvHandlerState) ->
	EvHandlerState.

keepalive(State=#ws_state{reply_to=ReplyTo}, EvHandler, EvHandlerState0) ->
	send(ping, State, ReplyTo, EvHandler, EvHandlerState0).

ping(_State, undefined, _ReplyTo, PingRef) ->
	{error, {ping_not_implemented, PingRef}}.

%% Send one frame.
send(Frame, State=#ws_state{stream_ref=StreamRef,
		socket=Socket, transport=Transport, in=In, extensions=Extensions},
		ReplyTo, EvHandler, EvHandlerState0) ->
	WsSendFrameEvent = #{
		stream_ref => StreamRef,
		reply_to => ReplyTo,
		extensions => Extensions,
		frame => Frame
	},
	EvHandlerState1 = EvHandler:ws_send_frame_start(WsSendFrameEvent, EvHandlerState0),
	case Transport:send(Socket, cow_ws:masked_frame(Frame, Extensions)) of
		ok ->
			EvHandlerState = EvHandler:ws_send_frame_end(WsSendFrameEvent, EvHandlerState1),
			if
				Frame =:= close; element(1, Frame) =:= close ->
					{[
						{state, State#ws_state{out=close}},
						%% We can close immediately if we already
						%% received a close frame.
						case In of
							close -> close;
							_ -> closing(State)
						end
					], EvHandlerState};
				true ->
					{[], EvHandlerState}
			end;
		Error={error, _} ->
			{Error, EvHandlerState1}
	end.

%% Send many frames.
ws_send(Frame, State, ReplyTo, EvHandler, EvHandlerState) when not is_list(Frame) ->
	send(Frame, State, ReplyTo, EvHandler, EvHandlerState);
ws_send([], _, _, _, EvHandlerState) ->
	{[], EvHandlerState};
ws_send([Frame|Tail], State, ReplyTo, EvHandler, EvHandlerState0) ->
	case send(Frame, State, ReplyTo, EvHandler, EvHandlerState0) of
		{[], EvHandlerState} ->
			ws_send(Tail, State, ReplyTo, EvHandler, EvHandlerState);
		Other ->
			Other
	end.

%% @todo We should probably check the _StreamRef value.
ws_send(Frames, State, _StreamRef, ReplyTo, EvHandler, EvHandlerState) ->
	ws_send(Frames, State, ReplyTo, EvHandler, EvHandlerState).

down(#ws_state{stream_ref=StreamRef}) ->
	[StreamRef].
