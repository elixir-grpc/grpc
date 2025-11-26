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

-module(gun_http).

-export([check_options/1]).
-export([name/0]).
-export([opts_name/0]).
-export([has_keepalive/0]).
-export([default_keepalive/0]).
-export([init/4]).
-export([switch_transport/3]).
-export([handle/5]).
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
-export([stream_info/2]).
-export([down/1]).
-export([ws_upgrade/11]).
-export([ws_send/6]).

%% Functions shared with gun_http2 and gun_pool.
-export([host_header/3]).

-type io() :: head | {body, non_neg_integer()} | body_close | body_chunked | body_trailer.

%% @todo Make that a record.
-type connect_info() :: {connect, gun:stream_ref(), gun:connect_destination()}.

-record(websocket, {
	ref :: gun:stream_ref(),
	reply_to :: gun:reply_to(),
	key :: binary(),
	extensions :: [binary()],
	opts :: gun:ws_opts()
}).

-record(stream, {
	ref :: gun:stream_ref() | connect_info() | #websocket{},
	reply_to :: gun:reply_to(),
	flow :: integer() | infinity,
	method :: binary(),

	%% Request target URI.
	authority :: iodata(),
	path :: iodata(),

	is_alive :: boolean(),
	handler_state :: undefined | gun_content_handler:state()
}).

-record(http_state, {
	socket :: inet:socket() | ssl:sslsocket(),
	transport :: module(),
	opts = #{} :: gun:http_opts(),
	version = 'HTTP/1.1' :: cow_http1:version(),
	connection = keepalive :: keepalive | close,
	buffer = <<>> :: binary(),

	%% Base stream ref, defined when the protocol runs
	%% inside an HTTP/2 CONNECT stream.
	base_stream_ref = undefined :: undefined | gun:stream_ref(),

	streams = [] :: [#stream{}],
	in = head :: io(),
	in_state = {0, 0} :: {non_neg_integer(), non_neg_integer()},
	out = head :: io()
}).

check_options(Opts) ->
	do_check_options(maps:to_list(Opts)).

do_check_options([]) ->
	ok;
do_check_options([{closing_timeout, infinity}|Opts]) ->
	do_check_options(Opts);
do_check_options([{closing_timeout, T}|Opts]) when is_integer(T), T > 0 ->
	do_check_options(Opts);
do_check_options([Opt={content_handlers, Handlers}|Opts]) ->
	case gun_content_handler:check_option(Handlers) of
		ok -> do_check_options(Opts);
		error -> {error, {options, {http, Opt}}}
	end;
do_check_options([{cookie_ignore_informational, B}|Opts]) when is_boolean(B) ->
	do_check_options(Opts);
do_check_options([{flow, InitialFlow}|Opts]) when is_integer(InitialFlow), InitialFlow > 0 ->
	do_check_options(Opts);
do_check_options([{keepalive, infinity}|Opts]) ->
	do_check_options(Opts);
do_check_options([{keepalive, K}|Opts]) when is_integer(K), K > 0 ->
	do_check_options(Opts);
do_check_options([{transform_header_name, F}|Opts]) when is_function(F) ->
	do_check_options(Opts);
do_check_options([{version, V}|Opts]) when V =:= 'HTTP/1.1'; V =:= 'HTTP/1.0' ->
	do_check_options(Opts);
do_check_options([Opt|_]) ->
	{error, {options, {http, Opt}}}.

name() -> http.
opts_name() -> http_opts.
has_keepalive() -> true.
default_keepalive() -> infinity.

init(_ReplyTo, Socket, Transport, Opts) ->
	BaseStreamRef = maps:get(stream_ref, Opts, undefined),
	Version = maps:get(version, Opts, 'HTTP/1.1'),
	{ok, connected, #http_state{socket=Socket, transport=Transport,
		opts=Opts, version=Version, base_stream_ref=BaseStreamRef}}.

switch_transport(Transport, Socket, State) ->
	State#http_state{socket=Socket, transport=Transport}.

%% Stop looping when we got no more data.
handle(<<>>, State, CookieStore, _, EvHandlerState) ->
	{{state, State}, CookieStore, EvHandlerState};
%% Close when server responds and we don't have any open streams.
handle(_, #http_state{streams=[]}, CookieStore, _, EvHandlerState) ->
	{close, CookieStore, EvHandlerState};
%% Wait for the full response headers before trying to parse them.
handle(Data, State=#http_state{in=head, buffer=Buffer,
		streams=[#stream{ref=StreamRef, reply_to=ReplyTo}|_]},
		CookieStore, EvHandler, EvHandlerState0) ->
	%% Send the event only if there was no data in the buffer.
	%% If there is data in the buffer then we already sent the event.
	EvHandlerState = case Buffer of
		<<>> ->
			EvHandler:response_start(#{
				stream_ref => stream_ref(State, StreamRef),
				reply_to => ReplyTo
			}, EvHandlerState0);
		_ ->
			EvHandlerState0
	end,
	Data2 = << Buffer/binary, Data/binary >>,
	case binary:match(Data2, <<"\r\n\r\n">>) of
		nomatch ->
			{{state, State#http_state{buffer=Data2}}, CookieStore, EvHandlerState};
		{_, _} ->
			handle_head(Data2, State#http_state{buffer= <<>>},
				CookieStore, EvHandler, EvHandlerState)
	end;
%% Everything sent to the socket until it closes is part of the response body.
handle(Data, State=#http_state{in=body_close}, CookieStore, _, EvHandlerState) ->
	{send_data(Data, State, nofin), CookieStore, EvHandlerState};
%% Chunked transfer-encoding may contain both data and trailers.
handle(Data, State=#http_state{in=body_chunked, in_state=InState, buffer=Buffer,
		streams=[#stream{ref=StreamRef, reply_to=ReplyTo}|_], connection=Conn},
		CookieStore, EvHandler, EvHandlerState0) ->
	Buffer2 = << Buffer/binary, Data/binary >>,
	case cow_http_te:stream_chunked(Buffer2, InState) of
		more ->
			{{state, State#http_state{buffer=Buffer2}}, CookieStore, EvHandlerState0};
		{more, Data2, InState2} ->
			{send_data(Data2, State#http_state{buffer= <<>>, in_state=InState2}, nofin),
				CookieStore, EvHandlerState0};
		{more, Data2, Length, InState2} when is_integer(Length) ->
			%% @todo See if we can recv faster than one message at a time.
			{send_data(Data2, State#http_state{buffer= <<>>, in_state=InState2}, nofin),
				CookieStore, EvHandlerState0};
		{more, Data2, Rest, InState2} ->
			%% @todo See if we can recv faster than one message at a time.
			{send_data(Data2, State#http_state{buffer=Rest, in_state=InState2}, nofin),
				CookieStore, EvHandlerState0};
		{done, HasTrailers, Rest} ->
			%% @todo response_end should be called AFTER send_data
			{IsFin, EvHandlerState} = case HasTrailers of
				trailers ->
					{nofin, EvHandlerState0};
				no_trailers ->
					EvHandlerState1 = EvHandler:response_end(#{
						stream_ref => stream_ref(State, StreamRef),
						reply_to => ReplyTo
					}, EvHandlerState0),
					{fin, EvHandlerState1}
			end,
			%% I suppose it doesn't hurt to append an empty binary.
			%% We ignore the active command because the stream ended.
			[{state, State1}|_] = send_data(<<>>, State, IsFin),
			case {HasTrailers, Conn} of
				{trailers, _} ->
					handle(Rest, State1#http_state{buffer = <<>>, in=body_trailer},
						CookieStore, EvHandler, EvHandlerState);
				{no_trailers, keepalive} ->
					handle(Rest, end_stream(State1#http_state{buffer= <<>>}),
						CookieStore, EvHandler, EvHandlerState);
				{no_trailers, close} ->
					{[{state, end_stream(State1)}, close], CookieStore, EvHandlerState}
			end;
		{done, Data2, HasTrailers, Rest} ->
			%% @todo response_end should be called AFTER send_data
			{IsFin, EvHandlerState} = case HasTrailers of
				trailers ->
					{nofin, EvHandlerState0};
				no_trailers ->
					EvHandlerState1 = EvHandler:response_end(#{
						stream_ref => stream_ref(State, StreamRef),
						reply_to => ReplyTo
					}, EvHandlerState0),
					{fin, EvHandlerState1}
			end,
			%% We ignore the active command because the stream ended.
			[{state, State1}|_] = send_data(Data2, State, IsFin),
			case {HasTrailers, Conn} of
				{trailers, _} ->
					handle(Rest, State1#http_state{buffer = <<>>, in=body_trailer},
						CookieStore, EvHandler, EvHandlerState);
				{no_trailers, keepalive} ->
					handle(Rest, end_stream(State1#http_state{buffer= <<>>}),
						CookieStore, EvHandler, EvHandlerState);
				{no_trailers, close} ->
					{[{state, end_stream(State1)}, close], CookieStore, EvHandlerState}
			end
	end;
handle(Data, State=#http_state{in=body_trailer, buffer=Buffer, connection=Conn,
		streams=[#stream{ref=StreamRef, reply_to=ReplyTo}|_]},
		CookieStore, EvHandler, EvHandlerState0) ->
	Data2 = << Buffer/binary, Data/binary >>,
	case binary:match(Data2, <<"\r\n\r\n">>) of
		nomatch ->
			{{state, State#http_state{buffer=Data2}}, CookieStore, EvHandlerState0};
		{_, _} ->
			{Trailers, Rest} = cow_http:parse_headers(Data2),
			%% @todo We probably want to pass this to gun_content_handler?
			RealStreamRef = stream_ref(State, StreamRef),
			gun:reply(ReplyTo, {gun_trailers, self(), RealStreamRef, Trailers}),
			ResponseEvent = #{
				stream_ref => RealStreamRef,
				reply_to => ReplyTo
			},
			EvHandlerState1 = EvHandler:response_trailers(ResponseEvent#{headers => Trailers}, EvHandlerState0),
			EvHandlerState = EvHandler:response_end(ResponseEvent, EvHandlerState1),
			case Conn of
				keepalive ->
					handle(Rest, end_stream(State#http_state{buffer= <<>>}),
						CookieStore, EvHandler, EvHandlerState);
				close ->
					{[{state, end_stream(State)}, close], CookieStore, EvHandlerState}
			end
	end;
%% We know the length of the rest of the body.
handle(Data, State=#http_state{in={body, Length}, connection=Conn,
		streams=[#stream{ref=StreamRef, reply_to=ReplyTo}|_]},
		CookieStore, EvHandler, EvHandlerState0) ->
	DataSize = byte_size(Data),
	if
		%% More data coming.
		DataSize < Length ->
			{send_data(Data, State#http_state{in={body, Length - DataSize}}, nofin),
				CookieStore, EvHandlerState0};
		%% Stream finished, no rest.
		DataSize =:= Length ->
			%% We ignore the active command because the stream ended.
			[{state, State1}|_] = send_data(Data, State, fin),
			EvHandlerState = EvHandler:response_end(#{
				stream_ref => stream_ref(State, StreamRef),
				reply_to => ReplyTo
			}, EvHandlerState0),
			case Conn of
				keepalive ->
					{[{state, end_stream(State1)}, {active, true}], CookieStore, EvHandlerState};
				close ->
					{[{state, end_stream(State1)}, close], CookieStore, EvHandlerState}
			end;
		%% Stream finished, rest.
		true ->
			<< Body:Length/binary, Rest/bits >> = Data,
			%% We ignore the active command because the stream ended.
			[{state, State1}|_] = send_data(Body, State, fin),
			EvHandlerState = EvHandler:response_end(#{
				stream_ref => stream_ref(State1, StreamRef),
				reply_to => ReplyTo
			}, EvHandlerState0),
			case Conn of
				keepalive -> handle(Rest, end_stream(State1), CookieStore, EvHandler, EvHandlerState);
				close -> {[{state, end_stream(State1)}, close], CookieStore, EvHandlerState}
			end
	end.

handle_head(Data, State=#http_state{opts=Opts,
		streams=[#stream{ref=StreamRef, authority=Authority, path=Path}|_]},
		CookieStore0, EvHandler, EvHandlerState) ->
	{Version, Status, _, Rest0} = cow_http:parse_status_line(Data),
	{Headers, Rest} = cow_http:parse_headers(Rest0),
	CookieStore = gun_cookies:set_cookie_header(scheme(State),
		Authority, Path, Status, Headers, CookieStore0, Opts),
	case StreamRef of
		{connect, _, _} when Status >= 200, Status < 300 ->
			handle_connect(Rest, State, CookieStore, EvHandler, EvHandlerState, Status, Headers);
		_ when Status >= 100, Status =< 199 ->
			handle_inform(Rest, State, CookieStore, EvHandler, EvHandlerState, Version, Status, Headers);
		_ ->
			handle_response(Rest, State, CookieStore, EvHandler, EvHandlerState, Version, Status, Headers)
	end.

%% We handle HTTP/1.0 responses to CONNECT requests the same as HTTP/1.1.
%% This is because many proxies have historically used HTTP/1.0 for their
%% response. The HTTP/1.1 specification does not disallow it: servers that
%% respond positively to a CONNECT request are supposed to implement it.
handle_connect(Rest, State=#http_state{
		streams=[Stream=#stream{ref={_, StreamRef, Destination}, reply_to=ReplyTo}|Tail]},
		CookieStore, EvHandler, EvHandlerState0, Status, Headers) ->
	RealStreamRef = stream_ref(State, StreamRef),
	%% @todo If the stream is cancelled we probably shouldn't finish the CONNECT setup.
	_ = case Stream of
		#stream{is_alive=false} -> ok;
		_ -> gun:reply(ReplyTo, {gun_response, self(), RealStreamRef, fin, Status, Headers})
	end,
	%% @todo Figure out whether the event should trigger if the stream was cancelled.
	EvHandlerState1 = EvHandler:response_headers(#{
		stream_ref => RealStreamRef,
		reply_to => ReplyTo,
		status => Status,
		headers => Headers
	}, EvHandlerState0),
	EvHandlerState = EvHandler:response_end(#{
		stream_ref => RealStreamRef,
		reply_to => ReplyTo
	}, EvHandlerState1),
	%% We expect there to be no additional data after the CONNECT response.
	%% @todo That's probably wrong.
	<<>> = Rest,
	_ = end_stream(State#http_state{streams=[Stream|Tail]}),
	NewHost = maps:get(host, Destination),
	NewPort = maps:get(port, Destination),
	case Destination of
		#{transport := tls} ->
			HandshakeEvent = #{
				stream_ref => RealStreamRef,
				reply_to => ReplyTo,
				tls_opts => maps:get(tls_opts, Destination, []),
				timeout => maps:get(tls_handshake_timeout, Destination, infinity)
			},
			Protocols = maps:get(protocols, Destination, [http2, http]),
			{[
				{origin, <<"https">>, NewHost, NewPort, connect},
				{tls_handshake, HandshakeEvent, Protocols, ReplyTo}
			], CookieStore, EvHandlerState};
		_ ->
			[NewProtocol0] = maps:get(protocols, Destination, [http]),
			NewProtocol = gun_protocols:add_stream_ref(NewProtocol0, RealStreamRef),
			Protocol = gun_protocols:handler(NewProtocol),
			gun:reply(ReplyTo, {gun_tunnel_up, self(), RealStreamRef, Protocol:name()}),
			{[
				{origin, <<"http">>, NewHost, NewPort, connect},
				{switch_protocol, NewProtocol, ReplyTo, <<>>}
			], CookieStore, EvHandlerState}
	end.

%% @todo We probably shouldn't send info messages if the stream is not alive.
handle_inform(Rest, State=#http_state{
		streams=[#stream{ref=StreamRef, reply_to=ReplyTo}|_]},
		CookieStore, EvHandler, EvHandlerState0, Version, Status, Headers) ->
	EvHandlerState = EvHandler:response_inform(#{
		stream_ref => stream_ref(State, StreamRef),
		reply_to => ReplyTo,
		status => Status,
		headers => Headers
	}, EvHandlerState0),
	case {Version, Status, StreamRef} of
		{'HTTP/1.1', 101, #websocket{}} ->
			{ws_handshake(Rest, State, StreamRef, Headers), CookieStore, EvHandlerState};
		%% Any other 101 response results in us switching to the raw protocol.
		%% @todo We should check that we asked for an upgrade before accepting it.
		{'HTTP/1.1', 101, _} when is_reference(StreamRef) ->
			try
				{_, Upgrade0} = lists:keyfind(<<"upgrade">>, 1, Headers),
				Upgrade = cow_http_hd:parse_upgrade(Upgrade0),
				gun:reply(ReplyTo, {gun_upgrade, self(), stream_ref(State, StreamRef), Upgrade, Headers}),
				%% @todo We probably need to add_stream_ref?
				{{switch_protocol, raw, ReplyTo, Rest}, CookieStore, EvHandlerState0}
			catch _:_ ->
				%% When the Upgrade header is missing or invalid we treat
				%% the response as any other informational response.
				gun:reply(ReplyTo, {gun_inform, self(), stream_ref(State, StreamRef), Status, Headers}),
				handle(Rest, State, CookieStore, EvHandler, EvHandlerState)
			end;
		_ ->
			gun:reply(ReplyTo, {gun_inform, self(), stream_ref(State, StreamRef), Status, Headers}),
			handle(Rest, State, CookieStore, EvHandler, EvHandlerState)
	end.

handle_response(Rest, State=#http_state{version=ClientVersion, opts=Opts, connection=Conn,
		streams=[Stream=#stream{ref=StreamRef, reply_to=ReplyTo, method=Method, is_alive=IsAlive}|Tail]},
		CookieStore, EvHandler, EvHandlerState0, Version, Status, Headers) ->
	In = response_io_from_headers(Method, Version, Status, Headers),
	IsFin = case In of head -> fin; _ -> nofin end,
	RealStreamRef = stream_ref(State, StreamRef),
	%% @todo Figure out whether the event should trigger if the stream was cancelled.
	{Handlers, EvHandlerState2} = case IsAlive of
		false ->
			{undefined, EvHandlerState0};
		true ->
			gun:reply(ReplyTo, {gun_response, self(), RealStreamRef, IsFin, Status, Headers}),
			EvHandlerState1 = EvHandler:response_headers(#{
				stream_ref => RealStreamRef,
				reply_to => ReplyTo,
				status => Status,
				headers => Headers
			}, EvHandlerState0),
			case IsFin of
				fin -> {undefined, EvHandlerState1};
				nofin ->
					Handlers0 = maps:get(content_handlers, Opts, [gun_data_h]),
					{gun_content_handler:init(ReplyTo, RealStreamRef,
						Status, Headers, Handlers0), EvHandlerState1}
			end
	end,
	EvHandlerState3 = case IsFin of
		nofin ->
			EvHandlerState2;
		fin ->
			EvHandler:response_end(#{
				stream_ref => RealStreamRef,
				reply_to => ReplyTo
			}, EvHandlerState2)
	end,
	Conn2 = if
		Conn =:= close -> close;
		Version =:= 'HTTP/1.0' -> close;
		ClientVersion =:= 'HTTP/1.0' -> close;
		true -> conn_from_headers(Version, Headers)
	end,
	%% We always reset in_state even if not chunked.
	if
		IsFin =:= fin, Conn2 =:= close ->
			{close, CookieStore, EvHandlerState3};
		IsFin =:= fin ->
			handle(Rest, end_stream(State#http_state{in=In,
				in_state={0, 0}, connection=Conn2,
				streams=[Stream#stream{handler_state=Handlers}|Tail]}),
				CookieStore, EvHandler, EvHandlerState3);
		Conn2 =:= close ->
			close_streams(State, Tail, closing),
			{CommandOrCommands, CookieStore1, EvHandlerState4} =
				handle(Rest, State#http_state{in=In,
					in_state={0, 0}, connection=Conn2,
					streams=[Stream#stream{handler_state=Handlers}]},
				       CookieStore, EvHandler, EvHandlerState3),
			Commands = if
				is_list(CommandOrCommands) ->
					CommandOrCommands ++ [closing(State)];
				true ->
					[CommandOrCommands, closing(State)]
			end,
			{Commands, CookieStore1, EvHandlerState4};
		true ->
			handle(Rest, State#http_state{in=In,
				in_state={0, 0}, connection=Conn2,
				streams=[Stream#stream{handler_state=Handlers}|Tail]},
				CookieStore, EvHandler, EvHandlerState3)
	end.

%% The state must be first in order to retrieve it when the stream ended.
send_data(<<>>, State, nofin) ->
	[{state, State}, {active, true}];
%% @todo What if we receive data when the HEAD method was used?
send_data(Data, State=#http_state{streams=[Stream=#stream{
		flow=Flow0, is_alive=true, handler_state=Handlers0}|Tail]}, IsFin) ->
	{ok, Dec, Handlers} = gun_content_handler:handle(IsFin, Data, Handlers0),
	Flow = case Flow0 of
		infinity -> infinity;
		_ -> Flow0 - Dec
	end,
	[
		{state, State#http_state{streams=[Stream#stream{flow=Flow, handler_state=Handlers}|Tail]}},
		{active, Flow > 0}
	];
send_data(_, State, _) ->
	[{state, State}, {active, true}].

%% We only update the active state when the current stream is being updated.
update_flow(State=#http_state{streams=[Stream=#stream{ref=StreamRef, flow=Flow0}|Tail]},
		_ReplyTo, StreamRef, Inc) ->
	Flow = case Flow0 of
		infinity -> infinity;
		_ -> Flow0 + Inc
	end,
	[
		{state, State#http_state{streams=[Stream#stream{flow=Flow}|Tail]}},
		{active, Flow > 0}
	];
update_flow(State=#http_state{streams=Streams0}, _ReplyTo, StreamRef, Inc) ->
	Streams = [case Ref of
		StreamRef when Flow =/= infinity ->
			Tuple#stream{flow=Flow + Inc};
		_ ->
			Tuple
	end || Tuple = #stream{ref=Ref, flow=Flow} <- Streams0],
	{state, State#http_state{streams=Streams}}.

%% We can immediately close the connection when there's no streams.
closing(_, #http_state{streams=[]}, _, EvHandlerState) ->
	{close, EvHandlerState};
%% Otherwise we set connection: close (even if the header was not sent)
%% and close any pipelined streams, only keeping the active stream.
closing(Reason, State=#http_state{streams=[LastStream|Tail]}, _, EvHandlerState) ->
	close_streams(State, Tail, {closing, Reason}),
	{[
		{state, State#http_state{connection=close, streams=[LastStream]}},
		closing(State)
	], EvHandlerState}.

closing(#http_state{opts=Opts}) ->
	Timeout = maps:get(closing_timeout, Opts, 15000),
	{closing, Timeout}.

close(Reason, State=#http_state{in=body_close,
		streams=[#stream{ref=StreamRef, reply_to=ReplyTo}|Tail]},
		EvHandler, EvHandlerState) ->
	%% We may have more than one stream in case we somehow close abruptly.
	close_streams(State, Tail, close_reason(Reason)),
	_ = send_data(<<>>, State, fin),
	EvHandler:response_end(#{
		stream_ref => stream_ref(State, StreamRef),
		reply_to => ReplyTo
	}, EvHandlerState);
close(Reason, State=#http_state{streams=Streams}, _, EvHandlerState) ->
	close_streams(State, Streams, close_reason(Reason)),
	EvHandlerState.

close_reason(closed) -> closed;
close_reason(Reason) -> {closed, Reason}.

%% @todo Do we want an event for this?
%%
%% @todo Need to propagate stream closing to tunneled streams.
close_streams(_, [], _) ->
	ok;
close_streams(State, [#stream{is_alive=false}|Tail], Reason) ->
	close_streams(State, Tail, Reason);
close_streams(State, [#stream{ref=StreamRef, reply_to=ReplyTo}|Tail], Reason) ->
	gun:reply(ReplyTo, {gun_error, self(), stream_ref(State, StreamRef), Reason}),
	close_streams(State, Tail, Reason).

%% We don't send a keep-alive when a CONNECT request was initiated.
keepalive(#http_state{streams=[#stream{ref={connect, _, _}}]}, _, EvHandlerState) ->
	{[], EvHandlerState};
%% We can only keep-alive by sending an empty line in-between streams.
keepalive(#http_state{socket=Socket, transport=Transport, out=head}, _, EvHandlerState) ->
	case Transport:send(Socket, <<"\r\n">>) of
		ok -> {[], EvHandlerState};
		Error={error, _} -> {Error, EvHandlerState}
	end;
keepalive(_State, _, EvHandlerState) ->
	{[], EvHandlerState}.

ping(_State, undefined, _ReplyTo, PingRef) ->
	{error, {ping_unsupported_by_protocol, PingRef}}.

headers(State, StreamRef, ReplyTo, _, _, _, _, _, _, CookieStore, _, EvHandlerState)
		when is_list(StreamRef) ->
	gun:reply(ReplyTo, {gun_error, self(), stream_ref(State, StreamRef),
		{badstate, "The stream is not a tunnel."}}),
	{[], CookieStore, EvHandlerState};
headers(State=#http_state{opts=Opts, out=head},
		StreamRef, ReplyTo, Method, Host, Port, Path, Headers,
		InitialFlow0, CookieStore0, EvHandler, EvHandlerState0) ->
	{SendResult, Authority, Conn, Out, CookieStore, EvHandlerState} = send_request(State,
		StreamRef, ReplyTo, Method, Host, Port, Path, Headers, undefined,
		CookieStore0, EvHandler, EvHandlerState0, ?FUNCTION_NAME),
	Command = case SendResult of
		ok ->
			InitialFlow = initial_flow(InitialFlow0, Opts),
			{state, new_stream(State#http_state{connection=Conn, out=Out}, StreamRef,
				ReplyTo, Method, Authority, Path, InitialFlow)};
		Error={error, _} ->
			Error
	end,
	{Command, CookieStore, EvHandlerState}.

request(State, StreamRef, ReplyTo, _, _, _, _, _, _, _, CookieStore, _, EvHandlerState)
		when is_list(StreamRef) ->
	gun:reply(ReplyTo, {gun_error, self(), stream_ref(State, StreamRef),
		{badstate, "The stream is not a tunnel."}}),
	{[], CookieStore, EvHandlerState};
request(State=#http_state{opts=Opts, out=head}, StreamRef, ReplyTo,
		Method, Host, Port, Path, Headers, Body,
		InitialFlow0, CookieStore0, EvHandler, EvHandlerState0) ->
	{SendResult, Authority, Conn, Out, CookieStore, EvHandlerState} = send_request(State,
		StreamRef, ReplyTo, Method, Host, Port, Path, Headers, Body,
		CookieStore0, EvHandler, EvHandlerState0, ?FUNCTION_NAME),
	Command = case SendResult of
		ok ->
			InitialFlow = initial_flow(InitialFlow0, Opts),
			{state, new_stream(State#http_state{connection=Conn, out=Out}, StreamRef,
				ReplyTo, Method, Authority, Path, InitialFlow)};
		Error={error, _} ->
			Error
	end,
	{Command, CookieStore, EvHandlerState}.

initial_flow(infinity, #{flow := InitialFlow}) -> InitialFlow;
initial_flow(InitialFlow, _) -> InitialFlow.

send_request(State=#http_state{socket=Socket, transport=Transport, version=Version},
		StreamRef, ReplyTo, Method, Host, Port, Path, Headers0, Body,
		CookieStore0, EvHandler, EvHandlerState0, Function) ->
	Headers1 = lists:keydelete(<<"transfer-encoding">>, 1, Headers0),
	Headers2 = case Body of
		undefined -> Headers1;
		_ -> lists:keydelete(<<"content-length">>, 1, Headers1)
	end,
	%% We use Headers2 because this is the smallest list.
	Conn = conn_from_headers(Version, Headers2),
	Out = case Body of
		undefined when Function =:= ws_upgrade -> head;
		undefined -> request_io_from_headers(Headers2);
		_ -> head
	end,
	{Authority, Headers3} = case lists:keyfind(<<"host">>, 1, Headers2) of
		false ->
			Authority0 = host_header(Transport:name(), Host, Port),
			{Authority0, [{<<"host">>, Authority0}|Headers2]};
		{_, Authority1} ->
			{Authority1, Headers2}
	end,
	Headers4 = transform_header_names(State, Headers3),
	Headers5 = case {Body, Out} of
		{undefined, body_chunked} when Version =:= 'HTTP/1.0' -> Headers4;
		{undefined, body_chunked} -> [{<<"transfer-encoding">>, <<"chunked">>}|Headers4];
		{undefined, _} -> Headers4;
		_ -> [{<<"content-length">>, integer_to_binary(iolist_size(Body))}|Headers4]
	end,
	{Headers, CookieStore} = gun_cookies:add_cookie_header(
		scheme(State), Authority, Path, Headers5, CookieStore0),
	RealStreamRef = stream_ref(State, StreamRef),
	RequestEvent = #{
		stream_ref => RealStreamRef,
		reply_to => ReplyTo,
		function => Function,
		method => Method,
		authority => Authority,
		path => Path,
		headers => Headers
	},
	EvHandlerState1 = EvHandler:request_start(RequestEvent, EvHandlerState0),
	SendResult = Transport:send(Socket, [
		cow_http:request(Method, Path, Version, Headers),
		[Body || Body =/= undefined]]),
	EvHandlerState2 = EvHandler:request_headers(RequestEvent, EvHandlerState1),
	EvHandlerState = case Out of
		head ->
			RequestEndEvent = #{
				stream_ref => RealStreamRef,
				reply_to => ReplyTo
			},
			EvHandler:request_end(RequestEndEvent, EvHandlerState2);
		_ ->
			EvHandlerState2
	end,
	{SendResult, Authority, Conn, Out, CookieStore, EvHandlerState}.

host_header(TransportName, Host0, Port) ->
	Host = case Host0 of
		{local, _SocketPath} -> <<>>;
		Tuple when tuple_size(Tuple) =:= 8 -> [$[, inet:ntoa(Tuple), $]]; %% IPv6.
		Tuple when tuple_size(Tuple) =:= 4 -> inet:ntoa(Tuple); %% IPv4.
		Atom when is_atom(Atom) -> atom_to_list(Atom);
		_ -> Host0
	end,
	case {TransportName, Port} of
		{tcp, 80} -> Host;
		{tls, 443} -> Host;
		_ -> [Host, $:, integer_to_binary(Port)]
	end.

transform_header_names(#http_state{opts=Opts}, Headers) ->
	case maps:get(transform_header_name, Opts, undefined) of
		undefined -> Headers;
		Fun -> lists:keymap(Fun, 1, Headers)
	end.

scheme(#http_state{transport=Transport}) ->
	case Transport of
		gun_tls -> <<"https">>;
		gun_tls_proxy -> <<"https">>;
		_ -> <<"http">>
	end.

%% We are expecting a new stream.
data(State=#http_state{out=head}, StreamRef, ReplyTo, _, _, _, EvHandlerState) ->
	error_stream_closed(State, StreamRef, ReplyTo),
	{[], EvHandlerState};
%% There are no active streams.
data(State=#http_state{streams=[]}, StreamRef, ReplyTo, _, _, _, EvHandlerState) ->
	error_stream_not_found(State, StreamRef, ReplyTo),
	{[], EvHandlerState};
%% We can only send data on the last created stream.
data(State=#http_state{socket=Socket, transport=Transport, version=Version,
		out=Out, streams=Streams}, StreamRef, ReplyTo, IsFin, Data,
		EvHandler, EvHandlerState0) ->
	case lists:last(Streams) of
		#stream{ref=StreamRef, is_alive=true} ->
			DataLength = iolist_size(Data),
			case Out of
				body_chunked when Version =:= 'HTTP/1.1', IsFin =:= fin ->
					DataToSend = if
						DataLength =:= 0 ->
							cow_http_te:last_chunk();
						true ->
							[
								cow_http_te:chunk(Data),
								cow_http_te:last_chunk()
							]
					end,
					case Transport:send(Socket, DataToSend) of
						ok ->
							RequestEndEvent = #{
								stream_ref => stream_ref(State, StreamRef),
								reply_to => ReplyTo
							},
							EvHandlerState = EvHandler:request_end(RequestEndEvent,
								EvHandlerState0),
							{{state, State#http_state{out=head}}, EvHandlerState};
						Error={error, _} ->
							{Error, EvHandlerState0}
					end;
				body_chunked when Version =:= 'HTTP/1.1' ->
					case Transport:send(Socket, cow_http_te:chunk(Data)) of
						ok -> {[], EvHandlerState0};
						Error={error, _} -> {Error, EvHandlerState0}
					end;
				{body, Length} when DataLength =< Length ->
					Length2 = Length - DataLength,
					case Transport:send(Socket, Data) of
						ok when Length2 =:= 0, IsFin =:= fin ->
							RequestEndEvent = #{
								stream_ref => stream_ref(State, StreamRef),
								reply_to => ReplyTo
							},
							EvHandlerState = EvHandler:request_end(RequestEndEvent, EvHandlerState0),
							{{state, State#http_state{out=head}}, EvHandlerState};
						ok when Length2 > 0, IsFin =:= nofin ->
							{{state, State#http_state{out={body, Length2}}}, EvHandlerState0};
						Error={error, _} ->
							{Error, EvHandlerState0}
					end;
				body_chunked -> %% HTTP/1.0
					case Transport:send(Socket, Data) of
						ok -> {[], EvHandlerState0};
						Error={error, _} -> {Error, EvHandlerState0}
					end
			end;
		_ ->
			error_stream_not_found(State, StreamRef, ReplyTo),
			{[], EvHandlerState0}
	end.

connect(State, StreamRef, ReplyTo, _, _, _, _, CookieStore, _, EvHandlerState)
		when is_list(StreamRef) ->
	gun:reply(ReplyTo, {gun_error, self(), stream_ref(State, StreamRef),
		{badstate, "The stream is not a tunnel."}}),
	{[], CookieStore, EvHandlerState};
connect(State=#http_state{streams=Streams}, StreamRef, ReplyTo, _, _, _, _,
		CookieStore, _, EvHandlerState)
		when Streams =/= [] ->
	gun:reply(ReplyTo, {gun_error, self(), stream_ref(State, StreamRef), {badstate,
		"CONNECT can only be used with HTTP/1.1 when no other streams are active."}}),
	{[], CookieStore, EvHandlerState};
connect(State=#http_state{socket=Socket, transport=Transport, opts=Opts, version=Version},
		StreamRef, ReplyTo, Destination=#{host := Host0}, _TunnelInfo, Headers0, InitialFlow0,
		CookieStore, EvHandler, EvHandlerState0) ->
	Host = case Host0 of
		Tuple when is_tuple(Tuple) -> inet:ntoa(Tuple);
		_ -> Host0
	end,
	Port = maps:get(port, Destination, 1080),
	Authority = [Host, $:, integer_to_binary(Port)],
	Headers1 = lists:keydelete(<<"content-length">>, 1,
		lists:keydelete(<<"transfer-encoding">>, 1, Headers0)),
	Headers2 = case lists:keymember(<<"host">>, 1, Headers1) of
		false -> [{<<"host">>, Authority}|Headers1];
		true -> Headers1
	end,
	HasProxyAuthorization = lists:keymember(<<"proxy-authorization">>, 1, Headers2),
	Headers3 = case {HasProxyAuthorization, Destination} of
		{false, #{username := UserID, password := Password}} ->
			[{<<"proxy-authorization">>, [
					<<"Basic ">>,
					base64:encode(iolist_to_binary([UserID, $:, Password]))]}
				|Headers2];
		_ ->
			Headers2
	end,
	Headers = transform_header_names(State, Headers3),
	RealStreamRef = stream_ref(State, StreamRef),
	RequestEvent = #{
		stream_ref => RealStreamRef,
		reply_to => ReplyTo,
		function => connect,
		method => <<"CONNECT">>,
		authority => Authority,
		headers => Headers
	},
	EvHandlerState1 = EvHandler:request_start(RequestEvent, EvHandlerState0),
	case Transport:send(Socket, cow_http:request(<<"CONNECT">>,
			Authority, Version, Headers)) of
		ok ->
			EvHandlerState2 = EvHandler:request_headers(RequestEvent, EvHandlerState1),
			RequestEndEvent = #{
				stream_ref => RealStreamRef,
				reply_to => ReplyTo
			},
			EvHandlerState = EvHandler:request_end(RequestEndEvent, EvHandlerState2),
			InitialFlow = initial_flow(InitialFlow0, Opts),
			{{state, new_stream(State, {connect, StreamRef, Destination},
				ReplyTo, <<"CONNECT">>, Authority, <<>>, InitialFlow)},
				CookieStore, EvHandlerState};
		Error={error, _} ->
			{Error, CookieStore, EvHandlerState1}
	end.

%% We can't cancel anything, we can just stop forwarding messages to the owner.
cancel(State0, StreamRef, ReplyTo, EvHandler, EvHandlerState0) ->
	case is_stream(State0, StreamRef) of
		true ->
			State = cancel_stream(State0, StreamRef),
			EvHandlerState = EvHandler:cancel(#{
				stream_ref => stream_ref(State, StreamRef),
				reply_to => ReplyTo,
				endpoint => local,
				reason => cancel
			}, EvHandlerState0),
			{{state, State}, EvHandlerState};
		false ->
			error_stream_not_found(State0, StreamRef, ReplyTo),
			{[], EvHandlerState0}
	end.

stream_info(#http_state{streams=Streams}, StreamRef) ->
	case lists:keyfind(StreamRef, #stream.ref, Streams) of
		#stream{reply_to=ReplyTo, is_alive=IsAlive} ->
			{ok, #{
				ref => StreamRef, %% @todo Wrong stream_ref? base_stream_ref it?
				reply_to => ReplyTo,
				state => case IsAlive of
					true -> running;
					false -> stopping
				end
			}};
		false ->
			{ok, undefined}
	end.

down(#http_state{streams=Streams}) ->
	[case Ref of
		{connect, Ref2, _} -> Ref2;
		#websocket{ref=Ref2} -> Ref2;
		_ -> Ref
	end || #stream{ref=Ref} <- Streams].

error_stream_closed(State, StreamRef, ReplyTo) ->
	gun:reply(ReplyTo, {gun_error, self(), stream_ref(State, StreamRef), {badstate,
		"The stream has already been closed."}}),
	ok.

error_stream_not_found(State, StreamRef, ReplyTo) ->
	gun:reply(ReplyTo, {gun_error, self(), stream_ref(State, StreamRef), {badstate,
		"The stream cannot be found."}}),
	ok.

%% Headers information retrieval.

conn_from_headers(Version, Headers) ->
	case lists:keyfind(<<"connection">>, 1, Headers) of
		false when Version =:= 'HTTP/1.0' ->
			close;
		false ->
			keepalive;
		{_, ConnHd} ->
			conn_from_header(cow_http_hd:parse_connection(ConnHd))
	end.

conn_from_header([]) -> close;
conn_from_header([<<"keep-alive">>|_]) -> keepalive;
conn_from_header([<<"upgrade">>|_]) -> keepalive;
conn_from_header([_|Tail]) -> conn_from_header(Tail).

request_io_from_headers(Headers) ->
	case lists:keyfind(<<"content-length">>, 1, Headers) of
		{_, Length} ->
			{body, cow_http_hd:parse_content_length(Length)};
		_ ->
			body_chunked
	end.

response_io_from_headers(<<"HEAD">>, _, _, _) ->
	head;
response_io_from_headers(_, _, Status, _) when (Status =:= 204) or (Status =:= 304) ->
	head;
response_io_from_headers(_, Version, _Status, Headers) ->
	case lists:keyfind(<<"transfer-encoding">>, 1, Headers) of
		{_, TE} when Version =:= 'HTTP/1.1' ->
			case cow_http_hd:parse_transfer_encoding(TE) of
				[<<"chunked">>] -> body_chunked;
				[<<"identity">>] -> body_close
			end;
		_ ->
			case lists:keyfind(<<"content-length">>, 1, Headers) of
				{_, <<"0">>} ->
					head;
				{_, Length} ->
					{body, cow_http_hd:parse_content_length(Length)};
				_ ->
					body_close
			end
	end.

%% Streams.

stream_ref(#http_state{base_stream_ref=undefined}, StreamRef) ->
	stream_ref(StreamRef);
stream_ref(#http_state{base_stream_ref=BaseStreamRef}, StreamRef)
		when is_reference(BaseStreamRef) ->
	[BaseStreamRef, stream_ref(StreamRef)];
stream_ref(#http_state{base_stream_ref=BaseStreamRef}, StreamRef) ->
	BaseStreamRef ++ [stream_ref(StreamRef)].

stream_ref({connect, StreamRef, _}) -> StreamRef;
stream_ref(#websocket{ref=StreamRef}) -> StreamRef;
stream_ref(StreamRef) -> StreamRef.

new_stream(State=#http_state{streams=Streams}, StreamRef, ReplyTo,
		Method, Authority, Path, InitialFlow) ->
	State#http_state{streams=Streams
		++ [#stream{ref=StreamRef, reply_to=ReplyTo, flow=InitialFlow,
			method=iolist_to_binary(Method), authority=Authority,
			path=iolist_to_binary(Path), is_alive=true}]}.

is_stream(#http_state{streams=Streams}, StreamRef) ->
	lists:keymember(StreamRef, #stream.ref, Streams).

cancel_stream(State=#http_state{streams=Streams}, StreamRef) ->
	Streams2 = [case Ref of
		StreamRef ->
			Tuple#stream{is_alive=false};
		_ ->
			Tuple
	end || Tuple = #stream{ref=Ref} <- Streams],
	State#http_state{streams=Streams2}.

end_stream(State=#http_state{streams=[_|Tail]}) ->
	State#http_state{in=head, streams=Tail}.

%% Websocket upgrade.

ws_upgrade(State, StreamRef, ReplyTo, _, _, _, _, _, CookieStore, _, EvHandlerState)
		when is_list(StreamRef) ->
	gun:reply(ReplyTo, {gun_error, self(), stream_ref(State, StreamRef),
		{badstate, "The stream is not a tunnel."}}),
	{[], CookieStore, EvHandlerState};
ws_upgrade(State=#http_state{version='HTTP/1.0'},
		StreamRef, ReplyTo, _, _, _, _, _, CookieStore, _, EvHandlerState) ->
	gun:reply(ReplyTo, {gun_error, self(), stream_ref(State, StreamRef), {badstate,
		"Websocket cannot be used over an HTTP/1.0 connection."}}),
	{[], CookieStore, EvHandlerState};
ws_upgrade(State=#http_state{out=head}, StreamRef, ReplyTo,
		Host, Port, Path, Headers0, WsOpts, CookieStore0, EvHandler, EvHandlerState0) ->
	{Headers1, GunExtensions} = case maps:get(compress, WsOpts, false) of
		true -> {[{<<"sec-websocket-extensions">>,
				<<"permessage-deflate; client_max_window_bits; server_max_window_bits=15">>}
			|Headers0],
			[<<"permessage-deflate">>]};
		false -> {Headers0, []}
	end,
	Headers2 = case maps:get(protocols, WsOpts, []) of
		[] -> Headers1;
		ProtoOpt ->
			<< _, _, Proto/bits >> = iolist_to_binary([[<<", ">>, P] || {P, _} <- ProtoOpt]),
			[{<<"sec-websocket-protocol">>, Proto}|Headers1]
	end,
	Key = cow_ws:key(),
	Headers = [
		{<<"connection">>, <<"upgrade">>},
		{<<"upgrade">>, <<"websocket">>},
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"sec-websocket-key">>, Key}
		|Headers2
	],
	{SendResult, Authority, Conn, Out, CookieStore, EvHandlerState} = send_request(State,
		StreamRef, ReplyTo, <<"GET">>, Host, Port, Path, Headers, undefined,
		CookieStore0, EvHandler, EvHandlerState0, ?FUNCTION_NAME),
	Command = case SendResult of
		ok ->
			InitialFlow = maps:get(flow, WsOpts, infinity),
			{state, new_stream(State#http_state{connection=Conn, out=Out},
				#websocket{ref=StreamRef, reply_to=ReplyTo, key=Key,
					extensions=GunExtensions, opts=WsOpts},
				ReplyTo, <<"GET">>, Authority, Path, InitialFlow)};
		Error={error, _} ->
			Error
	end,
	{Command, CookieStore, EvHandlerState}.

ws_handshake(Buffer, State, Ws=#websocket{key=Key}, Headers) ->
	%% @todo check upgrade, connection
	case lists:keyfind(<<"sec-websocket-accept">>, 1, Headers) of
		false ->
			close;
		{_, Accept} ->
			case cow_ws:encode_key(Key) of
				Accept ->
					ws_handshake_extensions_and_protocol(Buffer, State, Ws, Headers);
				_ ->
					close
			end
	end.

ws_handshake_extensions_and_protocol(Buffer, State,
		Ws=#websocket{extensions=Extensions0, opts=WsOpts}, Headers) ->
	case gun_ws:select_extensions(Headers, Extensions0, WsOpts) of
		close ->
			close;
		Extensions ->
			case gun_ws:select_protocol(Headers, WsOpts) of
				close ->
					close;
				Handler ->
					ws_handshake_end(Buffer, State, Ws, Headers, Extensions, Handler)
			end
	end.

%% We know that the most recent stream is the Websocket one.
ws_handshake_end(Buffer, State=#http_state{streams=[#stream{flow=InitialFlow}|_]},
		#websocket{ref=StreamRef, reply_to=ReplyTo, opts=Opts}, Headers, Extensions, Handler) ->
	%% Inform the user that the upgrade was successful and switch the protocol.
	RealStreamRef = stream_ref(State, StreamRef),
	gun:reply(ReplyTo, {gun_upgrade, self(), RealStreamRef, [<<"websocket">>], Headers}),
	{switch_protocol, {ws, #{
		stream_ref => RealStreamRef,
		headers => Headers,
		extensions => Extensions,
		flow => InitialFlow,
		handler => Handler,
		opts => Opts
	}}, ReplyTo, Buffer}.

%% Reject attempts to send Websocket frames on HTTP.
%% This can happen either because the user explicitly
%% called ws_send on an HTTP connection, or because
%% the connection was dropped and automatically
%% reconnected. Automatic reconnection doesn't perform
%% a Websocket upgrade automatically.
ws_send(_, _, _, ReplyTo, _, EvHandlerState) ->
	gun:reply(ReplyTo, {gun_error, self(), {badstate,
		"This connection does not currently accept Websocket frames. "
		"The Websocket upgrade was not performed. "
		"This may happen as a result of automatic reconnect."}}),
	{[], EvHandlerState}.
