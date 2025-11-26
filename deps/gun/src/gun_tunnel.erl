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

%% This module is used when a tunnel is established and either
%% StreamRef dereference or a TLS proxy process must be handled
%% by the tunnel layer.
-module(gun_tunnel).

-export([init/6]).
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
-export([tunneled_name/2]).
-export([down/1]).
-export([ws_upgrade/11]).
-export([ws_send/6]).

-record(tunnel_state, {
	%% Fake socket and transport.
	%% We accept 'undefined' only to simplify the init code.
	socket = undefined :: #{
		gun_pid := pid(),
		reply_to := gun:reply_to(),
		stream_ref := gun:stream_ref(),
		handle_continue_stream_ref := gun:stream_ref()
	} | pid() | undefined,
	transport = undefined :: gun_tcp_proxy | gun_tls_proxy | undefined,

	%% The stream_ref from which the stream was created. When
	%% the tunnel exists as a result of HTTP/2 CONNECT -> HTTP/1.1 CONNECT
	%% the stream_ref is the same as the HTTP/1.1 CONNECT one.
	stream_ref = undefined :: gun:stream_ref(),

	%% The pid we send messages to.
	reply_to = undefined :: pid(),

	%% When the tunnel is a 'connect' tunnel we must dereference the
	%% stream_ref. When it is 'socks' we must not as there was no
	%% stream involved in creating the tunnel.
	type = undefined :: connect | socks5,

	%% Transport and protocol name of the tunnel layer.
	tunnel_transport = undefined :: tcp | tls,
	tunnel_protocol = undefined :: http | http2 | socks,

	%% Tunnel information.
	info = undefined :: gun:tunnel_info(),

	%% The origin socket of the TLS proxy, if any. This is used to forward
	%% messages to the proxy process in order to decrypt the data.
	tls_origin_socket = undefined :: undefined | #{
		gun_pid := pid(),
		reply_to := pid(),
		stream_ref := gun:stream_ref(),
		handle_continue_stream_ref => gun:stream_ref()
	},

	opts = undefined :: undefined | any(), %% @todo Opts type.

	%% Protocol module and state of the outer layer. Only initialized
	%% after the TLS handshake has completed when TLS is involved.
	protocol = undefined :: module(),
	protocol_state = undefined :: any(),

	%% When the protocol is being switched the origin may change.
	%% We keep the new information to provide it in TunnelInfo of
	%% the new protocol when the switch occurs.
	protocol_origin = undefined :: undefined
		| {origin, binary(), inet:hostname() | inet:ip_address(), inet:port_number(), connect | socks5}
}).

%% Socket is the "origin socket" and Transport the "origin transport".
%% When the Transport indicate a TLS handshake was requested, the socket
%% and transport are given to the intermediary TLS proxy process.
%%
%% Opts is the options for the underlying HTTP/2 connection,
%% with some extra information added for the tunnel.
%%
%% @todo Mark the tunnel options as reserved.
init(ReplyTo, OriginSocket, OriginTransport, Opts=#{stream_ref := StreamRef, tunnel := Tunnel},
		EvHandler, EvHandlerState0) ->
	#{
		type := TunnelType,
		transport_name := TunnelTransport,
		protocol_name := TunnelProtocol,
		info := TunnelInfo
	} = Tunnel,
	State = #tunnel_state{stream_ref=StreamRef, reply_to=ReplyTo, type=TunnelType,
		tunnel_transport=TunnelTransport, tunnel_protocol=TunnelProtocol,
		info=TunnelInfo, opts=maps:without([stream_ref, tunnel], Opts)},
	case Tunnel of
		%% Initialize the protocol.
		#{new_protocol := NewProtocol} ->
			{Proto, ProtoOpts} = gun_protocols:handler_and_opts(NewProtocol, Opts),
			case Proto:init(ReplyTo, OriginSocket, OriginTransport,
					ProtoOpts#{stream_ref => StreamRef, tunnel_transport => tcp}) of
				{ok, _, ProtoState} ->
					EvHandlerState = EvHandler:protocol_changed(#{
						stream_ref => StreamRef,
						protocol => Proto:name()
					}, EvHandlerState0),
					%% When the tunnel protocol is HTTP/1.1 or SOCKS
					%% the gun_tunnel_up message was already sent.
					_ = case TunnelProtocol of
						http -> ok;
						socks -> ok;
						_ -> gun:reply(ReplyTo, {gun_tunnel_up, self(), StreamRef, Proto:name()})
					end,
					{tunnel, State#tunnel_state{socket=OriginSocket, transport=OriginTransport,
						protocol=Proto, protocol_state=ProtoState},
						EvHandlerState};
				Error={error, _} ->
					Error
			end;
		%% We can't initialize the protocol until the TLS handshake has completed.
		#{handshake_event := HandshakeEvent0, protocols := Protocols} ->
			#{handle_continue_stream_ref := ContinueStreamRef} = OriginSocket,
			#{
				origin_host := DestHost,
				origin_port := DestPort
			} = TunnelInfo,
			#{
				tls_opts := TLSOpts,
				timeout := TLSTimeout
			} = HandshakeEvent0,
			HandshakeEvent = HandshakeEvent0#{socket => OriginSocket},
			EvHandlerState = EvHandler:tls_handshake_start(HandshakeEvent, EvHandlerState0),
			{ok, ProxyPid} = gun_tls_proxy:start_link(DestHost, DestPort,
				TLSOpts, TLSTimeout, OriginSocket, gun_tls_proxy_http2_connect,
				{handle_continue, ContinueStreamRef, HandshakeEvent, Protocols}),
			{tunnel, State#tunnel_state{socket=ProxyPid, transport=gun_tls_proxy,
				tls_origin_socket=OriginSocket}, EvHandlerState}
	end.

%% When we receive data we pass it forward directly for TCP;
%% or we decrypt it and pass it via handle_continue for TLS.
handle(Data, State=#tunnel_state{transport=gun_tcp_proxy,
		protocol=Proto, protocol_state=ProtoState0},
		CookieStore0, EvHandler, EvHandlerState0) ->
	{Commands, CookieStore, EvHandlerState1} = Proto:handle(
		Data, ProtoState0, CookieStore0, EvHandler, EvHandlerState0),
	{ResCommands, EvHandlerState} = commands(Commands, State, EvHandler, EvHandlerState1),
	{ResCommands, CookieStore, EvHandlerState};
handle(Data, State=#tunnel_state{transport=gun_tls_proxy,
		socket=ProxyPid, tls_origin_socket=OriginSocket},
		CookieStore, _EvHandler, EvHandlerState) ->
	%% When we receive a DATA frame that contains TLS-encoded data,
	%% we must first forward it to the ProxyPid to be decoded. The
	%% Gun process will receive it back as a tls_proxy_http2_connect
	%% message and forward it to the right stream via the handle_continue
	%% callback.
	ProxyPid ! {tls_proxy_http2_connect, OriginSocket, Data},
	{{state, State}, CookieStore, EvHandlerState}.

%% This callback will only be called for TLS.
%%
%% The StreamRef in this callback is special because it includes
%% a reference() for Socks layers as well.
handle_continue(ContinueStreamRef, {gun_tls_proxy, ProxyPid, {ok, Negotiated},
		{handle_continue, _, HandshakeEvent, Protocols}},
		State=#tunnel_state{socket=ProxyPid, stream_ref=StreamRef, opts=Opts},
		CookieStore, EvHandler, EvHandlerState0)
		when is_reference(ContinueStreamRef) ->
	#{reply_to := ReplyTo} = HandshakeEvent,
	NewProtocol = gun_protocols:negotiated(Negotiated, Protocols),
	{Proto, ProtoOpts} = gun_protocols:handler_and_opts(NewProtocol, Opts),
	EvHandlerState1 = EvHandler:tls_handshake_end(HandshakeEvent#{
		socket => ProxyPid,
		protocol => Proto:name()
	}, EvHandlerState0),
	EvHandlerState = EvHandler:protocol_changed(#{
		stream_ref => StreamRef,
		protocol => Proto:name()
	}, EvHandlerState1),
	%% @todo Terminate the current protocol or something?
	OriginSocket = #{
		gun_pid => self(),
		reply_to => ReplyTo,
		stream_ref => StreamRef
	},
	case Proto:init(ReplyTo, OriginSocket, gun_tcp_proxy,
			ProtoOpts#{stream_ref => StreamRef, tunnel_transport => tls}) of
		{ok, _, ProtoState} ->
			gun:reply(ReplyTo, {gun_tunnel_up, self(), StreamRef, Proto:name()}),
			{{state, State#tunnel_state{protocol=Proto, protocol_state=ProtoState}},
				CookieStore, EvHandlerState};
		Error={error, _} ->
			{Error, CookieStore, EvHandlerState}
	end;
handle_continue(ContinueStreamRef, {gun_tls_proxy, ProxyPid, {error, Reason},
		{handle_continue, _, HandshakeEvent, _}},
		#tunnel_state{socket=ProxyPid}, CookieStore, EvHandler, EvHandlerState0)
		when is_reference(ContinueStreamRef) ->
	EvHandlerState = EvHandler:tls_handshake_end(HandshakeEvent#{
		error => Reason
	}, EvHandlerState0),
%% @todo
%%   The TCP connection can be closed by either peer.  The END_STREAM flag
%%   on a DATA frame is treated as being equivalent to the TCP FIN bit.  A
%%   client is expected to send a DATA frame with the END_STREAM flag set
%%   after receiving a frame bearing the END_STREAM flag.  A proxy that
%%   receives a DATA frame with the END_STREAM flag set sends the attached
%%   data with the FIN bit set on the last TCP segment.  A proxy that
%%   receives a TCP segment with the FIN bit set sends a DATA frame with
%%   the END_STREAM flag set.  Note that the final TCP segment or DATA
%%   frame could be empty.
	{{error, Reason}, CookieStore, EvHandlerState};
%% Send the data. This causes TLS to encrypt the data and send it to the inner layer.
handle_continue(ContinueStreamRef, {data, _ReplyTo, _StreamRef, IsFin, Data},
		#tunnel_state{}, CookieStore, _EvHandler, EvHandlerState)
		when is_reference(ContinueStreamRef) ->
	{{send, IsFin, Data}, CookieStore, EvHandlerState};
handle_continue(ContinueStreamRef, {tls_proxy, ProxyPid, Data},
		State=#tunnel_state{socket=ProxyPid, protocol=Proto, protocol_state=ProtoState},
		CookieStore0, EvHandler, EvHandlerState0)
		when is_reference(ContinueStreamRef) ->
	{Commands, CookieStore, EvHandlerState1} = Proto:handle(
		Data, ProtoState, CookieStore0, EvHandler, EvHandlerState0),
	{ResCommands, EvHandlerState} = commands(Commands, State, EvHandler, EvHandlerState1),
	{ResCommands, CookieStore, EvHandlerState};
handle_continue(ContinueStreamRef, {tls_proxy_closed, ProxyPid},
		#tunnel_state{socket=ProxyPid}, CookieStore, _EvHandler, EvHandlerState0)
		when is_reference(ContinueStreamRef) ->
	%% @todo All sub-streams must produce a stream_error.
	{{error, closed}, CookieStore, EvHandlerState0};
handle_continue(ContinueStreamRef, {tls_proxy_error, ProxyPid, Reason},
		#tunnel_state{socket=ProxyPid}, CookieStore, _EvHandler, EvHandlerState0)
		when is_reference(ContinueStreamRef) ->
	%% @todo All sub-streams must produce a stream_error.
	{{error, Reason}, CookieStore, EvHandlerState0};
%% We always dereference the ContinueStreamRef because it includes a
%% reference() for Socks layers too.
%%
%% @todo Assert StreamRef to be our reference().
handle_continue([_StreamRef|ContinueStreamRef0], Msg,
		State=#tunnel_state{protocol=Proto, protocol_state=ProtoState},
		CookieStore0, EvHandler, EvHandlerState0) ->
	ContinueStreamRef = case ContinueStreamRef0 of
		[CSR] -> CSR;
		_ -> ContinueStreamRef0
	end,
	{Commands, CookieStore, EvHandlerState1} = Proto:handle_continue(
		ContinueStreamRef, Msg, ProtoState, CookieStore0, EvHandler, EvHandlerState0),
	{ResCommands, EvHandlerState} = commands(Commands, State, EvHandler, EvHandlerState1),
	{ResCommands, CookieStore, EvHandlerState}.

%% @todo This function will need EvHandler/EvHandlerState?
update_flow(State=#tunnel_state{protocol=Proto, protocol_state=ProtoState},
		ReplyTo, StreamRef0, Inc) ->
	StreamRef = maybe_dereference(State, StreamRef0),
	Commands = Proto:update_flow(ProtoState, ReplyTo, StreamRef, Inc),
	{ResCommands, undefined} = commands(Commands, State, undefined, undefined),
	ResCommands.

closing(_Reason, _State, _EvHandler, EvHandlerState) ->
	%% @todo Graceful shutdown must be propagated to tunnels.
	{[], EvHandlerState}.

close(_Reason, _State, _EvHandler, EvHandlerState) ->
	%% @todo Closing must be propagated to tunnels.
	EvHandlerState.

keepalive(_State, _EvHandler, EvHandlerState) ->
	%% @todo Need to figure out how to handle keepalive for tunnels.
	{[], EvHandlerState}.

ping(State=#tunnel_state{protocol=Proto, protocol_state=ProtoState0},
		TunnelRef0, ReplyTo, PingRef) ->
	TunnelRef = case maybe_dereference(State, TunnelRef0) of
		[] -> undefined;
		TunnelRef1 -> TunnelRef1
	end,
	case Proto:ping(ProtoState0, TunnelRef, ReplyTo, PingRef) of
		{state, ProtoState} ->
			{state, State#tunnel_state{protocol_state=ProtoState}};
		Error = {error, _} ->
			Error
	end.

%% We pass the headers forward and optionally dereference StreamRef.
headers(State=#tunnel_state{protocol=Proto, protocol_state=ProtoState0},
		StreamRef0, ReplyTo, Method, Host, Port, Path, Headers,
		InitialFlow, CookieStore0, EvHandler, EvHandlerState0) ->
	StreamRef = maybe_dereference(State, StreamRef0),
	{Commands, CookieStore, EvHandlerState1} = Proto:headers(ProtoState0, StreamRef,
		ReplyTo, Method, Host, Port, Path, Headers,
		InitialFlow, CookieStore0, EvHandler, EvHandlerState0),
	{ResCommands, EvHandlerState} = commands(Commands, State, EvHandler, EvHandlerState1),
	{ResCommands, CookieStore, EvHandlerState}.

%% We pass the request forward and optionally dereference StreamRef.
request(State=#tunnel_state{protocol=Proto, protocol_state=ProtoState0,
		info=#{origin_host := OriginHost, origin_port := OriginPort}},
		StreamRef0, ReplyTo, Method, _Host, _Port, Path, Headers, Body,
		InitialFlow, CookieStore0, EvHandler, EvHandlerState0) ->
	StreamRef = maybe_dereference(State, StreamRef0),
	{Commands, CookieStore, EvHandlerState1} = Proto:request(ProtoState0, StreamRef,
		ReplyTo, Method, OriginHost, OriginPort, Path, Headers, Body,
		InitialFlow, CookieStore0, EvHandler, EvHandlerState0),
	{ResCommands, EvHandlerState} = commands(Commands, State, EvHandler, EvHandlerState1),
	{ResCommands, CookieStore, EvHandlerState}.

%% When the next tunnel is SOCKS we pass the data forward directly.
%% This is needed because SOCKS has no StreamRef and the data cannot
%% therefore be passed forward through the usual method.
data(State=#tunnel_state{protocol=Proto, protocol_state=ProtoState0,
		protocol_origin={origin, _, _, _, socks5}},
		StreamRef, ReplyTo, IsFin, Data, EvHandler, EvHandlerState0) ->
	{Commands, EvHandlerState1} = Proto:data(ProtoState0, StreamRef,
		ReplyTo, IsFin, Data, EvHandler, EvHandlerState0),
	{ResCommands, EvHandlerState} = commands(Commands, State, EvHandler, EvHandlerState1),
	{ResCommands, EvHandlerState};
%% CONNECT tunnels pass the data forward and dereference StreamRef
%% unless they are the recipient of the callback, in which case the
%% data is sent to the socket.
data(State=#tunnel_state{socket=Socket, transport=Transport,
		stream_ref=TunnelStreamRef0, protocol=Proto, protocol_state=ProtoState0},
		StreamRef0, ReplyTo, IsFin, Data, EvHandler, EvHandlerState0) ->
	TunnelStreamRef = outer_stream_ref(TunnelStreamRef0),
	case StreamRef0 of
		TunnelStreamRef ->
			case Transport:send(Socket, Data) of
				ok -> {[], EvHandlerState0};
				Error={error, _} -> {Error, EvHandlerState0}
			end;
		_ ->
			StreamRef = maybe_dereference(State, StreamRef0),
			{Commands, EvHandlerState1} = Proto:data(ProtoState0, StreamRef,
				ReplyTo, IsFin, Data, EvHandler, EvHandlerState0),
			{ResCommands, EvHandlerState} = commands(Commands, State,
				EvHandler, EvHandlerState1),
			{ResCommands, EvHandlerState}
	end.

%% We pass the CONNECT request forward and optionally dereference StreamRef.
connect(State=#tunnel_state{info=#{origin_host := Host, origin_port := Port},
		protocol=Proto, protocol_state=ProtoState0},
		StreamRef0, ReplyTo, Destination, _, Headers, InitialFlow,
		CookieStore0, EvHandler, EvHandlerState0) ->
	StreamRef = maybe_dereference(State, StreamRef0),
	{Commands, CookieStore, EvHandlerState1} = Proto:connect(ProtoState0, StreamRef,
		ReplyTo, Destination, #{host => Host, port => Port}, Headers, InitialFlow,
		CookieStore0, EvHandler, EvHandlerState0),
	{ResCommands, EvHandlerState} = commands(Commands, State, EvHandler, EvHandlerState1),
	{ResCommands, CookieStore, EvHandlerState}.

cancel(State=#tunnel_state{protocol=Proto, protocol_state=ProtoState0},
		StreamRef0, ReplyTo, EvHandler, EvHandlerState0) ->
	StreamRef = maybe_dereference(State, StreamRef0),
	{Commands, EvHandlerState1} = Proto:cancel(ProtoState0, StreamRef,
		ReplyTo, EvHandler, EvHandlerState0),
	{ResCommands, EvHandlerState} = commands(Commands, State, EvHandler, EvHandlerState1),
	{ResCommands, EvHandlerState}.

timeout(State=#tunnel_state{protocol=Proto, protocol_state=ProtoState0}, Msg, TRef) ->
	case Proto:timeout(ProtoState0, Msg, TRef) of
		{state, ProtoState} ->
			{state, State#tunnel_state{protocol_state=ProtoState}};
		Other ->
			Other
	end.

stream_info(#tunnel_state{transport=Transport0, stream_ref=TunnelStreamRef, reply_to=ReplyTo,
		tunnel_protocol=TunnelProtocol,
		info=#{origin_host := OriginHost, origin_port := OriginPort},
		protocol=Proto, protocol_state=ProtoState}, StreamRef)
		when is_reference(StreamRef), TunnelProtocol =/= socks ->
	Transport = case Transport0 of
		gun_tcp_proxy -> tcp;
		gun_tls_proxy -> tls
	end,
	Protocol = case Proto of
		gun_tunnel -> Proto:tunneled_name(ProtoState, false);
		_ -> Proto:name()
	end,
	{ok, #{
		ref => TunnelStreamRef,
		reply_to => ReplyTo,
		state => running,
		tunnel => #{
			transport => Transport,
			protocol => Protocol,
			origin_scheme => case {Transport, Protocol} of
				{_, raw} -> undefined;
				{tcp, _} -> <<"http">>;
				{tls, _} -> <<"https">>
			end,
			origin_host => OriginHost,
			origin_port => OriginPort
		}
	}};
stream_info(State=#tunnel_state{type=Type,
		tunnel_transport=IntermediaryTransport, tunnel_protocol=IntermediaryProtocol,
		info=TunnelInfo, protocol=Proto, protocol_state=ProtoState}, StreamRef0) ->
	StreamRef = maybe_dereference(State, StreamRef0),
	case Proto:stream_info(ProtoState, StreamRef) of
		{ok, undefined} ->
			{ok, undefined};
		{ok, Info} ->
			#{
				host := IntermediateHost,
				port := IntermediatePort
			} = TunnelInfo,
			IntermediaryInfo = #{
				type => Type,
				host => IntermediateHost,
				port => IntermediatePort,
				transport => IntermediaryTransport,
				protocol => IntermediaryProtocol
			},
			Intermediaries = maps:get(intermediaries, Info, []),
			{ok, Info#{
				intermediaries => [IntermediaryInfo|Intermediaries]
			}}
	end.

tunneled_name(#tunnel_state{protocol=Proto=gun_tunnel, protocol_state=ProtoState}, true) ->
	Proto:tunneled_name(ProtoState, false);
tunneled_name(#tunnel_state{tunnel_protocol=TunnelProto}, false) ->
	TunnelProto;
tunneled_name(#tunnel_state{protocol=Proto}, _) ->
	Proto:name().

down(_State) ->
	%% @todo Tunnels must be included in the gun_down message.
	[].

ws_upgrade(State=#tunnel_state{info=TunnelInfo, protocol=Proto, protocol_state=ProtoState0},
		StreamRef0, ReplyTo, _, _, Path, Headers, WsOpts,
		CookieStore0, EvHandler, EvHandlerState0) ->
	StreamRef = maybe_dereference(State, StreamRef0),
	#{
		origin_host := Host,
		origin_port := Port
	} = TunnelInfo,
	{Commands, CookieStore, EvHandlerState1} = Proto:ws_upgrade(ProtoState0, StreamRef, ReplyTo,
		Host, Port, Path, Headers, WsOpts,
		CookieStore0, EvHandler, EvHandlerState0),
	{ResCommands, EvHandlerState} = commands(Commands, State, EvHandler, EvHandlerState1),
	{ResCommands, CookieStore, EvHandlerState}.

ws_send(Frames, State=#tunnel_state{protocol=Proto, protocol_state=ProtoState},
		StreamRef0, ReplyTo, EvHandler, EvHandlerState0) ->
	StreamRef = maybe_dereference(State, StreamRef0),
	{Commands, EvHandlerState1} = Proto:ws_send(Frames,
		ProtoState, StreamRef, ReplyTo, EvHandler, EvHandlerState0),
	{ResCommands, EvHandlerState} = commands(Commands, State, EvHandler, EvHandlerState1),
	{ResCommands, EvHandlerState}.

%% Internal.

%% Returns an error on send errors, a state otherwise
commands(Command, State, EvHandler, EvHandlerState) when not is_list(Command) ->
	commands([Command], State, EvHandler, EvHandlerState);
commands([], State, _, EvHandlerState) ->
	{{state, State}, EvHandlerState};
commands([Error = {error, _}|_],
		State=#tunnel_state{socket=Socket, transport=Transport},
		_, EvHandlerState) ->
	%% We must terminate the TLS proxy pid if any.
	case Transport of
		gun_tls_proxy -> gun_tls_proxy:close(Socket);
		_ -> ok
	end,
	{[{state, State}, Error], EvHandlerState};
commands([{state, ProtoState}|Tail], State, EvHandler, EvHandlerState) ->
	commands(Tail, State#tunnel_state{protocol_state=ProtoState}, EvHandler, EvHandlerState);
%% @todo What to do about IsFin?
commands([{send, _IsFin, Data}|Tail],
		State=#tunnel_state{socket=Socket, transport=Transport},
		EvHandler, EvHandlerState) ->
	case Transport:send(Socket, Data) of
		ok -> commands(Tail, State, EvHandler, EvHandlerState);
		Error={error, _} -> {Error, EvHandlerState}
	end;
commands([Origin={origin, Scheme, Host, Port, Type}|Tail],
		State=#tunnel_state{stream_ref=StreamRef},
		EvHandler, EvHandlerState0) ->
	EvHandlerState = EvHandler:origin_changed(#{
		stream_ref => StreamRef,
		type => Type,
		origin_scheme => Scheme,
		origin_host => Host,
		origin_port => Port
	}, EvHandlerState0),
	commands(Tail, State#tunnel_state{protocol_origin=Origin}, EvHandler, EvHandlerState);
commands([{switch_protocol, NewProtocol, ReplyTo, <<>>}|Tail],
		State=#tunnel_state{socket=Socket, transport=Transport, opts=Opts,
		protocol_origin=undefined},
		EvHandler, EvHandlerState0) ->
	{Proto, ProtoOpts} = gun_protocols:handler_and_opts(NewProtocol, Opts),
	%% This should only apply to Websocket for the time being.
	case Proto:init(ReplyTo, Socket, Transport, ProtoOpts) of
		{ok, connected_ws_only, ProtoState} ->
			#{stream_ref := StreamRef} = ProtoOpts,
			EvHandlerState = EvHandler:protocol_changed(#{
				stream_ref => StreamRef,
				protocol => Proto:name()
			}, EvHandlerState0),
			commands(Tail, State#tunnel_state{protocol=Proto, protocol_state=ProtoState},
				EvHandler, EvHandlerState);
		Error={error, _} ->
			{Error, EvHandlerState0}
	end;
commands([{switch_protocol, NewProtocol, ReplyTo, <<>>}|Tail],
		State=#tunnel_state{transport=Transport, stream_ref=TunnelStreamRef,
		info=#{origin_host := Host, origin_port := Port}, opts=Opts, protocol=CurrentProto,
		protocol_origin={origin, _Scheme, OriginHost, OriginPort, Type}},
		EvHandler, EvHandlerState0) ->
	StreamRef = case Type of
		socks5 -> TunnelStreamRef;
		connect -> gun_protocols:stream_ref(NewProtocol)
	end,
	ContinueStreamRef0 = continue_stream_ref(State),
	ContinueStreamRef = case Type of
		socks5 -> ContinueStreamRef0 ++ [make_ref()];
		connect -> ContinueStreamRef0 ++ [lists:last(StreamRef)]
	end,
	OriginSocket = #{
		gun_pid => self(),
		reply_to => ReplyTo,
		stream_ref => StreamRef,
		handle_continue_stream_ref => ContinueStreamRef
	},
	ProtoOpts = Opts#{
		stream_ref => StreamRef,
		tunnel => #{
			type => Type,
			transport_name => case Transport of
				gun_tcp_proxy -> tcp;
				gun_tls_proxy -> tls
			end,
			protocol_name => CurrentProto:name(),
			info => #{
				host => Host,
				port => Port,
				origin_host => OriginHost,
				origin_port => OriginPort
			},
			new_protocol => NewProtocol
		}
	},
	Proto = gun_tunnel,
	case Proto:init(ReplyTo, OriginSocket, gun_tcp_proxy, ProtoOpts, EvHandler, EvHandlerState0) of
		{tunnel, ProtoState, EvHandlerState} ->
			commands(Tail, State#tunnel_state{protocol=Proto, protocol_state=ProtoState},
				EvHandler, EvHandlerState);
		Error={error, _} ->
			{Error, EvHandlerState0}
	end;
commands([{tls_handshake, HandshakeEvent0, Protocols, ReplyTo}|Tail],
		State=#tunnel_state{transport=Transport,
		info=#{origin_host := Host, origin_port := Port}, opts=Opts, protocol=CurrentProto,
		protocol_origin={origin, _Scheme, OriginHost, OriginPort, Type}},
		EvHandler, EvHandlerState0) ->
	#{
		stream_ref := StreamRef,
		tls_opts := TLSOpts0
	} = HandshakeEvent0,
	TLSOpts = gun:ensure_tls_opts(Protocols, TLSOpts0, OriginHost),
	HandshakeEvent = HandshakeEvent0#{
		tls_opts => TLSOpts
	},
	ContinueStreamRef0 = continue_stream_ref(State),
	ContinueStreamRef = case Type of
		socks5 -> ContinueStreamRef0 ++ [make_ref()];
		connect -> ContinueStreamRef0 ++ [lists:last(StreamRef)]
	end,
	OriginSocket = #{
		gun_pid => self(),
		reply_to => ReplyTo,
		stream_ref => StreamRef,
		handle_continue_stream_ref => ContinueStreamRef
	},
	ProtoOpts = Opts#{
		stream_ref => StreamRef,
		tunnel => #{
			type => Type,
			transport_name => case Transport of
				gun_tcp_proxy -> tcp;
				gun_tls_proxy -> tls
			end,
			protocol_name => CurrentProto:name(),
			info => #{
				host => Host,
				port => Port,
				origin_host => OriginHost,
				origin_port => OriginPort
			},
			handshake_event => HandshakeEvent,
			protocols => Protocols
		}
	},
	Proto = gun_tunnel,
	case Proto:init(ReplyTo, OriginSocket, gun_tcp_proxy, ProtoOpts, EvHandler, EvHandlerState0) of
		{tunnel, ProtoState, EvHandlerState} ->
			commands(Tail, State#tunnel_state{protocol=Proto, protocol_state=ProtoState},
				EvHandler, EvHandlerState);
		Error={error, _} ->
			{Error, EvHandlerState0}
	end;
commands([{active, true}|Tail], State, EvHandler, EvHandlerState) ->
	commands(Tail, State, EvHandler, EvHandlerState).

continue_stream_ref(#tunnel_state{socket=#{handle_continue_stream_ref := ContinueStreamRef}}) ->
	if
		is_list(ContinueStreamRef) -> ContinueStreamRef;
		true -> [ContinueStreamRef]
	end;
continue_stream_ref(#tunnel_state{tls_origin_socket=#{handle_continue_stream_ref := ContinueStreamRef}}) ->
	if
		is_list(ContinueStreamRef) -> ContinueStreamRef;
		true -> [ContinueStreamRef]
	end.

maybe_dereference(#tunnel_state{stream_ref=RealStreamRef, type=connect}, [StreamRef|Tail]) ->
	%% We ensure that the stream_ref is correct.
	StreamRef = outer_stream_ref(RealStreamRef),
	case Tail of
		[Ref] -> Ref;
		_ -> Tail
	end;
maybe_dereference(#tunnel_state{type=socks5}, StreamRef) ->
	StreamRef.

outer_stream_ref(StreamRef) when is_list(StreamRef) ->
	lists:last(StreamRef);
outer_stream_ref(StreamRef) ->
	StreamRef.
