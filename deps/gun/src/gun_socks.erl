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

-module(gun_socks).

-export([check_options/1]).
-export([name/0]).
-export([opts_name/0]).
-export([has_keepalive/0]).
-export([init/4]).
-export([switch_transport/3]).
-export([handle/5]).
-export([closing/4]).
-export([close/4]).
%% @todo down

-record(socks_state, {
	ref :: undefined | gun:stream_ref(),
	reply_to :: gun:reply_to(),
	socket :: inet:socket() | ssl:sslsocket(),
	transport :: module(),
	opts = #{} :: gun:socks_opts(),
	%% We only support version 5 at the moment.
	version :: 5,
	status :: auth_method_select | auth_username_password | connect
}).

check_options(Opts=#{host := _, port := _}) ->
	do_check_options(maps:to_list(maps:without([host, port], Opts)));
%% Host and port are not optional.
check_options(#{host := _}) ->
	{error, options, {socks, port}};
check_options(#{}) ->
	{error, options, {socks, host}}.

do_check_options([]) ->
	ok;
do_check_options([Opt={auth, L}|Opts]) ->
	case check_auth_opt(L) of
		ok -> do_check_options(Opts);
		error -> {error, {options, {socks, Opt}}}
	end;
%% @todo Proper protocols check.
do_check_options([{protocols, P}|Opts]) when is_list(P) ->
	do_check_options(Opts);
do_check_options([{tls_handshake_timeout, infinity}|Opts]) ->
	do_check_options(Opts);
do_check_options([{tls_handshake_timeout, T}|Opts]) when is_integer(T), T >= 0 ->
	do_check_options(Opts);
do_check_options([{tls_opts, L}|Opts]) when is_list(L) ->
	do_check_options(Opts);
do_check_options([{transport, T}|Opts]) when T =:= tcp; T =:= tls ->
	do_check_options(Opts);
do_check_options([{version, 5}|Opts]) ->
	do_check_options(Opts);
do_check_options([Opt|_]) ->
	{error, {options, {socks, Opt}}}.

check_auth_opt(Methods) ->
	%% Methods must not appear more than once, and they
	%% must be one of none or {username_password, binary(), binary()}.
	Check = lists:usort([case M of
		none -> ok;
		{username_password, U, P} when is_binary(U), is_binary(P) -> ok
	end || M <- Methods]),
	case {length(Methods) =:= length(Check), lists:usort(Check)} of
		{true, []} -> ok;
		{true, [ok]} -> ok;
		_ -> error
	end.

name() -> socks.
opts_name() -> socks_opts.
has_keepalive() -> false.

init(ReplyTo, Socket, Transport, Opts) ->
	StreamRef = maps:get(stream_ref, Opts, undefined),
	5 = Version = maps:get(version, Opts, 5),
	Auth = maps:get(auth, Opts, [none]),
	Methods = <<case A of
		{username_password, _, _} -> <<2>>;
		none -> <<0>>
	end || A <- Auth>>,
	case Transport:send(Socket, [<<5, (length(Auth))>>, Methods]) of
		ok ->
			{ok, connected_no_input, #socks_state{ref=StreamRef, reply_to=ReplyTo,
				socket=Socket, transport=Transport,
				opts=Opts, version=Version, status=auth_method_select}};
		Error={error, _Reason} ->
			Error
	end.

switch_transport(Transport, Socket, State) ->
	State#socks_state{socket=Socket, transport=Transport}.

handle(Data, State, CookieStore, _, EvHandlerState) ->
	{handle(Data, State), CookieStore, EvHandlerState}.

%% No authentication.
handle(<<5, 0>>, State=#socks_state{version=5, status=auth_method_select}) ->
	case send_socks5_connect(State) of
		ok -> {state, State#socks_state{status=connect}};
		Error={error, _} -> Error
	end;
%% Username/password authentication.
handle(<<5, 2>>, State=#socks_state{socket=Socket, transport=Transport, opts=#{auth := AuthMethods},
		version=5, status=auth_method_select}) ->
	[{username_password, Username, Password}] = [Method || Method <- AuthMethods],
	ULen = byte_size(Username),
	PLen = byte_size(Password),
	case Transport:send(Socket, <<1, ULen, Username/binary, PLen, Password/binary>>) of
		ok -> {state, State#socks_state{status=auth_username_password}};
		Error={error, _} -> Error
	end;
%% Username/password authentication successful.
handle(<<1, 0>>, State=#socks_state{version=5, status=auth_username_password}) ->
	case send_socks5_connect(State) of
		ok -> {state, State#socks_state{status=connect}};
		Error={error, _} -> Error
	end;
%% Username/password authentication error.
handle(<<1, _>>, #socks_state{version=5, status=auth_username_password}) ->
	{error, {socks5, username_password_auth_failure}};
%% Connect reply.
handle(<<5, 0, 0, Rest0/bits>>, #socks_state{ref=StreamRef, reply_to=ReplyTo, opts=Opts,
		version=5, status=connect}) ->
	%% @todo What to do with BoundAddr and BoundPort? Add as metadata to origin info?
	{_BoundAddr, _BoundPort} = case Rest0 of
		%% @todo Seen a server with <<1, 0:48>>.
		<<1, A, B, C, D, Port:16>> ->
			{{A, B, C, D}, Port};
		<<3, Len, Host:Len/binary, Port:16>> ->
			%% We convert to list to get an inet:hostname().
			{unicode:characters_to_list(Host), Port};
		<<4, A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16, Port:16>> ->
			{{A, B, C, D, E, F, G, H}, Port}
	end,
	%% @todo Maybe an event indicating success.
	#{host := NewHost, port := NewPort} = Opts,
	%% There is no origin scheme when not using HTTP but we act as if
	%% there is and simply correct the value in the info functions.
	case Opts of
		#{transport := tls} ->
			HandshakeEvent0 = #{
				reply_to => ReplyTo,
				tls_opts => maps:get(tls_opts, Opts, []),
				timeout => maps:get(tls_handshake_timeout, Opts, infinity)
			},
			HandshakeEvent = case StreamRef of
				undefined -> HandshakeEvent0;
				_ -> HandshakeEvent0#{stream_ref => StreamRef}
			end,
			[{origin, <<"https">>, NewHost, NewPort, socks5},
				{tls_handshake, HandshakeEvent, maps:get(protocols, Opts, [http2, http]), ReplyTo}];
		_ ->
			[NewProtocol0] = maps:get(protocols, Opts, [http]),
			NewProtocol = gun_protocols:add_stream_ref(NewProtocol0, StreamRef),
			Protocol = gun_protocols:handler(NewProtocol),
			gun:reply(ReplyTo, {gun_tunnel_up, self(), StreamRef, Protocol:name()}),
			[{origin, <<"http">>, NewHost, NewPort, socks5},
				{switch_protocol, NewProtocol, ReplyTo, <<>>}]
	end;
handle(<<5, Error, _/bits>>, #socks_state{version=5, status=connect}) ->
	Reason = case Error of
		1 -> general_socks_server_failure;
		2 -> connection_not_allowed_by_ruleset;
		3 -> network_unreachable;
		4 -> host_unreachable;
		5 -> connection_refused;
		6 -> ttl_expired;
		7 -> command_not_supported;
		8 -> address_type_not_supported;
		_ -> {unknown_error, Error}
	end,
	{error, {socks5, Reason}}.

send_socks5_connect(#socks_state{socket=Socket, transport=Transport, opts=Opts}) ->
	ATypeAndDestAddr = case maps:get(host, Opts) of
		{A, B, C, D} -> <<1, A, B, C, D>>;
		{A, B, C, D, E, F, G, H} -> <<4, A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>;
		Host when is_atom(Host) ->
			DestAddr0 = atom_to_binary(Host, utf8),
			<<3, (byte_size(DestAddr0)), DestAddr0/binary>>;
		Host ->
			DestAddr0 = unicode:characters_to_binary(Host, utf8),
			<<3, (byte_size(DestAddr0)), DestAddr0/binary>>
	end,
	DestPort = maps:get(port, Opts),
	Transport:send(Socket, <<5, 1, 0, ATypeAndDestAddr/binary, DestPort:16>>).

%% We can always close immediately.
closing(_, _, _, EvHandlerState) ->
	{close, EvHandlerState}.

close(_, _, _, EvHandlerState) ->
	EvHandlerState.
