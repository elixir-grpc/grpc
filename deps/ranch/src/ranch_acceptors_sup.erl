%% Copyright (c) Loïc Hoguin <essen@ninenines.eu>
%% Copyright (c) Jan Uhlig <juhlig@hnc-agency.org>
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

-module(ranch_acceptors_sup).
-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

-spec start_link(ranch:ref(), module(), module())
	-> {ok, pid()}.
start_link(Ref, Transport, Logger) ->
	supervisor:start_link(?MODULE, [Ref, Transport, Logger]).

-spec init([term()]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([Ref, Transport, Logger]) ->
	TransOpts = ranch_server:get_transport_options(Ref),
	NumAcceptors = maps:get(num_acceptors, TransOpts, 10),
	NumListenSockets = maps:get(num_listen_sockets, TransOpts, 1),
	LSockets = case get(lsockets) of
		undefined ->
			LSockets1 = start_listen_sockets(Ref, NumListenSockets, Transport, TransOpts, Logger),
			put(lsockets, LSockets1),
			LSockets1;
		LSockets1 ->
			LSockets1
	end,
	Procs = [begin
		LSocketId = (AcceptorId rem NumListenSockets) + 1,
		{_, LSocket} = lists:keyfind(LSocketId, 1, LSockets),
		#{
			id => {acceptor, self(), AcceptorId},
			start => {ranch_acceptor, start_link, [Ref, AcceptorId, LSocket, Transport, Logger]},
			shutdown => brutal_kill
		}
	end || AcceptorId <- lists:seq(1, NumAcceptors)],
	{ok, {#{intensity => 1 + ceil(math:log2(NumAcceptors))}, Procs}}.

-spec start_listen_sockets(any(), pos_integer(), module(), map(), module())
	-> [{pos_integer(), inet:socket()}].
start_listen_sockets(Ref, NumListenSockets, Transport, TransOpts0, Logger) when NumListenSockets > 0 ->
	BaseSocket = start_listen_socket(Ref, Transport, TransOpts0, Logger),
	{ok, Addr} = Transport:sockname(BaseSocket),
	ExtraSockets = case Addr of
		{local, _} when NumListenSockets > 1 ->
			listen_error(Ref, Transport, TransOpts0, reuseport_local, Logger);
		{local, _} ->
			[];
		{_, Port} ->
			SocketOpts = maps:get(socket_opts, TransOpts0, []),
			SocketOpts1 = lists:keystore(port, 1, SocketOpts, {port, Port}),
			TransOpts1 = TransOpts0#{socket_opts => SocketOpts1},
			[{N, start_listen_socket(Ref, Transport, TransOpts1, Logger)}
				|| N <- lists:seq(2, NumListenSockets)]
	end,
	ranch_server:set_addr(Ref, Addr),
	[{1, BaseSocket}|ExtraSockets].

-spec start_listen_socket(any(), module(), map(), module()) -> inet:socket().
start_listen_socket(Ref, Transport, TransOpts, Logger) ->
	case Transport:listen(TransOpts) of
		{ok, Socket} ->
			PostListenCb = maps:get(post_listen_callback, TransOpts, fun (_) -> ok end),
			case PostListenCb(Socket) of
				ok ->
					Socket;
				{error, Reason} ->
					listen_error(Ref, Transport, TransOpts, Reason, Logger)
			end;
		{error, Reason} ->
			listen_error(Ref, Transport, TransOpts, Reason, Logger)
	end.

-spec listen_error(any(), module(), any(), atom(), module()) -> no_return().
listen_error(Ref, Transport, TransOpts0, Reason, Logger) ->
	SocketOpts0 = maps:get(socket_opts, TransOpts0, []),
	SocketOpts = hide_socket_opts(SocketOpts0),
	TransOpts = TransOpts0#{socket_opts => SocketOpts},
	ranch:log(error,
		"Failed to start Ranch listener ~p in ~p:listen(~0p) for reason ~p (~s)~n",
		[Ref, Transport, TransOpts, Reason, format_error(Transport, Reason)], Logger),
	exit({listen_error, Ref, Reason}).

hide_socket_opts([]) ->
	[];
hide_socket_opts([{cert, _}|SocketOpts]) ->
	[{cert, '...'}|hide_socket_opts(SocketOpts)];
hide_socket_opts([{key, _}|SocketOpts]) ->
	[{key, '...'}|hide_socket_opts(SocketOpts)];
hide_socket_opts([{cacerts, _}|SocketOpts]) ->
	[{cacerts, '...'}|hide_socket_opts(SocketOpts)];
hide_socket_opts([{password, _}|SocketOpts]) ->
	[{password, '...'}|hide_socket_opts(SocketOpts)];
hide_socket_opts([{certs_keys, _}|SocketOpts]) ->
	[{certs_keys, '...'}|hide_socket_opts(SocketOpts)];
hide_socket_opts([SocketOpt|SocketOpts]) ->
	[SocketOpt|hide_socket_opts(SocketOpts)].

%% Handling of no_cert really should be done in ranch_ssl. We leave it here for
%% backwards compatibility with possibly existing custom transports without a
%% format_error/1 implementation that may rely on this module handling it.
%% TODO: Remove in Ranch 3.0
format_error(_, no_cert) ->
	"no certificate provided; see cert, certfile, sni_fun or sni_hosts options";
format_error(_, reuseport_local) ->
	"num_listen_sockets must be set to 1 for local sockets";
format_error(Transport, Reason) ->
	%% TODO: Required callback in Ranch 3.0
	case erlang:function_exported(Transport, format_error, 1) of
		true ->
			Transport:format_error(Reason);
		false ->
			lists:flatten(io_lib:format("~0p", [Reason]))
	end.
