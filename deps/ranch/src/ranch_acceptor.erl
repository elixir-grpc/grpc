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

-module(ranch_acceptor).

-export([start_link/5]).
-export([init/4]).
-export([loop/5]).

-spec start_link(ranch:ref(), pos_integer(), inet:socket(), module(), module())
	-> {ok, pid()}.
start_link(Ref, AcceptorId, LSocket, Transport, Logger) ->
	ConnsSup = ranch_server:get_connections_sup(Ref, AcceptorId),
	Pid = spawn_link(?MODULE, init, [LSocket, Transport, Logger, ConnsSup]),
	{ok, Pid}.

-spec init(inet:socket(), module(), module(), pid()) -> no_return().
init(LSocket, Transport, Logger, ConnsSup) ->
	MonitorRef = monitor(process, ConnsSup),
	loop(LSocket, Transport, Logger, ConnsSup, MonitorRef).

-spec loop(inet:socket(), module(), module(), pid(), reference()) -> no_return().
loop(LSocket, Transport, Logger, ConnsSup, MonitorRef) ->
	_ = case Transport:accept(LSocket, infinity) of
		{ok, CSocket} ->
			case Transport:controlling_process(CSocket, ConnsSup) of
				ok ->
					%% This call will not return until process has been started
					%% AND we are below the maximum number of connections.
					ranch_conns_sup:start_protocol(ConnsSup, MonitorRef,
						CSocket);
				{error, _} ->
					Transport:close(CSocket)
			end;
		%% Reduce the accept rate if we run out of file descriptors.
		%% We can't accept anymore anyway, so we might as well wait
		%% a little for the situation to resolve itself.
		{error, emfile} ->
			ranch:log(warning,
				"Ranch acceptor reducing accept rate: out of file descriptors~n",
				[], Logger),
			receive after 100 -> ok end;
		%% Exit if the listening socket got closed.
		{error, closed} ->
			exit(closed);
		%% Continue otherwise.
		{error, _} ->
			ok
	end,
	flush(Logger),
	?MODULE:loop(LSocket, Transport, Logger, ConnsSup, MonitorRef).

flush(Logger) ->
	receive Msg ->
		ranch:log(warning,
			"Ranch acceptor received unexpected message: ~p~n",
			[Msg], Logger),
		flush(Logger)
	after 0 ->
		ok
	end.
