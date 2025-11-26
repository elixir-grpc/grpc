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

-module(gun_tcp_proxy).

-export([name/0]).
-export([messages/0]).
-export([connect/3]).
-export([connect/4]).
-export([send/2]).
-export([setopts/2]).
-export([sockname/1]).
-export([close/1]).

-type socket() :: #{
	%% The pid of the Gun connection.
	gun_pid := pid(),

	%% The pid of the process that gets replies for this tunnel.
	reply_to := pid(),

	%% The full stream reference for this tunnel.
	stream_ref := gun:stream_ref()
}.

name() -> tcp_proxy.

messages() -> {tcp_proxy, tcp_proxy_closed, tcp_proxy_error}.

-spec connect(_, _, _) -> no_return().
connect(_, _, _) ->
	error(not_implemented).

-spec connect(_, _, _, _) -> no_return().
connect(_, _, _, _) ->
	error(not_implemented).

-spec send(socket(), iodata()) -> ok.
send(#{gun_pid := GunPid, reply_to := ReplyTo, stream_ref := StreamRef,
		handle_continue_stream_ref := ContinueStreamRef}, Data) ->
	GunPid ! {handle_continue, ContinueStreamRef, {data, ReplyTo, StreamRef, nofin, Data}},
	ok;
send(#{reply_to := ReplyTo, stream_ref := StreamRef}, Data) ->
	gen_statem:cast(self(), {data, ReplyTo, StreamRef, nofin, Data}).

-spec setopts(_, _) -> no_return().
setopts(#{handle_continue_stream_ref := _}, _) ->
	%% We send messages automatically regardless of active mode.
	ok;
setopts(_, _) ->
	error(not_implemented).

-spec sockname(_) -> no_return().
sockname(_) ->
	error(not_implemented).

-spec close(socket()) -> ok.
close(_) ->
	ok.
