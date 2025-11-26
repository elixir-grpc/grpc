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

-module(gun_tls_proxy_http2_connect).

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
	stream_ref := gun:stream_ref(),

	%% The full stream reference for the responsible HTTP/2 stream.
	handle_continue_stream_ref := gun:stream_ref()
}.

name() -> tls_proxy_http2_connect.

messages() -> {tls_proxy_http2_connect, tls_proxy_http2_connect_closed, tls_proxy_http2_connect_error}.

-spec connect(_, _, _) -> no_return().
connect(_, _, _) ->
	error(not_implemented).

-spec connect(_, _, _, _) -> no_return().
connect(_, _, _, _) ->
	error(not_implemented).

-spec send(socket(), iodata()) -> ok.
send(#{gun_pid := GunPid, reply_to := ReplyTo, stream_ref := DataStreamRef,
		handle_continue_stream_ref := StreamRef}, Data) ->
	GunPid ! {handle_continue, StreamRef, {data, ReplyTo, DataStreamRef, nofin, Data}},
	ok.

-spec setopts(_, _) -> no_return().
setopts(_, _) ->
	%% We send messages automatically regardless of active mode.
	ok.

-spec sockname(_) -> no_return().
sockname(_) ->
	error(not_implemented).

-spec close(socket()) -> ok.
close(_) ->
	ok.
