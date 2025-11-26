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

-module(ranch_server_proxy).

-behavior(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

-spec init([]) -> {ok, pid()} | {stop, term()}.
init([]) ->
	case wait_ranch_server(50) of
		{ok, Monitor} ->
			{ok, Monitor, hibernate};
		{error, Reason} ->
			{stop, Reason}
	end.

-spec handle_call(_, _, reference()) -> {noreply, reference(), hibernate}.
handle_call(_, _, Monitor) ->
	{noreply, Monitor, hibernate}.

-spec handle_cast(_, reference()) -> {noreply, reference(), hibernate}.
handle_cast(_, Monitor) ->
	{noreply, Monitor, hibernate}.

-spec handle_info(term(), reference()) -> {noreply, reference(), hibernate} | {stop, term(), reference()}.
handle_info({'DOWN', Monitor, process, _, Reason}, Monitor) ->
	{stop, Reason, Monitor};
handle_info(_, Monitor) ->
	{noreply, Monitor, hibernate}.

-spec code_change(term() | {down, term()}, reference(), term()) -> {ok, reference()}.
code_change(_, Monitor, _) ->
	{ok, Monitor}.

wait_ranch_server(N) ->
	case whereis(ranch_server) of
		undefined when N > 0 ->
			receive after 100 -> ok end,
			wait_ranch_server(N - 1);
		undefined ->
			{error, noproc};
		Pid ->
			Monitor = monitor(process, Pid),
			{ok, Monitor}
	end.
