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

-module(ranch_listener_sup).
-behaviour(supervisor).

-export([start_link/5]).
-export([init/1]).

-spec start_link(ranch:ref(), module(), any(), module(), any())
	-> {ok, pid()}.
start_link(Ref, Transport, TransOpts, Protocol, ProtoOpts) ->
	MaxConns = maps:get(max_connections, TransOpts, 1024),
	Logger = maps:get(logger, TransOpts, logger),
	ranch_server:set_new_listener_opts(Ref, MaxConns, TransOpts, ProtoOpts,
		[Ref, Transport, TransOpts, Protocol, ProtoOpts]),
	supervisor:start_link(?MODULE, {
		Ref, Transport, Protocol, Logger
	}).

-spec init({ranch:ref(), module(), module(), module()})
	-> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init({Ref, Transport, Protocol, Logger}) ->
	ok = ranch_server:set_listener_sup(Ref, self()),
	ChildSpecs = [
		#{
			id => ranch_conns_sup_sup,
			start => {ranch_conns_sup_sup, start_link, [Ref, Transport, Protocol, Logger]},
			type => supervisor
		},
		#{
			id => ranch_acceptors_sup,
			start => {ranch_acceptors_sup, start_link, [Ref, Transport, Logger]},
			type => supervisor
		}
	],
	{ok, {#{strategy => rest_for_one}, ChildSpecs}}.
