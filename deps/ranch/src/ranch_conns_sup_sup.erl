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

-module(ranch_conns_sup_sup).

-behaviour(supervisor).

-export([start_link/4]).
-export([init/1]).

-spec start_link(ranch:ref(), module(), module(), module()) -> {ok, pid()}.
start_link(Ref, Transport, Protocol, Logger) ->
	ok = ranch_server:cleanup_connections_sups(Ref),
	supervisor:start_link(?MODULE, {
		Ref, Transport, Protocol, Logger
	}).

-spec init({ranch:ref(), module(), module(), module()})
	-> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init({Ref, Transport, Protocol, Logger}) ->
	TransOpts = ranch_server:get_transport_options(Ref),
	NumAcceptors = maps:get(num_acceptors, TransOpts, 10),
	NumConnsSups = maps:get(num_conns_sups, TransOpts, NumAcceptors),
	StatsCounters = counters:new(2*NumConnsSups, []),
	ok = ranch_server:set_stats_counters(Ref, StatsCounters),
	ChildSpecs = [#{
		id => {ranch_conns_sup, N},
		start => {ranch_conns_sup, start_link, [Ref, N, Transport, TransOpts, Protocol, Logger]},
		type => supervisor
	} || N <- lists:seq(1, NumConnsSups)],
	{ok, {#{intensity => 1 + ceil(math:log2(NumConnsSups))}, ChildSpecs}}.
