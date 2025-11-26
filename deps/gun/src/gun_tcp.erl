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

-module(gun_tcp).

-export([name/0]).
-export([messages/0]).
-export([domain_lookup/4]).
-export([connect/2]).
-export([send/2]).
-export([setopts/2]).
-export([sockname/1]).
-export([close/1]).

-type lookup_info() :: #{
	ip_addresses := [inet:ip_address()],
	port := inet:port_number(),
	tcp_module := module(),
	tcp_opts := [gen_tcp:connect_option()]
}.
-export_type([lookup_info/0]).

name() -> tcp.

messages() -> {tcp, tcp_closed, tcp_error}.

%% The functions domain_lookup/4 and connect/2 are very similar
%% to gen_tcp:connect/4 except the logic is split in order to
%% be able to trigger events between the domain lookup step
%% and the actual connect step.

-spec domain_lookup(inet:ip_address() | inet:hostname(),
	inet:port_number(), [gen_tcp:connect_option()], timeout())
	-> {ok, lookup_info()} | {error, atom()}.
domain_lookup(Address, Port0, Opts0, Timeout) ->
	{Mod, Opts} = inet:tcp_module(Opts0, Address),
	Timer = inet:start_timer(Timeout),
	try Mod:getaddrs(Address, Timer) of
		{ok, IPs} ->
			case Mod:getserv(Port0) of
				{ok, Port} ->
					{ok, #{
						ip_addresses => IPs,
						port => Port,
						tcp_module => Mod,
						tcp_opts => Opts ++ [binary, {active, false}, {packet, raw}]
					}};
				Error ->
					maybe_exit(Error)
			end;
		Error ->
			maybe_exit(Error)
	after
		_ = inet:stop_timer(Timer)
	end.

-spec connect(lookup_info(), timeout())
	-> {ok, inet:socket()} | {error, atom()}.
connect(#{ip_addresses := IPs, port := Port, tcp_module := Mod, tcp_opts := Opts}, Timeout) ->
	Timer = inet:start_timer(Timeout),
	Res = try
		try_connect(IPs, Port, Opts, Timer, Mod, {error, einval})
	after
		_ = inet:stop_timer(Timer)
	end,
	case Res of
		{ok, S} -> {ok, S};
		Error -> maybe_exit(Error)
	end.

try_connect([IP|IPs], Port, Opts, Timer, Mod, _) ->
	Timeout = inet:timeout(Timer),
	case Mod:connect(IP, Port, Opts, Timeout) of
		{ok, S} -> {ok, S};
		{error, einval} -> {error, einval};
		{error, timeout} -> {error, timeout};
		Error -> try_connect(IPs, Port, Opts, Timer, Mod, Error)
	end;
try_connect([], _, _, _, _, Error) ->
	Error.

maybe_exit({error, einval}) -> exit(badarg);
maybe_exit({error, eaddrnotavail}) -> exit(badarg);
maybe_exit(Error) -> Error.

-spec send(inet:socket(), iodata()) -> ok | {error, atom()}.
send(Socket, Packet) ->
	gen_tcp:send(Socket, Packet).

-spec setopts(inet:socket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
	inet:setopts(Socket, Opts).

-spec sockname(inet:socket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(Socket) ->
	inet:sockname(Socket).

-spec close(inet:socket()) -> ok.
close(Socket) ->
	gen_tcp:close(Socket).
