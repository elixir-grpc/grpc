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

-module(ranch_tcp).
-behaviour(ranch_transport).

-export([name/0]).
-export([secure/0]).
-export([messages/0]).
-export([listen/1]).
-export([disallowed_listen_options/0]).
-export([accept/2]).
-export([handshake/2]).
-export([handshake/3]).
-export([handshake_continue/2]).
-export([handshake_continue/3]).
-export([handshake_cancel/1]).
-export([connect/3]).
-export([connect/4]).
-export([recv/3]).
-export([recv_proxy_header/2]).
-export([send/2]).
-export([sendfile/2]).
-export([sendfile/4]).
-export([sendfile/5]).
-export([setopts/2]).
-export([getopts/2]).
-export([getstat/1]).
-export([getstat/2]).
-export([controlling_process/2]).
-export([peername/1]).
-export([sockname/1]).
-export([shutdown/2]).
-export([close/1]).
-export([cleanup/1]).
-export([format_error/1]).

-type opt() :: {backlog, non_neg_integer()}
	| {buffer, non_neg_integer()}
	| {delay_send, boolean()}
	| {dontroute, boolean()}
	| {exit_on_close, boolean()}
	| {fd, non_neg_integer()}
	| {high_msgq_watermark, non_neg_integer()}
	| {high_watermark, non_neg_integer()}
	| inet
	| inet6
	| {ip, inet:ip_address() | inet:local_address()}
	| {ipv6_v6only, boolean()}
	| {keepalive, boolean()}
	| {linger, {boolean(), non_neg_integer()}}
	| {low_msgq_watermark, non_neg_integer()}
	| {low_watermark, non_neg_integer()}
	| {nodelay, boolean()}
	| {port, inet:port_number()}
	| {priority, integer()}
	| {raw, non_neg_integer(), non_neg_integer(), binary()}
	| {recbuf, non_neg_integer()}
	| {send_timeout, timeout()}
	| {send_timeout_close, boolean()}
	| {sndbuf, non_neg_integer()}
	| {tos, integer()}.
-export_type([opt/0]).

-type opts() :: [opt()].
-export_type([opts/0]).

-spec name() -> tcp.
name() -> tcp.

-spec secure() -> boolean().
secure() ->
	false.

-spec messages() -> {tcp, tcp_closed, tcp_error, tcp_passive}.
messages() -> {tcp, tcp_closed, tcp_error, tcp_passive}.

-spec listen(ranch:transport_opts(opts())) -> {ok, inet:socket()} | {error, atom()}.
listen(TransOpts) ->
	ok = cleanup(TransOpts),
	Logger = maps:get(logger, TransOpts, logger),
	SocketOpts = maps:get(socket_opts, TransOpts, []),
	%% We set the port to 0 because it is given in the Opts directly.
	%% The port in the options takes precedence over the one in the
	%% first argument.
	gen_tcp:listen(0, prepare_socket_opts(SocketOpts, Logger)).

prepare_socket_opts([Backend = {inet_backend, _}|SocketOpts], Logger) ->
	%% In OTP/23, the inet_backend option may be used to activate the
	%% experimental socket backend for inet/gen_tcp. If present, it must
	%% be the first option in the list.
	[Backend|prepare_socket_opts(SocketOpts, Logger)];
prepare_socket_opts(SocketOpts0, Logger) ->
	SocketOpts1 = ranch:set_option_default(SocketOpts0, backlog, 1024),
	SocketOpts2 = ranch:set_option_default(SocketOpts1, nodelay, true),
	SocketOpts3 = ranch:set_option_default(SocketOpts2, send_timeout, 30000),
	SocketOpts4 = ranch:set_option_default(SocketOpts3, send_timeout_close, true),
	ranch:filter_options(SocketOpts4, disallowed_listen_options(),
		[binary, {active, false}, {reuseaddr, true}], Logger).

%% 'binary' and 'list' are disallowed but they are handled
%% specifically as they do not have 2-tuple equivalents.
-spec disallowed_listen_options() -> [atom()].
disallowed_listen_options() ->
	[active, header, mode, packet, packet_size, line_delimiter, reuseaddr].

-spec accept(inet:socket(), timeout())
	-> {ok, inet:socket()} | {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
	gen_tcp:accept(LSocket, Timeout).

-spec handshake(inet:socket(), timeout()) -> {ok, inet:socket()}.
handshake(CSocket, Timeout) ->
	handshake(CSocket, [], Timeout).

-spec handshake(inet:socket(), opts(), timeout()) -> {ok, inet:socket()}.
handshake(CSocket, _, _) ->
	{ok, CSocket}.

-spec handshake_continue(inet:socket(), timeout()) -> no_return().
handshake_continue(CSocket, Timeout) ->
	handshake_continue(CSocket, [], Timeout).

-spec handshake_continue(inet:socket(), opts(), timeout()) -> no_return().
handshake_continue(_, _, _) ->
	error(not_supported).

-spec handshake_cancel(inet:socket()) -> no_return().
handshake_cancel(_) ->
	error(not_supported).

%% @todo Probably filter Opts?
-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts) when is_integer(Port) ->
	gen_tcp:connect(Host, Port,
		Opts ++ [binary, {active, false}]).

%% @todo Probably filter Opts?
-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any(), timeout())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts, Timeout) when is_integer(Port) ->
	gen_tcp:connect(Host, Port,
		Opts ++ [binary, {active, false}],
		Timeout).

-spec recv(inet:socket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	gen_tcp:recv(Socket, Length, Timeout).

-spec recv_proxy_header(inet:socket(), timeout())
	-> {ok, ranch_proxy_header:proxy_info()}
	| {error, closed | atom()}
	| {error, protocol_error, atom()}.
recv_proxy_header(Socket, Timeout) ->
	case recv(Socket, 0, Timeout) of
		{ok, Data} ->
			case ranch_proxy_header:parse(Data) of
				{ok, ProxyInfo, <<>>} ->
					{ok, ProxyInfo};
				{ok, ProxyInfo, Rest} ->
					case gen_tcp:unrecv(Socket, Rest) of
						ok ->
							{ok, ProxyInfo};
						Error ->
							Error
					end;
				{error, HumanReadable} ->
					{error, protocol_error, HumanReadable}
			end;
		Error ->
			Error
	end.

-spec send(inet:socket(), iodata()) -> ok | {error, atom()}.
send(Socket, Packet) ->
	gen_tcp:send(Socket, Packet).

-spec sendfile(inet:socket(), file:name_all() | file:fd())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, Filename) ->
	sendfile(Socket, Filename, 0, 0, []).

-spec sendfile(inet:socket(), file:name_all() | file:fd(), non_neg_integer(),
		non_neg_integer())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, File, Offset, Bytes) ->
	sendfile(Socket, File, Offset, Bytes, []).

-spec sendfile(inet:socket(), file:name_all() | file:fd(), non_neg_integer(),
		non_neg_integer(), [{chunk_size, non_neg_integer()}])
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, Filename, Offset, Bytes, Opts)
		when is_list(Filename) orelse is_atom(Filename)
		orelse is_binary(Filename) ->
	case file:open(Filename, [read, raw, binary]) of
		{ok, RawFile} ->
			try sendfile(Socket, RawFile, Offset, Bytes, Opts) of
				Result -> Result
			after
				ok = file:close(RawFile)
			end;
		{error, _} = Error ->
			Error
	end;
sendfile(Socket, RawFile, Offset, Bytes, Opts) ->
	Opts2 = case Opts of
		[] -> [{chunk_size, 16#1FFF}];
		_ -> Opts
	end,
	try file:sendfile(RawFile, Socket, Offset, Bytes, Opts2) of
		Result -> Result
	catch
		error:{badmatch, {error, enotconn}} ->
			%% file:sendfile/5 might fail by throwing a
			%% {badmatch, {error, enotconn}}. This is because its
			%% implementation fails with a badmatch in
			%% prim_file:sendfile/10 if the socket is not connected.
			{error, closed}
	end.

%% @todo Probably filter Opts?
-spec setopts(inet:socket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
	inet:setopts(Socket, Opts).

-spec getopts(inet:socket(), [atom()]) -> {ok, list()} | {error, atom()}.
getopts(Socket, Opts) ->
	inet:getopts(Socket, Opts).

-spec getstat(inet:socket()) -> {ok, list()} | {error, atom()}.
getstat(Socket) ->
	inet:getstat(Socket).

-spec getstat(inet:socket(), [atom()]) -> {ok, list()} | {error, atom()}.
getstat(Socket, OptionNames) ->
	inet:getstat(Socket, OptionNames).

-spec controlling_process(inet:socket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	gen_tcp:controlling_process(Socket, Pid).

-spec peername(inet:socket())
	-> {ok, {inet:ip_address(), inet:port_number()} | {local, binary()}} | {error, atom()}.
peername(Socket) ->
	inet:peername(Socket).

-spec sockname(inet:socket())
	-> {ok, {inet:ip_address(), inet:port_number()} | {local, binary()}} | {error, atom()}.
sockname(Socket) ->
	inet:sockname(Socket).

-spec shutdown(inet:socket(), read | write | read_write)
	-> ok | {error, atom()}.
shutdown(Socket, How) ->
	gen_tcp:shutdown(Socket, How).

-spec close(inet:socket()) -> ok.
close(Socket) ->
	gen_tcp:close(Socket).

-spec cleanup(ranch:transport_opts(opts())) -> ok.
cleanup(#{socket_opts:=SocketOpts}) ->
	case lists:keyfind(ip, 1, lists:reverse(SocketOpts)) of
		{ip, {local, SockFile}} ->
			_ = file:delete(SockFile),
			ok;
		_ ->
			ok
	end;
cleanup(_) ->
	ok.

-spec format_error(inet:posix() | system_limit) -> string().
format_error(Reason) ->
	inet:format_error(Reason).
