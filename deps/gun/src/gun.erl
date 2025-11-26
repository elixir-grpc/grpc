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

-module(gun).
-behavior(gen_statem).

-ifdef(OTP_RELEASE).
-compile({nowarn_deprecated_function, [{erlang, get_stacktrace, 0}]}).
-endif.

%% Connection.
-export([open/2]).
-export([open/3]).
-export([open_unix/2]).
-export([set_owner/2]).
-export([info/1]).
-export([close/1]).
-export([shutdown/1]).

%% Requests.
-export([delete/2]).
-export([delete/3]).
-export([delete/4]).
-export([get/2]).
-export([get/3]).
-export([get/4]).
-export([head/2]).
-export([head/3]).
-export([head/4]).
-export([options/2]).
-export([options/3]).
-export([options/4]).
-export([patch/3]).
-export([patch/4]).
-export([patch/5]).
-export([post/3]).
-export([post/4]).
-export([post/5]).
-export([put/3]).
-export([put/4]).
-export([put/5]).

%% Generic requests interface.
-export([headers/4]).
-export([headers/5]).
-export([request/5]).
-export([request/6]).

%% Streaming data.
-export([data/4]).

%% User pings.
-export([ping/1]).
-export([ping/2]).

%% Tunneling.
-export([connect/2]).
-export([connect/3]).
-export([connect/4]).

%% Cookies.
%% @todo -export([gc_cookies/1]).
%% @todo -export([session_gc_cookies/1]).

%% Awaiting gun messages.
-export([await/2]).
-export([await/3]).
-export([await/4]).
-export([await_body/2]).
-export([await_body/3]).
-export([await_body/4]).
-export([await_up/1]).
-export([await_up/2]).
-export([await_up/3]).

%% Flushing gun messages.
-export([flush/1]).

%% Streams.
-export([update_flow/3]).
-export([cancel/2]).
-export([stream_info/2]).

%% Websocket.
-export([ws_upgrade/2]).
-export([ws_upgrade/3]).
-export([ws_upgrade/4]).
-export([ws_send/3]).

%% Internals.
-export([start_link/4]).
-export([callback_mode/0]).
-export([init/1]).
-export([default_transport/1]).
-export([not_connected/3]).
-export([domain_lookup/3]).
-export([connecting/3]).
-export([initial_tls_handshake/3]).
-export([ensure_tls_opts/3]).
-export([tls_handshake/3]).
-export([connected_protocol_init/3]).
-export([connected/3]).
-export([connected_data_only/3]).
-export([connected_no_input/3]).
-export([connected_ws_only/3]).
-export([closing/3]).
-export([terminate/3]).
-export([reply/2]).

-type req_headers() :: [{binary() | string() | atom(), iodata()}]
	| #{binary() | string() | atom() => iodata()}.
-export_type([req_headers/0]).

-type ws_close_code() :: 1000..4999.

-type ws_frame() :: close | ping | pong
	| {text | binary | close | ping | pong, iodata()}
	| {close, ws_close_code(), iodata()}.
-export_type([ws_frame/0]).

-type protocol() :: http | http2 | http3 | raw | socks
	| {http, http_opts()} | {http2, http2_opts()} | {http3, http3_opts()}
	| {raw, raw_opts()} | {socks, socks_opts()}.
-export_type([protocol/0]).

-type protocols() :: [protocol()].
-export_type([protocols/0]).

-type stream_ref() :: reference() | [reference()].
-export_type([stream_ref/0]).

-type opts() :: #{
	connect_timeout => timeout(),
	cookie_store => gun_cookies:store(),
	domain_lookup_timeout => timeout(),
	event_handler => {module(), any()},
	http_opts => http_opts(),
	http2_opts => http2_opts(),
	http3_opts => http3_opts(),
	protocols => protocols(),
	raw_opts => raw_opts(),
	retry => non_neg_integer(),
	retry_fun => fun((non_neg_integer(), opts())
		-> #{retries => non_neg_integer(), timeout => pos_integer()}),
	retry_timeout => pos_integer(),
	socks_opts => socks_opts(),
	supervise => boolean(),
	tcp_opts => [gen_tcp:connect_option()],
	tls_handshake_timeout => timeout(),
	tls_opts => [ssl:tls_client_option()],
	trace => boolean(),
	transport => tcp | tls | ssl | quic,
	ws_opts => ws_opts()
}.
-export_type([opts/0]).

-type connect_destination() :: #{
	host := inet:hostname() | inet:ip_address(),
	port := inet:port_number(),
	username => iodata(),
	password => iodata(),
	protocols => protocols(),
	transport => tcp | tls,
	tls_opts => [ssl:tls_client_option()],
	tls_handshake_timeout => timeout()
}.
-export_type([connect_destination/0]).

-type intermediary() :: #{
	type := connect | socks5,
	host := inet:hostname() | inet:ip_address(),
	port := inet:port_number(),
	transport := tcp | tls | tls_proxy,
	protocol := http | socks
}.

-type tunnel_info() :: #{
	%% Tunnel.
	host := inet:hostname() | inet:ip_address(),
	port := inet:port_number(),

	%% Origin.
	origin_host => inet:hostname() | inet:ip_address(),
	origin_port => inet:port_number()
}.
-export_type([tunnel_info/0]).

-type raw_opts() :: #{
	flow => pos_integer(),
	%% Internal.
	tunnel_transport => tcp | tls
}.
-export_type([raw_opts/0]).

-type reply_to() :: pid()
	| fun((_) -> _)
	| {fun(), list()}
	| {module(), atom(), list()}.
-export_type([reply_to/0]).

-type req_opts() :: #{
	flow => pos_integer(),
	reply_to => reply_to(),
	tunnel => stream_ref()
}.
-export_type([req_opts/0]).

-type http_opts() :: #{
	closing_timeout => timeout(),
	content_handlers => gun_content_handler:opt(),
	cookie_ignore_informational => boolean(),
	flow => pos_integer(),
	keepalive => timeout(),
	transform_header_name => fun((binary()) -> binary()),
	version => 'HTTP/1.1' | 'HTTP/1.0',

	%% Internal.
	tunnel_transport => tcp | tls
}.
-export_type([http_opts/0]).

%% @todo Accept http_opts, http2_opts, and so on.
-type http2_opts() :: #{
	closing_timeout => timeout(),
	content_handlers => gun_content_handler:opt(),
	cookie_ignore_informational => boolean(),
	flow => pos_integer(),
	keepalive => timeout(),
	keepalive_tolerance => non_neg_integer(),
	notify_settings_changed => boolean(),

	%% Options copied from cow_http2_machine.
	connection_window_margin_size => 0..16#7fffffff,
	connection_window_update_threshold => 0..16#7fffffff,
	enable_connect_protocol => boolean(),
	initial_connection_window_size => 65535..16#7fffffff,
	initial_stream_window_size => 0..16#7fffffff,
	max_connection_window_size => 0..16#7fffffff,
	max_concurrent_streams => non_neg_integer() | infinity,
	max_decode_table_size => non_neg_integer(),
	max_encode_table_size => non_neg_integer(),
	max_fragmented_header_block_size => 16384..16#7fffffff,
	max_frame_size_received => 16384..16777215,
	max_frame_size_sent => 16384..16777215 | infinity,
	max_stream_window_size => 0..16#7fffffff,
	preface_timeout => timeout(),
	settings_timeout => timeout(),
	stream_window_data_threshold => 0..16#7fffffff,
	stream_window_margin_size => 0..16#7fffffff,
	stream_window_update_threshold => 0..16#7fffffff,

	%% Internal.
	tunnel_transport => tcp | tls
}.
-export_type([http2_opts/0]).

%% @todo
-type http3_opts() :: #{
}.
-export_type([http3_opts/0]).

-type socks_opts() :: #{
	version => 5,
	auth => [{username_password, binary(), binary()} | none],
	host := inet:hostname() | inet:ip_address(),
	port := inet:port_number(),
	protocols => protocols(),
	transport => tcp | tls,
	tls_opts => [ssl:tls_client_option()],
	tls_handshake_timeout => timeout(),

	%% Internal.
	tunnel_transport => tcp | tls
}.
-export_type([socks_opts/0]).

-type ws_opts() :: #{
	closing_timeout => timeout(),
	compress => boolean(),
	default_protocol => module(),
	flow => pos_integer(),
	keepalive => timeout(),
	protocols => [{binary(), module()}],
	reply_to => pid(),
	silence_pings => boolean(),
	tunnel => stream_ref(),
	user_opts => any()
}.
-export_type([ws_opts/0]).

-type resp_headers() :: [{binary(), binary()}].

-type await_result() :: {inform, 100..199, resp_headers()}
	| {response, fin | nofin, non_neg_integer(), resp_headers()}
	| {data, fin | nofin, binary()}
	| {sse, cow_sse:event() | fin}
	| {trailers, resp_headers()}
	| {push, stream_ref(), binary(), binary(), resp_headers()}
	| {upgrade, [binary()], resp_headers()}
	| {ws, ws_frame()}
	| {up, http | http2 | raw | socks}
	| {notify, settings_changed, map()}
	| {error, {stream_error | connection_error | down, any()} | timeout}.
-export_type([await_result/0]).

-type await_body_result() :: {ok, binary()}
	| {ok, binary(), resp_headers()}
	| {error, {stream_error | connection_error | down, any()} | timeout}.
-export_type([await_body_result/0]).

-record(state, {
	owner :: pid(),
	status :: {up, reference()} | {down, any()} | shutdown,
	host :: inet:hostname() | inet:ip_address(),
	port :: inet:port_number(),
	origin_scheme :: binary(),
	origin_host :: inet:hostname() | inet:ip_address(),
	origin_port :: inet:port_number(),
	intermediaries = [] :: [intermediary()],
	opts :: opts(),
	keepalive_ref :: undefined | reference(),
	socket :: undefined | inet:socket() | ssl:sslsocket() | pid(),
	transport :: module(),
	active = false :: boolean(),
	messages :: {atom(), atom(), atom()},
	protocol :: module(),
	protocol_state :: any(),
	cookie_store :: undefined | {module(), any()},
	event_handler :: module(),
	event_handler_state :: any()
}).

%% Connection.

-spec open(inet:hostname() | inet:ip_address(), inet:port_number())
	-> {ok, pid()} | {error, any()}.
open(Host, Port) ->
	open(Host, Port, #{}).

-spec open(inet:hostname() | inet:ip_address(), inet:port_number(), opts())
	-> {ok, pid()} | {error, any()}.
open(Host, Port, Opts) when is_list(Host); is_atom(Host); is_tuple(Host) ->
	do_open(Host, Port, Opts).

-spec open_unix(Path::string(), opts())
	-> {ok, pid()} | {error, any()}.
open_unix(SocketPath, Opts) ->
	do_open({local, SocketPath}, 0, Opts).

do_open(Host, Port, Opts0) ->
	%% We accept both ssl and tls but only use tls in the code.
	Opts = case Opts0 of
		#{transport := ssl} -> Opts0#{transport => tls};
		_ -> Opts0
	end,
	case check_options(maps:to_list(Opts)) of
		ok ->
			Result = case maps:get(supervise, Opts, true) of
				true -> supervisor:start_child(gun_conns_sup, [self(), Host, Port, Opts]);
				false -> start_link(self(), Host, Port, Opts)
			end,
			case Result of
				OK = {ok, ServerPid} ->
					consider_tracing(ServerPid, Opts),
					OK;
				StartError ->
					StartError
			end;
		CheckError ->
			CheckError
	end.

check_options([]) ->
	ok;
check_options([{connect_timeout, infinity}|Opts]) ->
	check_options(Opts);
check_options([{connect_timeout, T}|Opts]) when is_integer(T), T >= 0 ->
	check_options(Opts);
check_options([{cookie_store, {Mod, _}}|Opts]) when is_atom(Mod) ->
	check_options(Opts);
check_options([{domain_lookup_timeout, infinity}|Opts]) ->
	check_options(Opts);
check_options([{domain_lookup_timeout, T}|Opts]) when is_integer(T), T >= 0 ->
	check_options(Opts);
check_options([{event_handler, {Mod, _}}|Opts]) when is_atom(Mod) ->
	check_options(Opts);
check_options([{http_opts, ProtoOpts}|Opts]) when is_map(ProtoOpts) ->
	case gun_http:check_options(ProtoOpts) of
		ok ->
			check_options(Opts);
		Error ->
			Error
	end;
check_options([{http2_opts, ProtoOpts}|Opts]) when is_map(ProtoOpts) ->
	case gun_http2:check_options(ProtoOpts) of
		ok ->
			check_options(Opts);
		Error ->
			Error
	end;
check_options([{http3_opts, ProtoOpts}|Opts]) when is_map(ProtoOpts) ->
	case gun_http3:check_options(ProtoOpts) of
		ok ->
			check_options(Opts)
	end;
check_options([Opt = {protocols, L}|Opts]) when is_list(L) ->
	case check_protocols_opt(L) of
		ok -> check_options(Opts);
		error -> {error, {options, Opt}}
	end;
check_options([{raw_opts, ProtoOpts}|Opts]) when is_map(ProtoOpts) ->
	case gun_raw:check_options(ProtoOpts) of
		ok ->
			check_options(Opts);
		Error ->
			Error
	end;
check_options([{retry, R}|Opts]) when is_integer(R), R >= 0 ->
	check_options(Opts);
check_options([{retry_fun, F}|Opts]) when is_function(F, 2) ->
	check_options(Opts);
check_options([{retry_timeout, T}|Opts]) when is_integer(T), T >= 0 ->
	check_options(Opts);
check_options([{socks_opts, ProtoOpts}|Opts]) when is_map(ProtoOpts) ->
	case gun_socks:check_options(ProtoOpts) of
		ok ->
			check_options(Opts);
		Error ->
			Error
	end;
check_options([{supervise, B}|Opts]) when is_boolean(B) ->
	check_options(Opts);
check_options([{tcp_opts, L}|Opts]) when is_list(L) ->
	check_options(Opts);
check_options([{tls_handshake_timeout, infinity}|Opts]) ->
	check_options(Opts);
check_options([{tls_handshake_timeout, T}|Opts]) when is_integer(T), T >= 0 ->
	check_options(Opts);
check_options([{tls_opts, L}|Opts]) when is_list(L) ->
	check_options(Opts);
check_options([{trace, B}|Opts]) when is_boolean(B) ->
	check_options(Opts);
check_options([{transport, T}|Opts]) when T =:= tcp; T =:= tls; T =:= quic ->
	check_options(Opts);
check_options([{ws_opts, ProtoOpts}|Opts]) when is_map(ProtoOpts) ->
	case gun_ws:check_options(ProtoOpts) of
		ok ->
			check_options(Opts);
		Error ->
			Error
	end;
check_options([Opt|_]) ->
	{error, {options, Opt}}.

check_protocols_opt(Protocols) ->
	%% Protocols must not appear more than once, and they
	%% must be one of http, http2, http3, raw or socks.
	ProtoNames0 = lists:usort([case P0 of {P, _} -> P; P -> P end || P0 <- Protocols]),
	ProtoNames = [P || P <- ProtoNames0, lists:member(P, [http, http2, http3, raw, socks])],
	case length(Protocols) =:= length(ProtoNames) of
		false -> error;
		true ->
			%% When options are given alongside a protocol, they
			%% must be checked as well.
			TupleCheck = [case P of
				{http, Opts} -> gun_http:check_options(Opts);
				{http2, Opts} -> gun_http2:check_options(Opts);
				{http3, Opts} -> gun_http3:check_options(Opts);
				{raw, Opts} -> gun_raw:check_options(Opts);
				{socks, Opts} -> gun_socks:check_options(Opts)
			end || P <- Protocols, is_tuple(P)],
			case lists:usort(TupleCheck) of
				[] -> ok;
				[ok] -> ok;
				_ -> error
			end
	end.

consider_tracing(ServerPid, #{trace := true}) ->
	_ = dbg:tracer(),
	_ = dbg:tpl(gun, [{'_', [], [{return_trace}]}]),
	_ = dbg:tpl(gun_http, [{'_', [], [{return_trace}]}]),
	_ = dbg:tpl(gun_http2, [{'_', [], [{return_trace}]}]),
	_ = dbg:tpl(gun_http3, [{'_', [], [{return_trace}]}]),
	_ = dbg:tpl(gun_raw, [{'_', [], [{return_trace}]}]),
	_ = dbg:tpl(gun_socks, [{'_', [], [{return_trace}]}]),
	_ = dbg:tpl(gun_ws, [{'_', [], [{return_trace}]}]),
	_ = dbg:p(ServerPid, all),
	ok;
consider_tracing(_, _) ->
	ok.

-spec set_owner(pid(), pid()) -> ok.
set_owner(ServerPid, NewOwnerPid) ->
	gen_statem:cast(ServerPid, {set_owner, self(), NewOwnerPid}).

-spec info(pid()) -> map().
info(ServerPid) ->
	{CurrentStateName, #state{
		owner=Owner,
		socket=Socket,
		transport=Transport,
		protocol=Protocol,
		origin_scheme=OriginScheme,
		origin_host=OriginHost,
		origin_port=OriginPort,
		intermediaries=Intermediaries,
		cookie_store=CookieStore,
		event_handler=EventHandler,
		event_handler_state=EventHandlerState
	}} = sys:get_state(ServerPid),
	Info0 = #{
		owner => Owner,
		socket => Socket,
		%% @todo This is no longer correct for https because of QUIC.
		transport => case OriginScheme of
			<<"http">> -> tcp;
			<<"https">> -> tls
		end,
		state_name => CurrentStateName,
		origin_scheme => case Protocol of
			gun_raw -> undefined;
			gun_socks -> undefined;
			_ -> OriginScheme
		end,
		origin_host => OriginHost,
		origin_port => OriginPort,
		intermediaries => intermediaries_info(Intermediaries, []),
		cookie_store => CookieStore,
		event_handler => EventHandler,
		event_handler_state => EventHandlerState
	},
	Info = case Socket of
		undefined ->
			Info0;
		_ ->
			case Transport:sockname(Socket) of
				{ok, {SockIP, SockPort}} ->
					Info0#{
						sock_ip => SockIP,
						sock_port => SockPort
					};
				{error, _} ->
					Info0
			end
	end,
	case Protocol of
		undefined -> Info;
		_ -> Info#{protocol => Protocol:name()}
	end.

%% We change tls_proxy into tls for intermediaries.
%%
%% Intermediaries are listed in the order data goes through them,
%% that's why we reverse the order here.
intermediaries_info([], Acc) ->
	Acc;
intermediaries_info([Intermediary=#{transport := Transport0}|Tail], Acc) ->
	Transport = case Transport0 of
		tls_proxy -> tls;
		_ -> Transport0
	end,
	intermediaries_info(Tail, [Intermediary#{transport => Transport}|Acc]).

-spec close(pid()) -> ok.
close(ServerPid) ->
	supervisor:terminate_child(gun_conns_sup, ServerPid).

-spec shutdown(pid()) -> ok.
shutdown(ServerPid) ->
	gen_statem:cast(ServerPid, shutdown).

%% Requests.

-spec delete(pid(), iodata()) -> stream_ref().
delete(ServerPid, Path) ->
	request(ServerPid, <<"DELETE">>, Path, [], <<>>).

-spec delete(pid(), iodata(), req_headers()) -> stream_ref().
delete(ServerPid, Path, Headers) ->
	request(ServerPid, <<"DELETE">>, Path, Headers, <<>>).

-spec delete(pid(), iodata(), req_headers(), req_opts()) -> stream_ref().
delete(ServerPid, Path, Headers, ReqOpts) ->
	request(ServerPid, <<"DELETE">>, Path, Headers, <<>>, ReqOpts).

-spec get(pid(), iodata()) -> stream_ref().
get(ServerPid, Path) ->
	request(ServerPid, <<"GET">>, Path, [], <<>>).

-spec get(pid(), iodata(), req_headers()) -> stream_ref().
get(ServerPid, Path, Headers) ->
	request(ServerPid, <<"GET">>, Path, Headers, <<>>).

-spec get(pid(), iodata(), req_headers(), req_opts()) -> stream_ref().
get(ServerPid, Path, Headers, ReqOpts) ->
	request(ServerPid, <<"GET">>, Path, Headers, <<>>, ReqOpts).

-spec head(pid(), iodata()) -> stream_ref().
head(ServerPid, Path) ->
	request(ServerPid, <<"HEAD">>, Path, [], <<>>).

-spec head(pid(), iodata(), req_headers()) -> stream_ref().
head(ServerPid, Path, Headers) ->
	request(ServerPid, <<"HEAD">>, Path, Headers, <<>>).

-spec head(pid(), iodata(), req_headers(), req_opts()) -> stream_ref().
head(ServerPid, Path, Headers, ReqOpts) ->
	request(ServerPid, <<"HEAD">>, Path, Headers, <<>>, ReqOpts).

-spec options(pid(), iodata()) -> stream_ref().
options(ServerPid, Path) ->
	request(ServerPid, <<"OPTIONS">>, Path, [], <<>>).

-spec options(pid(), iodata(), req_headers()) -> stream_ref().
options(ServerPid, Path, Headers) ->
	request(ServerPid, <<"OPTIONS">>, Path, Headers, <<>>).

-spec options(pid(), iodata(), req_headers(), req_opts()) -> stream_ref().
options(ServerPid, Path, Headers, ReqOpts) ->
	request(ServerPid, <<"OPTIONS">>, Path, Headers, <<>>, ReqOpts).

-spec patch(pid(), iodata(), req_headers()) -> stream_ref().
patch(ServerPid, Path, Headers) ->
	headers(ServerPid, <<"PATCH">>, Path, Headers).

-spec patch(pid(), iodata(), req_headers(), iodata() | req_opts()) -> stream_ref().
patch(ServerPid, Path, Headers, ReqOpts) when is_map(ReqOpts) ->
	headers(ServerPid, <<"PATCH">>, Path, Headers, ReqOpts);
patch(ServerPid, Path, Headers, Body) ->
	request(ServerPid, <<"PATCH">>, Path, Headers, Body).

-spec patch(pid(), iodata(), req_headers(), iodata(), req_opts()) -> stream_ref().
patch(ServerPid, Path, Headers, Body, ReqOpts) ->
	request(ServerPid, <<"PATCH">>, Path, Headers, Body, ReqOpts).

-spec post(pid(), iodata(), req_headers()) -> stream_ref().
post(ServerPid, Path, Headers) ->
	headers(ServerPid, <<"POST">>, Path, Headers).

-spec post(pid(), iodata(), req_headers(), iodata() | req_opts()) -> stream_ref().
post(ServerPid, Path, Headers, ReqOpts) when is_map(ReqOpts) ->
	headers(ServerPid, <<"POST">>, Path, Headers, ReqOpts);
post(ServerPid, Path, Headers, Body) ->
	request(ServerPid, <<"POST">>, Path, Headers, Body).

-spec post(pid(), iodata(), req_headers(), iodata(), req_opts()) -> stream_ref().
post(ServerPid, Path, Headers, Body, ReqOpts) ->
	request(ServerPid, <<"POST">>, Path, Headers, Body, ReqOpts).

-spec put(pid(), iodata(), req_headers()) -> stream_ref().
put(ServerPid, Path, Headers) ->
	headers(ServerPid, <<"PUT">>, Path, Headers).

-spec put(pid(), iodata(), req_headers(), iodata() | req_opts()) -> stream_ref().
put(ServerPid, Path, Headers, ReqOpts) when is_map(ReqOpts) ->
	headers(ServerPid, <<"PUT">>, Path, Headers, ReqOpts);
put(ServerPid, Path, Headers, Body) ->
	request(ServerPid, <<"PUT">>, Path, Headers, Body).

-spec put(pid(), iodata(), req_headers(), iodata(), req_opts()) -> stream_ref().
put(ServerPid, Path, Headers, Body, ReqOpts) ->
	request(ServerPid, <<"PUT">>, Path, Headers, Body, ReqOpts).

%% Generic requests interface.
%%
%% @todo Accept a TargetURI map as well as a normal Path.

-spec headers(pid(), iodata(), iodata(), req_headers()) -> stream_ref().
headers(ServerPid, Method, Path, Headers) ->
	headers(ServerPid, Method, Path, Headers, #{}).

-spec headers(pid(), iodata(), iodata(), req_headers(), req_opts()) -> stream_ref().
headers(ServerPid, Method, Path, Headers0, ReqOpts) ->
	Tunnel = get_tunnel(ReqOpts),
	StreamRef = make_stream_ref(Tunnel),
	InitialFlow = maps:get(flow, ReqOpts, infinity),
	ReplyTo = maps:get(reply_to, ReqOpts, self()),
	gen_statem:cast(ServerPid, {headers, ReplyTo, StreamRef,
		Method, Path, normalize_headers(Headers0), InitialFlow}),
	StreamRef.

-spec request(pid(), iodata(), iodata(), req_headers(), iodata()) -> stream_ref().
request(ServerPid, Method, Path, Headers, Body) ->
	request(ServerPid, Method, Path, Headers, Body, #{}).

-spec request(pid(), iodata(), iodata(), req_headers(), iodata(), req_opts()) -> stream_ref().
request(ServerPid, Method, Path, Headers, Body, ReqOpts) ->
	Tunnel = get_tunnel(ReqOpts),
	StreamRef = make_stream_ref(Tunnel),
	InitialFlow = maps:get(flow, ReqOpts, infinity),
	ReplyTo = maps:get(reply_to, ReqOpts, self()),
	gen_statem:cast(ServerPid, {request, ReplyTo, StreamRef,
		Method, Path, normalize_headers(Headers), Body, InitialFlow}),
	StreamRef.

get_tunnel(#{tunnel := Tunnel}) when is_reference(Tunnel) ->
	[Tunnel];
get_tunnel(#{tunnel := Tunnel}) ->
	Tunnel;
get_tunnel(_) ->
	undefined.

make_stream_ref(undefined) -> make_ref();
make_stream_ref(Tunnel) -> Tunnel ++ [make_ref()].

normalize_headers([]) ->
	[];
normalize_headers([{Name, Value}|Tail]) when is_binary(Name) ->
	[{string:lowercase(Name), Value}|normalize_headers(Tail)];
normalize_headers([{Name, Value}|Tail]) when is_list(Name) ->
	[{string:lowercase(unicode:characters_to_binary(Name)), Value}|normalize_headers(Tail)];
normalize_headers([{Name, Value}|Tail]) when is_atom(Name) ->
	[{string:lowercase(atom_to_binary(Name, latin1)), Value}|normalize_headers(Tail)];
normalize_headers(Headers) when is_map(Headers) ->
	normalize_headers(maps:to_list(Headers)).

%% Streaming data.

-spec data(pid(), stream_ref(), fin | nofin, iodata()) -> ok.
data(ServerPid, StreamRef, IsFin, Data) ->
	case iolist_size(Data) of
		0 when IsFin =:= nofin ->
			ok;
		_ ->
			gen_statem:cast(ServerPid, {data, self(), StreamRef, IsFin, Data})
	end.

%% User pings.

-spec ping(pid()) -> reference().
ping(ServerPid) ->
	ping(ServerPid, #{}).

-spec ping(pid(), req_opts()) -> reference().
ping(ServerPid, ReqOpts) ->
	Tunnel = get_tunnel(ReqOpts),
	PingRef = make_ref(),
	ReplyTo = maps:get(reply_to, ReqOpts, self()),
	gen_statem:cast(ServerPid, {ping, ReplyTo, Tunnel, PingRef}),
	PingRef.

%% Tunneling.

-spec connect(pid(), connect_destination()) -> stream_ref().
connect(ServerPid, Destination) ->
	connect(ServerPid, Destination, [], #{}).

-spec connect(pid(), connect_destination(), req_headers()) -> stream_ref().
connect(ServerPid, Destination, Headers) ->
	connect(ServerPid, Destination, Headers, #{}).

-spec connect(pid(), connect_destination(), req_headers(), req_opts()) -> stream_ref().
connect(ServerPid, Destination, Headers, ReqOpts) ->
	Tunnel = get_tunnel(ReqOpts),
	StreamRef = make_stream_ref(Tunnel),
	InitialFlow = maps:get(flow, ReqOpts, infinity),
	ReplyTo = maps:get(reply_to, ReqOpts, self()),
	gen_statem:cast(ServerPid, {connect, ReplyTo, StreamRef,
		Destination, Headers, InitialFlow}),
	StreamRef.

%% Awaiting gun messages.

-spec await(pid(), stream_ref()) -> await_result().
await(ServerPid, StreamRef) ->
	MRef = monitor(process, ServerPid),
	Res = await(ServerPid, StreamRef, 5000, MRef),
	demonitor(MRef, [flush]),
	Res.

-spec await(pid(), stream_ref(), timeout() | reference()) -> await_result().
await(ServerPid, StreamRef, MRef) when is_reference(MRef) ->
	await(ServerPid, StreamRef, 5000, MRef);
await(ServerPid, StreamRef, Timeout) ->
	MRef = monitor(process, ServerPid),
	Res = await(ServerPid, StreamRef, Timeout, MRef),
	demonitor(MRef, [flush]),
	Res.

-spec await(pid(), stream_ref(), timeout(), reference()) -> await_result().
await(ServerPid, StreamRef, Timeout, MRef) ->
	receive
		{gun_inform, ServerPid, StreamRef, Status, Headers} ->
			{inform, Status, Headers};
		{gun_response, ServerPid, StreamRef, IsFin, Status, Headers} ->
			{response, IsFin, Status, Headers};
		{gun_data, ServerPid, StreamRef, IsFin, Data} ->
			{data, IsFin, Data};
		{gun_sse, ServerPid, StreamRef, Event} ->
			{sse, Event};
		{gun_trailers, ServerPid, StreamRef, Trailers} ->
			{trailers, Trailers};
		{gun_push, ServerPid, StreamRef, NewStreamRef, Method, URI, Headers} ->
			{push, NewStreamRef, Method, URI, Headers};
		{gun_upgrade, ServerPid, StreamRef, Protocols, Headers} ->
			{upgrade, Protocols, Headers};
		{gun_ws, ServerPid, StreamRef, Frame} ->
			{ws, Frame};
		{gun_tunnel_up, ServerPid, StreamRef, Protocol} ->
			{up, Protocol};
		{gun_notify, ServerPid, Type, Info} ->
			{notify, Type, Info};
		{gun_error, ServerPid, StreamRef, Reason} ->
			{error, {stream_error, Reason}};
		{gun_error, ServerPid, Reason} ->
			{error, {connection_error, Reason}};
		{'DOWN', MRef, process, ServerPid, Reason} ->
			{error, {down, Reason}}
	after Timeout ->
		{error, timeout}
	end.

-spec await_body(pid(), stream_ref()) -> await_body_result().
await_body(ServerPid, StreamRef) ->
	MRef = monitor(process, ServerPid),
	Res = await_body(ServerPid, StreamRef, 5000, MRef, <<>>),
	demonitor(MRef, [flush]),
	Res.

-spec await_body(pid(), stream_ref(), timeout() | reference()) -> await_body_result().
await_body(ServerPid, StreamRef, MRef) when is_reference(MRef) ->
	await_body(ServerPid, StreamRef, 5000, MRef, <<>>);
await_body(ServerPid, StreamRef, Timeout) ->
	MRef = monitor(process, ServerPid),
	Res = await_body(ServerPid, StreamRef, Timeout, MRef, <<>>),
	demonitor(MRef, [flush]),
	Res.

-spec await_body(pid(), stream_ref(), timeout(), reference()) -> await_body_result().
await_body(ServerPid, StreamRef, Timeout, MRef) ->
	await_body(ServerPid, StreamRef, Timeout, MRef, <<>>).

await_body(ServerPid, StreamRef, Timeout, MRef, Acc) ->
	receive
		{gun_data, ServerPid, StreamRef, nofin, Data} ->
			await_body(ServerPid, StreamRef, Timeout, MRef,
				<< Acc/binary, Data/binary >>);
		{gun_data, ServerPid, StreamRef, fin, Data} ->
			{ok, << Acc/binary, Data/binary >>};
		%% It's OK to return trailers here because the client
		%% specifically requested them.
		{gun_trailers, ServerPid, StreamRef, Trailers} ->
			{ok, Acc, Trailers};
		{gun_error, ServerPid, StreamRef, Reason} ->
			{error, {stream_error, Reason}};
		{gun_error, ServerPid, Reason} ->
			{error, {connection_error, Reason}};
		{'DOWN', MRef, process, ServerPid, Reason} ->
			{error, {down, Reason}}
	after Timeout ->
		{error, timeout}
	end.

-spec await_up(pid())
	-> {ok, http | http2 | http3 | raw | socks}
	| {error, {down, any()} | timeout}.
await_up(ServerPid) ->
	MRef = monitor(process, ServerPid),
	Res = await_up(ServerPid, 5000, MRef),
	demonitor(MRef, [flush]),
	Res.

-spec await_up(pid(), reference() | timeout())
	-> {ok, http | http2 | http3 | raw | socks}
	| {error, {down, any()} | timeout}.
await_up(ServerPid, MRef) when is_reference(MRef) ->
	await_up(ServerPid, 5000, MRef);
await_up(ServerPid, Timeout) ->
	MRef = monitor(process, ServerPid),
	Res = await_up(ServerPid, Timeout, MRef),
	demonitor(MRef, [flush]),
	Res.

-spec await_up(pid(), timeout(), reference())
	-> {ok, http | http2 | http3 | raw | socks}
	| {error, {down, any()} | timeout}.
await_up(ServerPid, Timeout, MRef) ->
	receive
		{gun_up, ServerPid, Protocol} ->
			{ok, Protocol};
		{'DOWN', MRef, process, ServerPid, Reason} ->
			{error, {down, Reason}}
	after Timeout ->
		{error, timeout}
	end.

%% Flushing gun messages.

-spec flush(pid() | stream_ref()) -> ok.
flush(ServerPid) when is_pid(ServerPid) ->
	flush_pid(ServerPid);
flush(StreamRef) ->
	flush_ref(StreamRef).

-spec flush_pid(pid()) -> ok.
flush_pid(ServerPid) ->
	receive
		{gun_up, ServerPid, _} ->
			flush_pid(ServerPid);
		{gun_down, ServerPid, _, _, _} ->
			flush_pid(ServerPid);
		{gun_inform, ServerPid, _, _, _} ->
			flush_pid(ServerPid);
		{gun_response, ServerPid, _, _, _, _} ->
			flush_pid(ServerPid);
		{gun_data, ServerPid, _, _, _} ->
			flush_pid(ServerPid);
		{gun_trailers, ServerPid, _, _} ->
			flush_pid(ServerPid);
		{gun_push, ServerPid, _, _, _, _, _, _} ->
			flush_pid(ServerPid);
		{gun_error, ServerPid, _, _} ->
			flush_pid(ServerPid);
		{gun_error, ServerPid, _} ->
			flush_pid(ServerPid);
		{gun_tunnel_up, ServerPid, _, _} ->
			flush_pid(ServerPid);
		{gun_upgrade, ServerPid, _, _, _} ->
			flush_pid(ServerPid);
		{gun_ws, ServerPid, _, _} ->
			flush_pid(ServerPid);
		{'DOWN', _, process, ServerPid, _} ->
			flush_pid(ServerPid)
	after 0 ->
		ok
	end.

-spec flush_ref(stream_ref()) -> ok.
flush_ref(StreamRef) ->
	receive
		{gun_inform, _, StreamRef, _, _} ->
			flush_ref(StreamRef);
		{gun_response, _, StreamRef, _, _, _} ->
			flush_ref(StreamRef);
		{gun_data, _, StreamRef, _, _} ->
			flush_ref(StreamRef);
		{gun_trailers, _, StreamRef, _} ->
			flush_ref(StreamRef);
		{gun_push, _, StreamRef, _, _, _, _, _} ->
			flush_ref(StreamRef);
		{gun_error, _, StreamRef, _} ->
			flush_ref(StreamRef);
		{gun_tunnel_up, _, StreamRef, _} ->
			flush_ref(StreamRef);
		{gun_upgrade, _, StreamRef, _, _} ->
			flush_ref(StreamRef);
		{gun_ws, _, StreamRef, _} ->
			flush_ref(StreamRef)
	after 0 ->
		ok
	end.

%% Flow control.

-spec update_flow(pid(), stream_ref(), pos_integer()) -> ok.
update_flow(ServerPid, StreamRef, Flow) ->
	gen_statem:cast(ServerPid, {update_flow, self(), StreamRef, Flow}).

%% Cancelling a stream.

-spec cancel(pid(), stream_ref()) -> ok.
cancel(ServerPid, StreamRef) ->
	gen_statem:cast(ServerPid, {cancel, self(), StreamRef}).

%% Information about a stream.

-spec stream_info(pid(), stream_ref()) -> {ok, map() | undefined} | {error, not_connected}.
stream_info(ServerPid, StreamRef) ->
	gen_statem:call(ServerPid, {stream_info, StreamRef}).

%% Websocket.

-spec ws_upgrade(pid(), iodata()) -> stream_ref().
ws_upgrade(ServerPid, Path) ->
	ws_upgrade(ServerPid, Path, []).

-spec ws_upgrade(pid(), iodata(), req_headers()) -> stream_ref().
ws_upgrade(ServerPid, Path, Headers) ->
	StreamRef = make_ref(),
	gen_statem:cast(ServerPid, {ws_upgrade, self(), StreamRef, Path, normalize_headers(Headers)}),
	StreamRef.

-spec ws_upgrade(pid(), iodata(), req_headers(), ws_opts()) -> stream_ref().
ws_upgrade(ServerPid, Path, Headers, Opts0) ->
	Tunnel = get_tunnel(Opts0),
	Opts = maps:without([tunnel], Opts0),
	ok = gun_ws:check_options(Opts),
	StreamRef = make_stream_ref(Tunnel),
	ReplyTo = maps:get(reply_to, Opts, self()),
	gen_statem:cast(ServerPid, {ws_upgrade, ReplyTo, StreamRef, Path, normalize_headers(Headers), Opts}),
	StreamRef.

-spec ws_send(pid(), stream_ref(), ws_frame() | [ws_frame()]) -> ok.
ws_send(ServerPid, StreamRef, Frames) ->
	gen_statem:cast(ServerPid, {ws_send, self(), StreamRef, Frames}).

%% Internals.

callback_mode() -> state_functions.

start_link(Owner, Host, Port, Opts) ->
	gen_statem:start_link(?MODULE, {Owner, Host, Port, Opts}, []).

init({Owner, Host, Port, Opts}) ->
	Retry = maps:get(retry, Opts, 5),
	OriginTransport = maps:get(transport, Opts, default_transport(Port)),
	%% When Unix Domain Sockets are used we set
	%% the origin authority to "localhost" by default.
	{OriginHost, OriginPort} = case Host of
		{local, _} ->
			OriginPort0 = case OriginTransport of
				tcp -> 80;
				_ -> 443
			end,
			{<<"localhost">>, OriginPort0};
		_ ->
			{Host, Port}
	end,
	%% The OriginScheme is not really http when we connect to socks/raw.
	%% This is corrected in the gun:info/1 and gun:stream_info/2 functions where applicable.
	{OriginScheme, Transport} = case OriginTransport of
		tcp -> {<<"http">>, gun_tcp};
		tls -> {<<"https">>, gun_tls};
		quic -> {<<"https">>, gun_quicer}
	end,
	OwnerRef = monitor(process, Owner),
	{EvHandler, EvHandlerState0} = maps:get(event_handler, Opts,
		{gun_default_event_h, undefined}),
	EvHandlerState = EvHandler:init(#{
		owner => Owner,
		transport => OriginTransport,
		origin_scheme => OriginScheme,
		origin_host => Host,
		origin_port => Port,
		opts => Opts
	}, EvHandlerState0),
	CookieStore = maps:get(cookie_store, Opts, undefined),
	State = #state{owner=Owner, status={up, OwnerRef},
		host=Host, port=Port, origin_scheme=OriginScheme,
		origin_host=OriginHost, origin_port=OriginPort, opts=Opts,
		transport=Transport, messages=Transport:messages(),
		event_handler=EvHandler, event_handler_state=EvHandlerState,
		cookie_store=CookieStore},
	{ok, domain_lookup, State,
		{next_event, internal, {retries, Retry, not_connected}}}.

default_transport(443) -> tls;
default_transport(_) -> tcp.

not_connected(_, {retries, 0, normal}, State) ->
	{stop, normal, State};
not_connected(_, {retries, 0, Reason}, State) ->
	{stop, {shutdown, Reason}, State};
not_connected(_, {retries, Retries0, _}, State=#state{opts=Opts}) ->
	Fun = maps:get(retry_fun, Opts, fun default_retry_fun/2),
	#{
		timeout := Timeout,
		retries := Retries
	} = Fun(Retries0, Opts),
	{next_state, domain_lookup, State,
		{state_timeout, Timeout, {retries, Retries, not_connected}}};
not_connected({call, From}, {stream_info, _}, _) ->
	{keep_state_and_data, {reply, From, {error, not_connected}}};
not_connected(Type, Event, State) ->
	handle_common(Type, Event, ?FUNCTION_NAME, State).

default_retry_fun(Retries, Opts) ->
	%% We retry immediately after a disconnect.
	Timeout = case maps:get(retry, Opts, 5) of
		Retries -> 0;
		_ -> maps:get(retry_timeout, Opts, 5000)
	end,
	#{
		retries => Retries - 1,
		timeout => Timeout
	}.

domain_lookup(_, {retries, Retries, _}, State=#state{host=Host, port=Port, opts=Opts,
		event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	TransOpts = maps:get(tcp_opts, Opts, [
		{send_timeout, 15000},
		{send_timeout_close, true}
	]),
	DomainLookupTimeout = maps:get(domain_lookup_timeout, Opts, infinity),
	DomainLookupEvent = #{
		host => Host,
		port => Port,
		tcp_opts => TransOpts,
		timeout => DomainLookupTimeout
	},
	EvHandlerState1 = EvHandler:domain_lookup_start(DomainLookupEvent, EvHandlerState0),
	case gun_tcp:domain_lookup(Host, Port, TransOpts, DomainLookupTimeout) of
		{ok, LookupInfo} ->
			EvHandlerState = EvHandler:domain_lookup_end(DomainLookupEvent#{
				lookup_info => LookupInfo
			}, EvHandlerState1),
			{next_state, connecting, State#state{event_handler_state=EvHandlerState},
				{next_event, internal, {retries, Retries, LookupInfo}}};
		{error, Reason} ->
			EvHandlerState = EvHandler:domain_lookup_end(DomainLookupEvent#{
				error => Reason
			}, EvHandlerState1),
			{next_state, not_connected, State#state{event_handler_state=EvHandlerState},
				{next_event, internal, {retries, Retries, Reason}}}
	end;
domain_lookup({call, From}, {stream_info, _}, _) ->
	{keep_state_and_data, {reply, From, {error, not_connected}}};
domain_lookup(Type, Event, State) ->
	handle_common(Type, Event, ?FUNCTION_NAME, State).

connecting(_, {retries, Retries, LookupInfo}, State=#state{opts=Opts,
		transport=gun_quicer, event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	%% @todo We are doing the TLS handshake at the same time,
	%%       we cannot separate it from the connection. Fire events.
	ConnectTimeout = maps:get(connect_timeout, Opts, infinity),
	ConnectEvent = #{
		lookup_info => LookupInfo,
		timeout => ConnectTimeout
	},
	EvHandlerState1 = EvHandler:connect_start(ConnectEvent, EvHandlerState0),
	case gun_quicer:connect(LookupInfo, ConnectTimeout) of
		{ok, Socket} ->
			%% @todo We should double check the ALPN result.
			[Protocol] = maps:get(protocols, Opts, [http3]),
			ProtocolName = case Protocol of
				{P, _} -> P;
				P -> P
			end,
			EvHandlerState = EvHandler:connect_end(ConnectEvent#{
				socket => Socket,
				protocol => ProtocolName
			}, EvHandlerState1),
			{next_state, connected_protocol_init,
				State#state{event_handler_state=EvHandlerState},
				{next_event, internal, {connected, Retries, Socket, Protocol}}};
		{error, Reason} ->
			EvHandlerState = EvHandler:connect_end(ConnectEvent#{
				error => Reason
			}, EvHandlerState1),
			{next_state, not_connected, State#state{event_handler_state=EvHandlerState},
				{next_event, internal, {retries, Retries, Reason}}}
	end;
connecting(_, {retries, Retries, LookupInfo}, State=#state{opts=Opts,
		transport=Transport, event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	ConnectTimeout = maps:get(connect_timeout, Opts, infinity),
	ConnectEvent = #{
		lookup_info => LookupInfo,
		timeout => ConnectTimeout
	},
	EvHandlerState1 = EvHandler:connect_start(ConnectEvent, EvHandlerState0),
	case gun_tcp:connect(LookupInfo, ConnectTimeout) of
		{ok, Socket} when Transport =:= gun_tcp ->
			[Protocol] = maps:get(protocols, Opts, [http]),
			ProtocolName = case Protocol of
				{P, _} -> P;
				P -> P
			end,
			EvHandlerState = EvHandler:connect_end(ConnectEvent#{
				socket => Socket,
				protocol => ProtocolName
			}, EvHandlerState1),
			{next_state, connected_protocol_init,
				State#state{event_handler_state=EvHandlerState},
				{next_event, internal, {connected, Retries, Socket, Protocol}}};
		{ok, Socket} when Transport =:= gun_tls ->
			EvHandlerState = EvHandler:connect_end(ConnectEvent#{
				socket => Socket
			}, EvHandlerState1),
			{next_state, initial_tls_handshake, State#state{event_handler_state=EvHandlerState},
				{next_event, internal, {retries, Retries, Socket}}};
		{error, Reason} ->
			EvHandlerState = EvHandler:connect_end(ConnectEvent#{
				error => Reason
			}, EvHandlerState1),
			{next_state, not_connected, State#state{event_handler_state=EvHandlerState},
				{next_event, internal, {retries, Retries, Reason}}}
	end.

initial_tls_handshake(_, {retries, Retries, Socket}, State0=#state{opts=Opts, origin_host=OriginHost}) ->
	Protocols = maps:get(protocols, Opts, [http2, http]),
	HandshakeEvent = #{
		%% @todo This results in ensure_tls_opts being called twice.
		tls_opts => ensure_tls_opts(Protocols, maps:get(tls_opts, Opts, []), OriginHost),
		timeout => maps:get(tls_handshake_timeout, Opts, infinity)
	},
	case normal_tls_handshake(Socket, State0, HandshakeEvent, Protocols) of
		{ok, TLSSocket, Protocol, State} ->
			{next_state, connected_protocol_init, State,
				{next_event, internal, {connected, Retries, TLSSocket, Protocol}}};
		{error, Reason, State} ->
			{next_state, not_connected, State,
				{next_event, internal, {retries, Retries, Reason}}}
	end.

ensure_tls_opts(Protocols0, TransOpts0, OriginHost) ->
	%% CA certificates.
	TransOpts1 = case lists:keymember(cacerts, 1, TransOpts0) of
		true ->
			TransOpts0;
		false ->
			case lists:keymember(cacertfile, 1, TransOpts0) of
				true ->
					TransOpts0;
				false ->
					%% This function was added in OTP-25. We use it  when it is
					%% available and keep the previous behavior when it isn't.
					case erlang:function_exported(public_key, cacerts_get, 0) of
						true ->
							[{cacerts, public_key:cacerts_get()}|TransOpts0];
						false ->
							TransOpts0
					end
			end
	end,
	%% Wildcard certificate matching.
	TransOpts2 = case lists:keymember(customize_hostname_check, 1, TransOpts1) of
		true ->
			TransOpts1;
		false ->
			HTTPSMatchFun = public_key:pkix_verify_hostname_match_fun(https),
			[{customize_hostname_check, [{match_fun, HTTPSMatchFun}]}|TransOpts1]
	end,
	%% ALPN.
	Protocols = lists:foldl(fun
		(http, Acc) -> [<<"http/1.1">>|Acc];
		({http, _}, Acc) -> [<<"http/1.1">>|Acc];
		(http2, Acc) -> [<<"h2">>|Acc];
		({http2, _}, Acc) -> [<<"h2">>|Acc];
		(_, Acc) -> Acc
	end, [], Protocols0),
	TransOpts = [
		{alpn_advertised_protocols, Protocols}
	|TransOpts2],
	%% SNI.
	%%
	%% Normally only DNS hostnames are supported for SNI. However, the ssl
	%% application itself allows any string through so we do the same.
	%%
	%% Only add SNI if not already present and OriginHost isn't an IP address.
	case lists:keymember(server_name_indication, 1, TransOpts) of
		false when is_list(OriginHost) ->
			[{server_name_indication, OriginHost}|TransOpts];
		false when is_atom(OriginHost) ->
			[{server_name_indication, atom_to_list(OriginHost)}|TransOpts];
		_ ->
			TransOpts
	end.

%% Normal TLS handshake.
tls_handshake(internal, {tls_handshake, HandshakeEvent, Protocols, ReplyTo},
		State0=#state{socket=Socket, transport=gun_tcp}) ->
	StreamRef = maps:get(stream_ref, HandshakeEvent, undefined),
	case normal_tls_handshake(Socket, State0, HandshakeEvent, Protocols) of
		{ok, TLSSocket, NewProtocol0, State} ->
			NewProtocol1 = gun_protocols:add_stream_ref(NewProtocol0, StreamRef),
			NewProtocol = case NewProtocol1 of
				{NewProtocolName, NewProtocolOpts} -> {NewProtocolName, NewProtocolOpts#{tunnel_transport => tls}};
				NewProtocolName -> {NewProtocolName, #{tunnel_transport => tls}}
			end,
			Protocol = gun_protocols:handler(NewProtocol),
			reply(ReplyTo, {gun_tunnel_up, self(), StreamRef, Protocol:name()}),
			commands([
				{switch_transport, gun_tls, TLSSocket},
				{switch_protocol, NewProtocol, ReplyTo, <<>>}
			], State);
		{error, Reason, State} ->
			commands({error, Reason}, State)
	end;
%% TLS over TLS.
tls_handshake(internal, {tls_handshake,
		HandshakeEvent0=#{tls_opts := TLSOpts0, timeout := TLSTimeout}, Protocols, ReplyTo},
		State=#state{socket=Socket, transport=Transport, origin_host=OriginHost, origin_port=OriginPort,
		event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	TLSOpts = ensure_tls_opts(Protocols, TLSOpts0, OriginHost),
	HandshakeEvent = HandshakeEvent0#{
		tls_opts => TLSOpts,
		socket => Socket
	},
	EvHandlerState = EvHandler:tls_handshake_start(HandshakeEvent, EvHandlerState0),
	{ok, ProxyPid} = gun_tls_proxy:start_link(OriginHost, OriginPort,
		TLSOpts, TLSTimeout, Socket, Transport, {HandshakeEvent, Protocols, ReplyTo}),
	commands([{switch_transport, gun_tls_proxy, ProxyPid}], State#state{
		socket=ProxyPid, transport=gun_tls_proxy, event_handler_state=EvHandlerState});
%% When using gun_tls_proxy we need a separate message to know whether
%% the handshake succeeded and whether we need to switch to a different protocol.
tls_handshake(info, {gun_tls_proxy, Socket, {ok, Negotiated}, {HandshakeEvent, Protocols, ReplyTo}},
		State0=#state{socket=Socket, event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	NewProtocol0 = gun_protocols:negotiated(Negotiated, Protocols),
	StreamRef = maps:get(stream_ref, HandshakeEvent, undefined),
	NewProtocol1 = gun_protocols:add_stream_ref(NewProtocol0, StreamRef),
	NewProtocol = case NewProtocol1 of
		{NewProtocolName, NewProtocolOpts} -> {NewProtocolName, NewProtocolOpts#{tunnel_transport => tls}};
		NewProtocolName -> {NewProtocolName, #{tunnel_transport => tls}}
	end,
	Protocol = gun_protocols:handler(NewProtocol),
	reply(ReplyTo, {gun_tunnel_up, self(), StreamRef, Protocol:name()}),
	EvHandlerState = EvHandler:tls_handshake_end(HandshakeEvent#{
		socket => Socket,
		protocol => Protocol:name()
	}, EvHandlerState0),
	commands([{switch_protocol, NewProtocol, ReplyTo, <<>>}], State0#state{event_handler_state=EvHandlerState});
tls_handshake(info, {gun_tls_proxy, Socket, Error = {error, Reason}, {HandshakeEvent, _, _}},
		State=#state{socket=Socket, event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	EvHandlerState = EvHandler:tls_handshake_end(HandshakeEvent#{
		error => Reason
	}, EvHandlerState0),
	commands([Error], State#state{event_handler_state=EvHandlerState});
tls_handshake(Type, Event, State) ->
	handle_common_connected_no_input(Type, Event, ?FUNCTION_NAME, State).

normal_tls_handshake(Socket, State=#state{
		origin_host=OriginHost, event_handler=EvHandler, event_handler_state=EvHandlerState0},
		HandshakeEvent0=#{tls_opts := TLSOpts0, timeout := TLSTimeout}, Protocols) ->
	TLSOpts = ensure_tls_opts(Protocols, TLSOpts0, OriginHost),
	HandshakeEvent = HandshakeEvent0#{
		tls_opts => TLSOpts,
		socket => Socket
	},
	EvHandlerState1 = EvHandler:tls_handshake_start(HandshakeEvent, EvHandlerState0),
	case gun_tls:connect(Socket, TLSOpts, TLSTimeout) of
		{ok, TLSSocket} ->
			%% When initially connecting we are in passive mode and
			%% in that state we expect this call to always succeed.
			%% In rare scenarios (suspended Gun process) it may
			%% return {error,closed}, but this indicates that the
			%% socket process is gone and we cannot retrieve a potential
			%% TLS alert.
			%%
			%% When using HTTP/1.1 CONNECT we are also in passive mode
			%% because CONNECT involves a response that is received via
			%% active mode, which automatically goes into passive mode
			%% ({active,once}), and we only reenable active mode after
			%% processing commands.
			case ssl:negotiated_protocol(TLSSocket) of
				{error, Reason = closed} ->
					EvHandlerState = EvHandler:tls_handshake_end(HandshakeEvent#{
						error => Reason
					}, EvHandlerState1),
					{error, Reason, State#state{event_handler_state=EvHandlerState}};
				NegotiatedProtocol ->
					NewProtocol = gun_protocols:negotiated(NegotiatedProtocol, Protocols),
					Protocol = gun_protocols:handler(NewProtocol),
					EvHandlerState = EvHandler:tls_handshake_end(HandshakeEvent#{
						socket => TLSSocket,
						protocol => Protocol:name()
					}, EvHandlerState1),
					{ok, TLSSocket, NewProtocol,
						State#state{event_handler_state=EvHandlerState}}
			end;
		{error, Reason} ->
			EvHandlerState = EvHandler:tls_handshake_end(HandshakeEvent#{
				error => Reason
			}, EvHandlerState1),
			{error, Reason, State#state{event_handler_state=EvHandlerState}}
	end.

connected_protocol_init(internal, {connected, Retries, Socket, NewProtocol},
		State0=#state{owner=Owner, opts=Opts, transport=Transport}) ->
	{Protocol, ProtoOpts} = gun_protocols:handler_and_opts(NewProtocol, Opts),
	case Protocol:init(Owner, Socket, Transport, ProtoOpts) of
		{error, Reason} ->
			{next_state, not_connected, State0,
				{next_event, internal, {retries, Retries, Reason}}};
		{ok, StateName, ProtoState} ->
			%% @todo Don't send gun_up and gun_down if active/1 fails here.
			reply(Owner, {gun_up, self(), Protocol:name()}),
			State1 = State0#state{socket=Socket, protocol=Protocol,
				protocol_state=ProtoState, active=true},
			case active(State1) of
				{ok, State2} ->
					State = case Protocol:has_keepalive() of
						true -> keepalive_timeout(State2);
						false -> State2
					end,
					{next_state, StateName, State};
				Disconnect ->
					Disconnect
			end
	end.

connected_no_input(Type, Event, State) ->
	handle_common_connected_no_input(Type, Event, ?FUNCTION_NAME, State).

connected_data_only(cast, Msg, _)
		when element(1, Msg) =:= headers; element(1, Msg) =:= request;
			element(1, Msg) =:= connect; element(1, Msg) =:= ws_upgrade;
			element(1, Msg) =:= ws_send ->
	ReplyTo = element(2, Msg),
	reply(ReplyTo, {gun_error, self(), {badstate,
		"This connection does not accept new requests to be opened "
		"nor does it accept Websocket frames."}}),
	keep_state_and_data;
connected_data_only(Type, Event, State) ->
	handle_common_connected(Type, Event, ?FUNCTION_NAME, State).

connected_ws_only(cast, {ws_send, ReplyTo, StreamRef, Frames}, State=#state{
		protocol=Protocol=gun_ws, protocol_state=ProtoState,
		event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	{Commands, EvHandlerState} = Protocol:ws_send(Frames,
		ProtoState, dereference_stream_ref(StreamRef, State),
		ReplyTo, EvHandler, EvHandlerState0),
	commands(Commands, State#state{event_handler_state=EvHandlerState});
connected_ws_only(cast, Msg, _)
		when element(1, Msg) =:= headers; element(1, Msg) =:= request; element(1, Msg) =:= data;
			element(1, Msg) =:= connect; element(1, Msg) =:= ws_upgrade ->
	ReplyTo = element(2, Msg),
	reply(ReplyTo, {gun_error, self(), {badstate,
		"This connection only accepts Websocket frames."}}),
	keep_state_and_data;
connected_ws_only(Type, Event, State) ->
	handle_common_connected_no_input(Type, Event, ?FUNCTION_NAME, State).

%% Public HTTP interface.
%%
%% @todo It might be better, internally, to pass around a URIMap
%% containing the target URI, instead of separate Host/Port/PathWithQs.
connected(cast, {ping, ReplyTo, Tunnel0, PingRef},
		State=#state{protocol=Protocol, protocol_state=ProtoState}) ->
	Tunnel = case dereference_stream_ref(Tunnel0, State) of
		[] -> undefined;
		Tunnel1 -> Tunnel1
	end,
	Commands = Protocol:ping(ProtoState, Tunnel, ReplyTo, PingRef),
	commands(Commands, State);
connected(cast, {headers, ReplyTo, StreamRef, Method, Path, Headers, InitialFlow},
		State=#state{origin_host=Host, origin_port=Port,
			protocol=Protocol, protocol_state=ProtoState, cookie_store=CookieStore0,
			event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	{Commands, CookieStore, EvHandlerState} = Protocol:headers(ProtoState,
		dereference_stream_ref(StreamRef, State), ReplyTo,
		Method, Host, Port, Path, Headers,
		InitialFlow, CookieStore0, EvHandler, EvHandlerState0),
	commands(Commands, State#state{cookie_store=CookieStore,
		event_handler_state=EvHandlerState});
connected(cast, {request, ReplyTo, StreamRef, Method, Path, Headers, Body, InitialFlow},
		State=#state{origin_host=Host, origin_port=Port,
			protocol=Protocol, protocol_state=ProtoState, cookie_store=CookieStore0,
			event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	{Commands, CookieStore, EvHandlerState} = Protocol:request(ProtoState,
		dereference_stream_ref(StreamRef, State), ReplyTo,
		Method, Host, Port, Path, Headers, Body,
		InitialFlow, CookieStore0, EvHandler, EvHandlerState0),
	commands(Commands, State#state{cookie_store=CookieStore,
		event_handler_state=EvHandlerState});
connected(cast, {connect, ReplyTo, StreamRef, Destination, Headers, InitialFlow},
		State=#state{origin_host=Host, origin_port=Port,
			protocol=Protocol, protocol_state=ProtoState, cookie_store=CookieStore0,
			event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	{Commands, CookieStore, EvHandlerState} = Protocol:connect(ProtoState,
		dereference_stream_ref(StreamRef, State), ReplyTo,
		Destination, #{host => Host, port => Port},
		Headers, InitialFlow, CookieStore0, EvHandler, EvHandlerState0),
	commands(Commands, State#state{cookie_store=CookieStore,
		event_handler_state=EvHandlerState});
%% Public Websocket interface.
connected(cast, {ws_upgrade, ReplyTo, StreamRef, Path, Headers}, State=#state{opts=Opts}) ->
	WsOpts = maps:get(ws_opts, Opts, #{}),
	connected(cast, {ws_upgrade, ReplyTo, StreamRef, Path, Headers, WsOpts}, State);
connected(cast, {ws_upgrade, ReplyTo, StreamRef, Path, Headers, WsOpts},
		State=#state{origin_host=Host, origin_port=Port,
			protocol=Protocol, protocol_state=ProtoState, cookie_store=CookieStore0,
			event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	EvHandlerState1 = EvHandler:ws_upgrade(#{
		stream_ref => StreamRef,
		reply_to => ReplyTo,
		opts => WsOpts
	}, EvHandlerState0),
	%% @todo Can fail if HTTP/1.0.
	{Commands, CookieStore, EvHandlerState} = Protocol:ws_upgrade(ProtoState,
		dereference_stream_ref(StreamRef, State), ReplyTo,
		Host, Port, Path, Headers, WsOpts, CookieStore0, EvHandler, EvHandlerState1),
	commands(Commands, State#state{cookie_store=CookieStore,
		event_handler_state=EvHandlerState});
%% @todo Maybe better standardize the protocol callbacks argument orders.
connected(cast, {ws_send, ReplyTo, StreamRef, Frames}, State=#state{
		protocol=Protocol, protocol_state=ProtoState,
		event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	{Commands, EvHandlerState} = Protocol:ws_send(Frames,
		ProtoState, dereference_stream_ref(StreamRef, State),
		ReplyTo, EvHandler, EvHandlerState0),
	commands(Commands, State#state{event_handler_state=EvHandlerState});
connected(Type, Event, State) ->
	handle_common_connected(Type, Event, ?FUNCTION_NAME, State).

%% When the origin is using raw we do not dereference the stream_ref
%% because it expects the full stream_ref to function (there's no
%% other stream involved for this connection).
dereference_stream_ref(StreamRef, #state{protocol=gun_raw}) ->
	StreamRef;
dereference_stream_ref(StreamRef, #state{intermediaries=Intermediaries}) ->
	%% @todo It would be better to validate with the intermediary's stream_refs.
	case length([http || #{protocol := http} <- Intermediaries]) of
		0 ->
			StreamRef;
		N ->
			{_, Tail} = lists:split(N, StreamRef),
			case Tail of
				[SR] -> SR;
				_ -> Tail
			end
	end.

%% Switch to the graceful connection close state.
closing(State=#state{protocol=Protocol, protocol_state=ProtoState,
		event_handler=EvHandler, event_handler_state=EvHandlerState0}, Reason) ->
	{Commands, EvHandlerState} = Protocol:closing(Reason, ProtoState, EvHandler, EvHandlerState0),
	commands(Commands, State#state{event_handler_state=EvHandlerState}).

%% @todo Should explicitly reject ws_send in this state?
closing(state_timeout, closing_timeout, State=#state{status=Status}) ->
	Reason = case Status of
		shutdown -> shutdown;
		{down, _} -> owner_down;
		_ -> normal
	end,
	disconnect(State, Reason);
%% When reconnect is disabled, fail HTTP/Websocket operations immediately.
closing(cast, {headers, ReplyTo, StreamRef, _Method, _Path, _Headers, _InitialFlow},
		State=#state{opts=#{retry := 0}}) ->
	reply(ReplyTo, {gun_error, self(), StreamRef, closing}),
	{keep_state, State};
closing(cast, {request, ReplyTo, StreamRef, _Method, _Path, _Headers, _Body, _InitialFlow},
		State=#state{opts=#{retry := 0}}) ->
	reply(ReplyTo, {gun_error, self(), StreamRef, closing}),
	{keep_state, State};
closing(cast, {connect, ReplyTo, StreamRef, _Destination, _Headers, _InitialFlow},
		State=#state{opts=#{retry := 0}}) ->
	reply(ReplyTo, {gun_error, self(), StreamRef, closing}),
	{keep_state, State};
closing(cast, {ws_upgrade, ReplyTo, StreamRef, _Path, _Headers},
		State=#state{opts=#{retry := 0}}) ->
	reply(ReplyTo, {gun_error, self(), StreamRef, closing}),
	{keep_state, State};
closing(cast, {ws_upgrade, ReplyTo, StreamRef, _Path, _Headers, _WsOpts},
		State=#state{opts=#{retry := 0}}) ->
	reply(ReplyTo, {gun_error, self(), StreamRef, closing}),
	{keep_state, State};
closing(Type, Event, State) ->
	handle_common_connected(Type, Event, ?FUNCTION_NAME, State).

%% Common events when we have a connection.
%%
%% One function accepts new input, the other doesn't.

%% @todo Do we want to reject ReplyTo if it's not the process
%% who initiated the connection? For both data and cancel.
handle_common_connected(cast, {data, ReplyTo, StreamRef, IsFin, Data}, _,
		State=#state{protocol=Protocol, protocol_state=ProtoState,
			event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	{Commands, EvHandlerState} = Protocol:data(ProtoState,
		dereference_stream_ref(StreamRef, State),
		ReplyTo, IsFin, Data, EvHandler, EvHandlerState0),
	commands(Commands, State#state{event_handler_state=EvHandlerState});
handle_common_connected(info, {timeout, TRef, Name}, _,
		State=#state{protocol=Protocol, protocol_state=ProtoState}) ->
	Commands = Protocol:timeout(ProtoState, Name, TRef),
	commands(Commands, State);
handle_common_connected(Type, Event, StateName, StateData) ->
	handle_common_connected_no_input(Type, Event, StateName, StateData).

%% Socket events.
handle_common_connected_no_input(info, Msg, _, State=#state{
		protocol=Protocol=gun_http3, protocol_state=ProtoState, cookie_store=CookieStore0,
		event_handler=EvHandler, event_handler_state=EvHandlerState0})
		when element(1, Msg) =:= quic ->
%	ct:pal("~p", [Msg]),
	{Commands, CookieStore, EvHandlerState} = Protocol:handle(Msg,
		ProtoState, CookieStore0, EvHandler, EvHandlerState0),
	maybe_active(commands(Commands, State#state{cookie_store=CookieStore,
		event_handler_state=EvHandlerState}));
handle_common_connected_no_input(info, {OK, Socket, Data}, _,
		State=#state{socket=Socket, messages={OK, _, _},
		protocol=Protocol, protocol_state=ProtoState, cookie_store=CookieStore0,
		event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	{Commands, CookieStore, EvHandlerState} = Protocol:handle(Data,
		ProtoState, CookieStore0, EvHandler, EvHandlerState0),
	maybe_active(commands(Commands, State#state{cookie_store=CookieStore,
		event_handler_state=EvHandlerState}));
handle_common_connected_no_input(info, {Closed, Socket}, _,
		State=#state{socket=Socket, messages={_, Closed, _}}) ->
	disconnect(State, closed);
handle_common_connected_no_input(info, {Error, Socket, Reason}, _,
		State=#state{socket=Socket, messages={_, _, Error}}) ->
	disconnect(State, {error, Reason});
%% Socket events from TLS proxy sockets set up by HTTP/2 CONNECT.
%% We always forward the messages to Protocol:handle_continue.
handle_common_connected_no_input(info,
		Msg={gun_tls_proxy, _, _, {handle_continue, StreamRef, _, _}}, _,
		State0=#state{protocol=Protocol, protocol_state=ProtoState, cookie_store=CookieStore0,
			event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	{Commands, CookieStore, EvHandlerState} = Protocol:handle_continue(
		dereference_stream_ref(StreamRef, State0),
		Msg, ProtoState, CookieStore0, EvHandler, EvHandlerState0),
	maybe_active(commands(Commands, State0#state{cookie_store=CookieStore,
		event_handler_state=EvHandlerState}));
handle_common_connected_no_input(info, {handle_continue, StreamRef, Msg}, _,
		State0=#state{protocol=Protocol, protocol_state=ProtoState, cookie_store=CookieStore0,
			event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	{Commands, CookieStore, EvHandlerState} = Protocol:handle_continue(
		dereference_stream_ref(StreamRef, State0),
		Msg, ProtoState, CookieStore0, EvHandler, EvHandlerState0),
	maybe_active(commands(Commands, State0#state{cookie_store=CookieStore,
		event_handler_state=EvHandlerState}));
%% Timeouts.
handle_common_connected_no_input(info, keepalive, _,
		State=#state{protocol=Protocol, protocol_state=ProtoState0,
		event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	{Commands, EvHandlerState} = Protocol:keepalive(ProtoState0, EvHandler, EvHandlerState0),
	commands(Commands, keepalive_timeout(State#state{
		event_handler_state=EvHandlerState}));
handle_common_connected_no_input(cast, {update_flow, ReplyTo, StreamRef, Flow}, _,
		State0=#state{protocol=Protocol, protocol_state=ProtoState}) ->
	Commands = Protocol:update_flow(ProtoState, ReplyTo, StreamRef, Flow),
	maybe_active(commands(Commands, State0));
handle_common_connected_no_input(cast, {cancel, ReplyTo, StreamRef}, _,
		State=#state{protocol=Protocol, protocol_state=ProtoState,
		event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	{Commands, EvHandlerState} = Protocol:cancel(ProtoState,
		dereference_stream_ref(StreamRef, State), ReplyTo, EvHandler, EvHandlerState0),
	commands(Commands, State#state{event_handler_state=EvHandlerState});
handle_common_connected_no_input({call, From}, {stream_info, StreamRef}, _,
		State=#state{intermediaries=Intermediaries0, protocol=Protocol, protocol_state=ProtoState}) ->
	Intermediaries = [I || I=#{protocol := http} <- Intermediaries0],
	{keep_state_and_data, {reply, From,
		if
			%% The stream_ref refers to an intermediary.
			length(StreamRef) =< length(Intermediaries) ->
				Intermediary = lists:nth(length(StreamRef), lists:reverse(Intermediaries)),
				{Intermediaries1, Tail} = lists:splitwith(
					fun(Int) -> Int =/= Intermediary end,
					lists:reverse(Intermediaries0)),
				Tunnel = tunnel_info_from_intermediaries(State, Tail),
				{ok, #{
					ref => StreamRef,
					reply_to => undefined, %% @todo
					state => running,
					intermediaries => intermediaries_info(lists:reverse(Intermediaries1), []),
					tunnel => Tunnel
				}};
			is_reference(StreamRef), Intermediaries =/= [] ->
				%% We take all intermediaries up to the first CONNECT intermediary.
				{Intermediaries1, Tail} = lists:splitwith(
					fun(#{protocol := P}) -> P =:= socks end,
					lists:reverse(Intermediaries0)),
				Tunnel = tunnel_info_from_intermediaries(State, Tail),
				{ok, #{
					ref => StreamRef,
					reply_to => undefined, %% @todo
					state => running,
					intermediaries => intermediaries_info(lists:reverse(Intermediaries1), []),
					tunnel => Tunnel
				}};
			true ->
				case Protocol:stream_info(ProtoState, dereference_stream_ref(StreamRef, State)) of
					{ok, undefined} ->
						{ok, undefined};
					{ok, Info0} ->
						Info = Info0#{ref => StreamRef},
						case Intermediaries0 of
							[] ->
								{ok, Info};
							_ ->
								Tail = maps:get(intermediaries, Info, []),
								{ok, Info#{
									intermediaries => intermediaries_info(Intermediaries0, []) ++ Tail
								}}
						end
				end
		end
	}};
handle_common_connected_no_input(Type, Event, StateName, State) ->
	handle_common(Type, Event, StateName, State).

maybe_active({keep_state, State0}) ->
	case active(State0) of
		{ok, State} ->
			{keep_state, State};
		Disconnect ->
			Disconnect
	end;
maybe_active({next_state, closing, State0, Actions}) ->
	case active(State0) of
		{ok, State} ->
			{next_state, closing, State, Actions};
		Disconnect ->
			Disconnect
	end;
maybe_active(Other) ->
	Other.

active(State=#state{active=false}) ->
	{ok, State};
active(State=#state{transport=gun_quicer}) ->
	{ok, State};
active(State=#state{socket=Socket, transport=Transport}) ->
	case Transport:setopts(Socket, [{active, once}]) of
		ok ->
			{ok, State};
		{error, closed} ->
			disconnect(State, closed);
		Error = {error, _} ->
			disconnect(State, Error)
	end.

tunnel_info_from_intermediaries(State, Tail) ->
	case Tail of
		%% If the next endpoint is an intermediary take its infos.
		[_, Intermediary|_] ->
			#{
				host := IntermediaryHost,
				port := IntermediaryPort,
				transport := IntermediaryTransport,
				protocol := IntermediaryProtocol
			} = Intermediary,
			#{
				transport => IntermediaryTransport,
				protocol => IntermediaryProtocol,
				origin_scheme => case IntermediaryTransport of
					tcp -> <<"http">>;
					tls -> <<"https">>
				end,
				origin_host => IntermediaryHost,
				origin_port => IntermediaryPort
			};
		%% Otherwise take the infos from the state.
		_ ->
			tunnel_info_from_state(State)
	end.

tunnel_info_from_state(#state{origin_scheme=OriginScheme,
		origin_host=OriginHost, origin_port=OriginPort, protocol=Proto}) ->
	#{
		transport => case OriginScheme of
			<<"http">> -> tcp;
			<<"https">> -> tls
		end,
		protocol => Proto:name(),
		origin_scheme => case Proto of
			gun_raw -> undefined;
			_ -> OriginScheme
		end,
		origin_host => OriginHost,
		origin_port => OriginPort
	}.

%% Common events.
handle_common(cast, {set_owner, CurrentOwner, NewOwner}, _,
		State=#state{owner=CurrentOwner, status={up, CurrentOwnerRef}}) ->
	%% @todo This should probably trigger an event.
	demonitor(CurrentOwnerRef, [flush]),
	NewOwnerRef = monitor(process, NewOwner),
	{keep_state, State#state{owner=NewOwner, status={up, NewOwnerRef}}};
%% We cannot change the owner when we are shutting down.
handle_common(cast, {set_owner, CurrentOwner, _}, _, #state{owner=CurrentOwner}) ->
	reply(CurrentOwner, {gun_error, self(), {badstate,
		"The owner of the connection cannot be changed when the connection is shutting down."}}),
	keep_state_and_state;
handle_common(cast, shutdown, StateName, State=#state{
		status=Status, socket=Socket, transport=Transport, protocol=Protocol}) ->
	case {Socket, Protocol} of
		{undefined, _} ->
			{stop, shutdown};
		{_, undefined} ->
			%% @todo This is missing the disconnect/terminate events.
			Transport:close(Socket),
			{stop, shutdown};
		_ when StateName =:= closing, element(1, Status) =:= up ->
			{keep_state, status(State, shutdown)};
		_ when StateName =:= closing ->
			keep_state_and_data;
		_ ->
			closing(status(State, shutdown), shutdown)
	end;
%% We stop when the owner is down.
%% @todo We need to demonitor/flush when the status is no longer up.
handle_common(info, {'DOWN', OwnerRef, process, Owner, Reason}, StateName, State=#state{
		owner=Owner, status={up, OwnerRef}, socket=Socket, transport=Transport, protocol=Protocol}) ->
	case Socket of
		undefined ->
			owner_down(Reason, State);
		_ ->
			case Protocol of
				undefined ->
					%% @todo This is missing the disconnect/terminate events.
					Transport:close(Socket),
					owner_down(Reason, State);
				%% We are already closing so no need to initiate closing again.
				_ when StateName =:= closing ->
					{keep_state, status(State, {down, Reason})};
				_ ->
					closing(status(State, {down, Reason}), owner_down)
			end
	end;
handle_common({call, From}, _, _, _) ->
	{keep_state_and_data, {reply, From, {error, bad_call}}};
%% We postpone all HTTP/Websocket operations until we are connected.
handle_common(cast, _, StateName, _) when StateName =/= connected ->
	{keep_state_and_data, postpone};
handle_common(Type, Event, StateName, StateData) ->
	error_logger:error_msg("Unexpected event in state ~p of type ~p:~n~w~n~p~n",
		[StateName, Type, Event, StateData]),
	keep_state_and_data.

commands(Command, State) when not is_list(Command) ->
	commands([Command], State);
commands([], State) ->
	{keep_state, State};
commands([close|_], State) ->
	disconnect(State, normal);
commands([{closing, Timeout}|_], State) ->
	{next_state, closing, keepalive_cancel(State),
		{state_timeout, Timeout, closing_timeout}};
commands([Error={error, _}|_], State) ->
	disconnect(State, Error);
commands([{active, Active}|Tail], State) when is_boolean(Active) ->
	commands(Tail, State#state{active=Active});
commands([{state, ProtoState}|Tail], State) ->
	commands(Tail, State#state{protocol_state=ProtoState});
%% Order is important: the origin must be changed before
%% the transport and/or protocol in order to keep track
%% of the intermediaries properly.
commands([{origin, Scheme, Host, Port, Type}|Tail],
		State=#state{protocol=Protocol, origin_scheme=IntermediateScheme,
			origin_host=IntermediateHost, origin_port=IntermediatePort, intermediaries=Intermediaries,
			event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	EvHandlerState = EvHandler:origin_changed(#{
		type => Type,
		origin_scheme => Scheme,
		origin_host => Host,
		origin_port => Port
	}, EvHandlerState0),
	Info = #{
		type => Type,
		host => IntermediateHost,
		port => IntermediatePort,
		transport => case IntermediateScheme of
			<<"http">> -> tcp;
			<<"https">> -> tls
		end,
		protocol => Protocol:name()
	},
	commands(Tail, State#state{origin_scheme=Scheme,
		origin_host=Host, origin_port=Port, intermediaries=[Info|Intermediaries],
		event_handler_state=EvHandlerState});
commands([{switch_transport, Transport, Socket}|Tail], State0=#state{
		protocol=Protocol, protocol_state=ProtoState0}) ->
	ProtoState = Protocol:switch_transport(Transport, Socket, ProtoState0),
	State1 = State0#state{socket=Socket, transport=Transport,
		messages=Transport:messages(), protocol_state=ProtoState},
	case active(State1) of
		{ok, State} ->
			commands(Tail, State);
		Disconnect ->
			Disconnect
	end;
commands([{switch_protocol, NewProtocol, ReplyTo, Buffer}], State0=#state{
		opts=Opts, socket=Socket, transport=Transport, messages={OK, _, _},
		event_handler=EvHandler, event_handler_state=EvHandlerState0}) ->
	{Protocol, ProtoOpts0} = gun_protocols:handler_and_opts(NewProtocol, Opts),
	ProtoOpts = case ProtoOpts0 of
		#{tunnel_transport := _} -> ProtoOpts0;
		_ -> ProtoOpts0#{tunnel_transport => tcp}
	end,
	%% @todo Handle error result from Protocol:init/4
	{ok, StateName, ProtoState} = Protocol:init(ReplyTo, Socket, Transport, ProtoOpts),
	ProtocolChangedEvent = case ProtoOpts of
		#{stream_ref := StreamRef} ->
			#{stream_ref => StreamRef, protocol => Protocol:name()};
		_ ->
			#{protocol => Protocol:name()}
	end,
	EvHandlerState = EvHandler:protocol_changed(ProtocolChangedEvent, EvHandlerState0),
	%% We cancel the existing keepalive and, depending on the protocol,
	%% we enable keepalive again, effectively resetting the timer.
	State1 = State0#state{protocol=Protocol, protocol_state=ProtoState,
		event_handler_state=EvHandlerState},
	case active(State1) of
		{ok, State2} ->
			State = keepalive_cancel(State2),
			Actions = case Buffer of
				<<>> -> [];
				_ -> [{next_event, info, {OK, Socket, Buffer}}]
			end,
			case Protocol:has_keepalive() of
				true -> {next_state, StateName, keepalive_timeout(State), Actions};
				false -> {next_state, StateName, State, Actions}
			end;
		Disconnect ->
			Disconnect
	end;
%% Perform a TLS handshake.
commands([TLSHandshake={tls_handshake, _, _, _}], State) ->
	{next_state, tls_handshake, State,
		{next_event, internal, TLSHandshake}}.

disconnect(State0=#state{owner=Owner, status=Status, opts=Opts,
		intermediaries=Intermediaries, socket=Socket, transport=Transport0,
		protocol=Protocol, protocol_state=ProtoState,
		event_handler=EvHandler, event_handler_state=EvHandlerState0}, Reason0) ->
	Reason = maybe_tls_alert(State0, Reason0),
	EvHandlerState1 = Protocol:close(Reason, ProtoState, EvHandler, EvHandlerState0),
	_ = Transport0:close(Socket),
	EvHandlerState = EvHandler:disconnect(#{reason => Reason}, EvHandlerState1),
	State1 = State0#state{event_handler_state=EvHandlerState},
	case Status of
		{down, DownReason} ->
			owner_down(DownReason, State1);
		shutdown ->
			{stop, shutdown, State1};
		{up, _} ->
			%% We closed the socket, discard any remaining socket events.
			disconnect_flush(State1),
			KilledStreams = Protocol:down(ProtoState),
			%% @todo Reason here may be {error, Reason1} which leads to
			%% different behavior compared to down messages received
			%% from failing to connect where Reason1 is what gets sent.
			reply(Owner, {gun_down, self(), Protocol:name(), Reason, KilledStreams}),
			Retry = maps:get(retry, Opts, 5),
			State2 = keepalive_cancel(State1#state{
				socket=undefined, protocol=undefined, protocol_state=undefined}),
			State = case Intermediaries of
				[] ->
					State2;
				_ ->
					#{host := OriginHost, port := OriginPort,
						transport := OriginTransport} = lists:last(Intermediaries),
					{OriginScheme, Transport} = case OriginTransport of
						tcp -> {<<"http">>, gun_tcp};
						tls -> {<<"https">>, gun_tls}
					end,
					State2#state{transport=Transport, origin_scheme=OriginScheme,
						origin_host=OriginHost, origin_port=OriginPort,
						intermediaries=[]}
			end,
			{next_state, not_connected, State,
				{next_event, internal, {retries, Retry, Reason}}}
	end.

%% With TLS 1.3 the handshake may not have validated the certificate
%% by the time it completes. The validation may therefore fail at any
%% time afterwards. TLS 1.3 also introduced post-handshake authentication
%% which would produce the same results. Erlang/OTP's ssl has a number
%% of asynchronous functions which won't return the alert as an error
%% and instead return a plain {error,closed}, including ssl:send.
%% Gun must therefore check whether a close is resulting from a TLS alert
%% and use that alert as a more descriptive disconnect reason.
%%
%% Sometimes, ssl:send will return {error,einval}, because while the
%% TLS pseudo-socket still exists, the underlying TCP socket is already
%% gone. In that case we can still query the TLS pseudo-socket to get
%% the detailed TLS alert.
%%
%% @todo We currently do not support retrieving the alert from a gun_tls_proxy
%% socket. We need a test case to best understand what should be done there.
%% But since the socket belongs to that process we likely need additional
%% changes there to make it work.
maybe_tls_alert(#state{socket=Socket, transport=gun_tls,
		active=true, messages={_, _, Error}}, Reason0)
		%% The unwrapped tuple we get half the time makes this clause more complex.
		when Reason0 =:= {error, closed}; Reason0 =:= {error, einval}; Reason0 =:= closed ->
	%% When active mode is enabled we should have the alert in our
	%% mailbox so we can just retrieve it. In case it is late we
	%% use a short timeout to increase the chances of catching it.
	receive
		{Error, Socket, Reason} ->
			Reason
	after 200 ->
		Reason0
	end;
maybe_tls_alert(#state{socket=Socket, transport=Transport=gun_tls,
		active=false}, Reason0)
		when Reason0 =:= {error, closed}; Reason0 =:= {error, einval}; Reason0 =:= closed ->
	%% When active mode is disabled we can do a number of operations to
	%% receive the alert. Enabling active mode is one of them.
	case Transport:setopts(Socket, [{active, once}]) of
		{error, Reason={tls_alert, _}} ->
			Reason;
		_ ->
			Reason0
	end;
%% We unwrap the TLS alert error for consistency.
%% @todo Consistenly wrap/unwrap all errors instead of just this one.
maybe_tls_alert(_, {error, Reason={tls_alert, _}}) ->
	Reason;
%% We may also need to receive the alert when proxying TLS.
maybe_tls_alert(#state{socket=Socket, transport=gun_tls_proxy,
		active=true, messages={_, _, Error}}, Reason0)
		%% The unwrapped tuple we get half the time makes this clause more complex.
		when Reason0 =:= {error, closed}; Reason0 =:= {error, einval}; Reason0 =:= closed ->
	receive
		{Error, Socket, Reason} ->
			Reason
	after 200 ->
		Reason0
	end;
maybe_tls_alert(_, Reason) ->
	Reason.

disconnect_flush(State=#state{socket=Socket, messages={OK, Closed, Error}}) ->
	receive
		{OK, Socket, _} -> disconnect_flush(State);
		{Closed, Socket} -> disconnect_flush(State);
		{Error, Socket, _} -> disconnect_flush(State)
	after 0 ->
		ok
	end.

status(State=#state{status={up, OwnerRef}}, NewStatus) ->
	demonitor(OwnerRef, [flush]),
	State#state{status=NewStatus};
status(State, NewStatus) ->
	State#state{status=NewStatus}.

keepalive_timeout(State=#state{opts=Opts, protocol=Protocol}) ->
	ProtoOpts = maps:get(Protocol:opts_name(), Opts, #{}),
	Keepalive = maps:get(keepalive, ProtoOpts, Protocol:default_keepalive()),
	KeepaliveRef = case Keepalive of
		infinity -> undefined;
		%% @todo Maybe change that to a start_timer.
		_ -> erlang:send_after(Keepalive, self(), keepalive)
	end,
	State#state{keepalive_ref=KeepaliveRef}.

keepalive_cancel(State=#state{keepalive_ref=undefined}) ->
	State;
keepalive_cancel(State=#state{keepalive_ref=KeepaliveRef}) ->
	_ = erlang:cancel_timer(KeepaliveRef),
	%% Flush if we have a keepalive message
	receive
		keepalive -> ok
	after 0 ->
		ok
	end,
	State#state{keepalive_ref=undefined}.

owner_down(normal, State) -> {stop, normal, State};
owner_down(shutdown, State) -> {stop, shutdown, State};
owner_down(Shutdown = {shutdown, _}, State) -> {stop, Shutdown, State};
owner_down(Reason, State) -> {stop, {shutdown, {owner_down, Reason}}, State}.

terminate(Reason, StateName, #state{event_handler=EvHandler,
		event_handler_state=EvHandlerState, cookie_store=Store}) ->
	_ = case Store of
		undefined -> ok;
		%% Optimization: gun_cookies_list isn't a persistent cookie store.
		{gun_cookies_list, _} -> ok;
		_ -> gun_cookies:session_gc(Store)
	end,
	TerminateEvent = #{
		state => StateName,
		reason => Reason
	},
	EvHandler:terminate(TerminateEvent, EvHandlerState).

reply(Pid, Reply) when is_pid(Pid) ->
	Pid ! Reply;
reply({M, F, A}, Reply) when is_atom(M), is_atom(F), is_list(A) ->
	apply(M, F, [Reply|A]);
reply(Fun, Reply) when is_function(Fun, 1) ->
	Fun(Reply);
reply({Fun, A}, Reply) when is_list(A), is_function(Fun, length(A) + 1) ->
	apply(Fun, [Reply|A]).
