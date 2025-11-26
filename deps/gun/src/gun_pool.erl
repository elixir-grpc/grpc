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

-module(gun_pool).
-behaviour(gen_statem).

%% Pools.
-export([start_pool/3]).
-export([stop_pool/2]).
-export([stop_pool/3]).
% @todo shutdown pool?
-export([info/0]).
-export([info/1]).
-export([info/2]).
-export([await_up/1]).
-export([await_up/2]).
-export([checkout/2]). %% Use responsibly!

%% Requests.
-export([delete/2]).
-export([delete/3]).
-export([get/2]).
-export([get/3]).
-export([head/2]).
-export([head/3]).
-export([options/2]).
-export([options/3]).
-export([patch/2]).
-export([patch/3]).
-export([patch/4]).
-export([post/2]).
-export([post/3]).
-export([post/4]).
-export([put/2]).
-export([put/3]).
-export([put/4]).

%% Generic requests interface.
-export([headers/3]).
-export([headers/4]).
-export([request/4]).
-export([request/5]).

%% Streaming data.
-export([data/3]).

%% Tunneling. (HTTP/2+ only.)
%% @todo -export([connect/2]).
%% @todo -export([connect/3]).
%% @todo -export([connect/4]).

%% Cookies.
%% @todo -export([gc_cookies/1]).
%% @todo -export([session_gc_cookies/1]).

%% Awaiting gun messages.
-export([await/1]).
-export([await/2]).
-export([await/3]).
-export([await_body/1]).
-export([await_body/2]).
-export([await_body/3]).

%% Flushing gun messages.
-export([flush/1]).

%% Streams.
-export([update_flow/2]).
-export([cancel/1]).
-export([stream_info/1]).

%% Websocket. (HTTP/2+ only for upgrade.)
%% -export([ws_upgrade/1]).
%% -export([ws_upgrade/2]).
%% -export([ws_upgrade/3]).
-export([ws_send/2]).
%% -export([ws_send/3]). (HTTP/2+ only.)

%% Internals.
-export([callback_mode/0]).
-export([start_link/3]).
-export([init/1]).
-export([degraded/3]).
-export([operational/3]).
-export([terminate/3]).

-type setup_msg() :: {gun_up, pid(), http | http2 | raw | socks}
	| {gun_upgrade, pid(), gun:stream_ref(), [binary()], [{binary(), binary()}]}.

-type opts() :: #{
	conn_opts => gun:opts(),
	scope => any(),
	setup_fun => {fun((pid(), setup_msg(), any()) -> any()), any()},
	size => non_neg_integer()
}.
-export_type([opts/0]).

-type pool_stream_ref() :: {pid(), gun:stream_ref()}.
-export_type([pool_stream_ref/0]).

-type error_result() :: {error, pool_not_found | no_connection_available, atom()}. %% @todo {pool_start_error, SupError}

-type request_result() :: {async, pool_stream_ref()}
	%% @todo {sync, ...} perhaps with Status, Headers, Body, and Extra info such as intermediate responses.
	| error_result().

-type req_opts() :: #{
	%% Options common with normal Gun.
	flow => pos_integer(),
	reply_to => pid(),
%	@todo tunnel => stream_ref(),

	%% Options specific to pools.
	checkout_call_timeout => timeout(),
	checkout_retry => [pos_integer()],
	scope => any(),
	start_pool_if_missing => boolean()
}.
-export_type([req_opts/0]).

-type ws_send_opts() :: #{
	authority => iodata(),

	%% Options specific to pools.
	checkout_call_timeout => timeout(),
	checkout_retry => [pos_integer()],
	scope => any(),
	start_pool_if_missing => boolean()
}.
-export_type([ws_send_opts/0]).

%% @todo tunnel
-type meta() :: #{pid() => #{ws => gun:stream_ref()}}.

-record(state, {
	host :: inet:hostname() | inet:ip_address(),
	port :: inet:port_number(),
	opts :: opts(),
	table :: ets:tid(),
	conns :: #{pid() => down | {setup, any()} | {up, http | http2 | ws | raw, map()}},
	conns_meta = #{} :: meta(),
	await_up = [] :: [{pid(), any()}]
}).

%% Pool management.

-spec start_pool(inet:hostname() | inet:ip_address(), inet:port_number(), opts())
	-> {ok, pid()} | {error, any()}.
start_pool(Host, Port, Opts) ->
	supervisor:start_child(gun_pools_sup, [Host, Port, Opts]).

-spec stop_pool(inet:hostname() | inet:ip_address(), inet:port_number())
	-> ok.
stop_pool(Host, Port) ->
	stop_pool(Host, Port, #{}).

-type stop_opts() :: #{
	scope => any(),
	transport => tcp | tls
}.

-spec stop_pool(inet:hostname() | inet:ip_address(), inet:port_number(), stop_opts())
	-> ok | {error, pool_not_found, atom()}.
stop_pool(Host, Port, StopOpts) ->
	Transport = maps:get(transport, StopOpts, gun:default_transport(Port)),
	Authority = gun_http:host_header(Transport, Host, Port),
	case get_pool(Authority, StopOpts) of
		undefined ->
			{error, pool_not_found,
				'No pool was found for the given scope and authority.'};
		ManagerPid ->
			supervisor:terminate_child(gun_pools_sup, ManagerPid)
	end.

-spec info() -> [map()].
info() ->
	ets:foldl(fun({{Scope, _}, ManagerPid}, Acc) ->
		{StateName, Info} = info(ManagerPid),
		[Info#{scope => Scope, state => StateName}|Acc]
	end, [], gun_pools).

-spec info(pid() | binary()) -> undefined | {degraded | operational, map()}.
info(ManagerPid) when is_pid(ManagerPid) ->
	gen_statem:call(ManagerPid, info);
info(Authority) ->
	info(Authority, default).

-spec info(binary(), any()) -> undefined | {degraded | operational, map()}.
info(Authority, Scope) ->
	case ets:lookup(gun_pools, {Scope, Authority}) of
		[] ->
			undefined;
		[{_, ManagerPid}] ->
			gen_statem:call(ManagerPid, info)
	end.

-spec await_up(pid() | binary()) -> ok | {error, pool_not_found, atom()}.
await_up(ManagerPid) when is_pid(ManagerPid) ->
	gen_statem:call(ManagerPid, await_up, 5000);
await_up(Authority) ->
	await_up(Authority, default).

-spec await_up(binary(), any()) -> ok | {error, pool_not_found, atom()}.
await_up(Authority, Scope) ->
	case ets:lookup(gun_pools, {Scope, Authority}) of
		[] ->
			{error, pool_not_found,
				'No pool was found for the given scope and authority.'};
		[{_, ManagerPid}] ->
			gen_statem:call(ManagerPid, await_up, 5000)
	end.

-spec checkout(pid(), req_opts() | ws_send_opts()) -> undefined | {pid(), map()}.
checkout(ManagerPid, ReqOpts=#{checkout_retry := Retry}) when is_list(Retry) ->
	CallTimeout = maps:get(checkout_call_timeout, ReqOpts, 5000),
	case gen_server:call(ManagerPid, {checkout, ReqOpts}, CallTimeout) of
		undefined ->
			checkout_retry(ManagerPid, ReqOpts, CallTimeout, Retry);
		Result ->
			Result
	end;
checkout(ManagerPid, ReqOpts) ->
	CallTimeout = maps:get(checkout_call_timeout, ReqOpts, 5000),
	gen_server:call(ManagerPid, {checkout, ReqOpts}, CallTimeout).

%% When the checkout_retry option is used, and the first call resulted
%% in no connection being given out, we wait for the configured amount
%% of time then try again. We loop over the wait times until there is
%% none.
checkout_retry(_, _, _, []) ->
	undefined;
checkout_retry(ManagerPid, ReqOpts, CallTimeout, [Wait|Retry]) ->
	timer:sleep(Wait),
	case gen_server:call(ManagerPid, {checkout, ReqOpts}, CallTimeout) of
		undefined ->
			checkout_retry(ManagerPid, ReqOpts, CallTimeout, Retry);
		Result ->
			Result
	end.

%% Requests.

-spec delete(iodata(), gun:req_headers()) -> request_result().
delete(Path, Headers) ->
	request(<<"DELETE">>, Path, Headers, <<>>).

-spec delete(iodata(), gun:req_headers(), req_opts()) -> request_result().
delete(Path, Headers, ReqOpts) ->
	request(<<"DELETE">>, Path, Headers, <<>>, ReqOpts).

-spec get(iodata(), gun:req_headers()) -> request_result().
get(Path, Headers) ->
	request(<<"GET">>, Path, Headers, <<>>).

-spec get(iodata(), gun:req_headers(), req_opts()) -> request_result().
get(Path, Headers, ReqOpts) ->
	request(<<"GET">>, Path, Headers, <<>>, ReqOpts).

-spec head(iodata(), gun:req_headers()) -> request_result().
head(Path, Headers) ->
	request(<<"HEAD">>, Path, Headers, <<>>).

-spec head(iodata(), gun:req_headers(), req_opts()) -> request_result().
head(Path, Headers, ReqOpts) ->
	request(<<"HEAD">>, Path, Headers, <<>>, ReqOpts).

-spec options(iodata(), gun:req_headers()) -> request_result().
options(Path, Headers) ->
	request(<<"OPTIONS">>, Path, Headers, <<>>).

-spec options(iodata(), gun:req_headers(), req_opts()) -> request_result().
options(Path, Headers, ReqOpts) ->
	request(<<"OPTIONS">>, Path, Headers, <<>>, ReqOpts).

-spec patch(iodata(), gun:req_headers()) -> request_result().
patch(Path, Headers) ->
	headers(<<"PATCH">>, Path, Headers).

-spec patch(iodata(), gun:req_headers(), iodata() | req_opts()) -> request_result().
patch(Path, Headers, ReqOpts) when is_map(ReqOpts) ->
	headers(<<"PATCH">>, Path, Headers, ReqOpts);
patch(Path, Headers, Body) ->
	request(<<"PATCH">>, Path, Headers, Body).

-spec patch(iodata(), gun:req_headers(), iodata(), req_opts()) -> request_result().
patch(Path, Headers, Body, ReqOpts) ->
	request(<<"PATCH">>, Path, Headers, Body, ReqOpts).

-spec post(iodata(), gun:req_headers()) -> request_result().
post(Path, Headers) ->
	headers(<<"POST">>, Path, Headers).

-spec post(iodata(), gun:req_headers(), iodata() | req_opts()) -> request_result().
post(Path, Headers, ReqOpts) when is_map(ReqOpts) ->
	headers(<<"POST">>, Path, Headers, ReqOpts);
post(Path, Headers, Body) ->
	request(<<"POST">>, Path, Headers, Body).

-spec post(iodata(), gun:req_headers(), iodata(), req_opts()) -> request_result().
post(Path, Headers, Body, ReqOpts) ->
	request(<<"POST">>, Path, Headers, Body, ReqOpts).

-spec put(iodata(), gun:req_headers()) -> request_result().
put(Path, Headers) ->
	headers(<<"PUT">>, Path, Headers).

-spec put(iodata(), gun:req_headers(), iodata() | req_opts()) -> request_result().
put(Path, Headers, ReqOpts) when is_map(ReqOpts) ->
	headers(<<"PUT">>, Path, Headers, ReqOpts);
put(Path, Headers, Body) ->
	request(<<"PUT">>, Path, Headers, Body).

-spec put(iodata(), gun:req_headers(), iodata(), req_opts()) -> request_result().
put(Path, Headers, Body, ReqOpts) ->
	request(<<"PUT">>, Path, Headers, Body, ReqOpts).

%% Generic requests interface.
%%
%% @todo Accept a TargetURI map as well as a normal Path.

-spec headers(iodata(), iodata(), gun:req_headers()) -> request_result().
headers(Method, Path, Headers) ->
	headers(Method, Path, Headers, #{}).

-spec headers(iodata(), iodata(), gun:req_headers(), req_opts()) -> request_result().
headers(Method, Path, Headers, ReqOpts) ->
	case get_pool(authority(Headers), ReqOpts) of
		undefined ->
			{error, pool_not_found,
				'No pool was found for the given scope and authority.'};
		ManagerPid ->
			case checkout(ManagerPid, ReqOpts) of
				undefined ->
					{error, no_connection_available,
						'No connection in the pool with enough capacity available to open a new stream.'};
				{ConnPid, _Meta} ->
					StreamRef = gun:headers(ConnPid, Method, Path, Headers, ReqOpts),
					%% @todo Synchronous mode.
					{async, {ConnPid, StreamRef}}
			end
	end.

-spec request(iodata(), iodata(), gun:req_headers(), iodata()) -> request_result().
request(Method, Path, Headers, Body) ->
	request(Method, Path, Headers, Body, #{}).

-spec request(iodata(), iodata(), gun:req_headers(), iodata(), req_opts()) -> request_result().
request(Method, Path, Headers, Body, ReqOpts) ->
	case get_pool(authority(Headers), ReqOpts) of
		undefined ->
			{error, pool_not_found,
				'No pool was found for the given scope and authority.'};
		ManagerPid ->
			case checkout(ManagerPid, ReqOpts) of
				undefined ->
					{error, no_connection_available,
						'No connection in the pool with enough capacity available to open a new stream.'};
				{ConnPid, _Meta} ->
					StreamRef = gun:request(ConnPid, Method, Path, Headers, Body, ReqOpts),
					%% @todo Synchronous mode.
					{async, {ConnPid, StreamRef}}
			end
	end.

%% We require the host to be given in the headers for the time being.
%% @todo Allow passing it in options. Websocket send already does that.
authority(#{<<"host">> := Authority}) ->
	Authority;
authority(Headers) ->
	{_, Authority} = lists:keyfind(<<"host">>, 1, Headers),
	Authority.

-spec get_pool(iolist(), req_opts() | ws_send_opts() | stop_opts()) -> pid() | undefined.
get_pool(Authority0, ReqOpts) ->
	Authority = iolist_to_binary(Authority0),
	%% @todo Perhaps rename this to temporary.
	%% There's two concepts: temporary pool is started and stops
	%% when there's no traffic. Dynamic pool simply reduces its number
	%% of connections when there's no traffic.
	StartPoolIfMissing = maps:get(start_pool_if_missing, ReqOpts, false),
	Scope = maps:get(scope, ReqOpts, default),
	case ets:lookup(gun_pools, {Scope, Authority}) of
		[] when StartPoolIfMissing ->
			start_missing_pool(Authority, ReqOpts);
		[] ->
			undefined;
		[{_, ManagerPid}] ->
			%% @todo With temporary pool, getting a pid here doesn't mean the pool can be used.
			%% Indeed the manager could be in process of stopping. I suppose we must
			%% do a check but perhaps it's best to leave that detail to the user
			%% (they can easily retry and recreate the pool if necessary).
			ManagerPid
	end.

start_missing_pool(_Authority, _ReqOpts) ->
	undefined.

%% Streaming data.

-spec data(pool_stream_ref(), fin | nofin, iodata()) -> ok.
data({ConnPid, StreamRef}, IsFin, Data) ->
	gun:data(ConnPid, StreamRef, IsFin, Data).

%% Awaiting gun messages.

-spec await(pool_stream_ref()) -> gun:await_result().
await({ConnPid, StreamRef}) ->
	gun:await(ConnPid, StreamRef).

-spec await(pool_stream_ref(), timeout() | reference()) -> gun:await_result().
await({ConnPid, StreamRef}, MRefOrTimeout) ->
	gun:await(ConnPid, StreamRef, MRefOrTimeout).

-spec await(pool_stream_ref(), timeout(), reference()) -> gun:await_result().
await({ConnPid, StreamRef}, Timeout, MRef) ->
	gun:await(ConnPid, StreamRef, Timeout, MRef).

-spec await_body(pool_stream_ref()) -> gun:await_body_result().
await_body({ConnPid, StreamRef}) ->
	gun:await_body(ConnPid, StreamRef).

-spec await_body(pool_stream_ref(), timeout() | reference()) -> gun:await_body_result().
await_body({ConnPid, StreamRef}, MRefOrTimeout) ->
	gun:await_body(ConnPid, StreamRef, MRefOrTimeout).

-spec await_body(pool_stream_ref(), timeout(), reference()) -> gun:await_body_result().
await_body({ConnPid, StreamRef}, Timeout, MRef) ->
	gun:await_body(ConnPid, StreamRef, Timeout, MRef).

%% Flushing gun messages.

-spec flush(pool_stream_ref()) -> ok.
flush({ConnPid, _}) ->
	gun:flush(ConnPid).

%% Flow control.

-spec update_flow(pool_stream_ref(), pos_integer()) -> ok.
update_flow({ConnPid, StreamRef}, Flow) ->
	gun:update_flow(ConnPid, StreamRef, Flow).

%% Cancelling a stream.

-spec cancel(pool_stream_ref()) -> ok.
cancel({ConnPid, StreamRef}) ->
	gun:cancel(ConnPid, StreamRef).

%% Information about a stream.

-spec stream_info(pool_stream_ref()) -> {ok, map() | undefined} | {error, not_connected}.
stream_info({ConnPid, StreamRef}) ->
	gun:stream_info(ConnPid, StreamRef).

%% Websocket.

-spec ws_send(gun:ws_frame() | [gun:ws_frame()], ws_send_opts()) -> ok | error_result().
ws_send(Frames, WsSendOpts=#{authority := Authority}) ->
	case get_pool(Authority, WsSendOpts) of
		undefined ->
			{error, pool_not_found,
				'No pool was found for the given scope and authority.'};
		ManagerPid ->
			case checkout(ManagerPid, WsSendOpts) of
				undefined ->
					{error, no_connection_available,
						'No connection in the pool with enough capacity available to send Websocket frames.'};
				{ConnPid, #{ws := StreamRef}} ->
					gun:ws_send(ConnPid, StreamRef, Frames)
			end
	end.

%% Pool manager internals.
%%
%% The pool manager is responsible for starting connection processes
%% and restarting them as necessary. It also provides a suitable
%% connection process to any caller that needs it.
%%
%% The pool manager installs an event handler into each connection.
%% The event handler is responsible for counting the number of
%% active streams. It updates the gun_pooled_conns ets table
%% whenever a stream begins or ends.
%%
%% A connection is deemed suitable if it is possible to open new
%% streams. How many streams can be open at any one time depends
%% on the protocol. For HTTP/2 the manager process keeps track of
%% the connection's settings to know the maximum. For non-stream
%% based protocols, there is no limit.
%%
%% The connection to be used is otherwise chosen randomly. The
%% first connection that is suitable is returned. There is no
%% need to "give back" the connection to the manager.

%% @todo
%% What should happen if we always fail to reconnect? I suspect we keep the manager
%% around and propagate errors, the same as if there's no more capacity? Perhaps have alarms?

callback_mode() -> state_functions.

start_link(Host, Port, Opts) ->
	gen_statem:start_link(?MODULE, {Host, Port, Opts}, []).

init({Host, Port, Opts}) ->
	process_flag(trap_exit, true),
	true = ets:insert_new(gun_pools, {gun_pools_key(Host, Port, Opts), self()}),
	Tid = ets:new(gun_pooled_conns, [ordered_set, public]),
	Size = maps:get(size, Opts, 8),
	%% @todo Only start processes in static mode.
	ConnOpts = conn_opts(Tid, Opts),
	Conns = maps:from_list([begin
		{ok, ConnPid} = gun:open(Host, Port, ConnOpts),
		_ = monitor(process, ConnPid),
		{ConnPid, down}
	end || _ <- lists:seq(1, Size)]),
	State = #state{
		host=Host,
		port=Port,
		opts=Opts,
		table=Tid,
		conns=Conns
	},
	%% If Size is 0 then we can never be operational.
	{ok, degraded, State}.

gun_pools_key(Host, Port, Opts) ->
	Transport = maps:get(transport, Opts, gun:default_transport(Port)),
	Authority = gun_http:host_header(Transport, Host, Port),
	Scope = maps:get(scope, Opts, default),
	{Scope, iolist_to_binary(Authority)}.

conn_opts(Tid, Opts) ->
	ConnOpts = maps:get(conn_opts, Opts, #{}),
	EventHandlerState = maps:with([event_handler], ConnOpts),
	H2Opts = maps:get(http2_opts, ConnOpts, #{}),
	ConnOpts#{
		event_handler => {gun_pool_events_h, EventHandlerState#{
			table => Tid
		}},
		http2_opts => H2Opts#{
			notify_settings_changed => true
		}
	}.

%% We use the degraded state as long as at least one connection is degraded.
%% @todo Probably keep count of connections separately to avoid counting every time.
degraded(info, Msg={gun_up, ConnPid, _}, StateData=#state{opts=Opts, conns=Conns}) ->
	#{ConnPid := down} = Conns,
	%% We optionally run the setup function if one is defined. The
	%% setup function tells us whether we are fully up or not. The
	%% setup function may be called repeatedly until the connection
	%% is established.
	%%
	%% @todo It is possible that the connection never does get
	%% fully established. We should deal with this. We probably
	%% need to handle all messages.
	{SetupFun, SetupState0} = setup_fun(Opts),
	degraded_setup(ConnPid, Msg, StateData, SetupFun, SetupState0);
%% @todo
%degraded(info, Msg={gun_tunnel_up, ConnPid, _, _}, StateData0=#state{conns=Conns}) ->
%	;
degraded(info, Msg={gun_upgrade, ConnPid, _, _, _},
		StateData=#state{opts=#{setup_fun := {SetupFun, _}}, conns=Conns}) ->
	%% @todo Probably shouldn't crash if the state is incorrect, that's programmer error though.
	#{ConnPid := {setup, SetupState0}} = Conns,
	%% We run the setup function again using the state previously kept.
	degraded_setup(ConnPid, Msg, StateData, SetupFun, SetupState0);
degraded(Type, Event, StateData) ->
	handle_common(Type, Event, ?FUNCTION_NAME, StateData).

setup_fun(#{setup_fun := SetupFun}) ->
	SetupFun;
setup_fun(_) ->
	{fun (_, {gun_up, _, Protocol}, _) ->
		{up, Protocol, #{}}
	end, undefined}.

degraded_setup(ConnPid, Msg, StateData0=#state{conns=Conns, conns_meta=ConnsMeta,
		await_up=AwaitUp}, SetupFun, SetupState0) ->
	case SetupFun(ConnPid, Msg, SetupState0) of
		Setup={setup, _SetupState} ->
			StateData = StateData0#state{conns=Conns#{ConnPid => Setup}},
			{keep_state, StateData};
		%% The Meta is different from Settings. It allows passing around
		%% Websocket or tunnel stream refs.
		{up, Protocol, Meta} ->
			Settings = #{},
			StateData = StateData0#state{
				conns=Conns#{ConnPid => {up, Protocol, Settings}},
				conns_meta=ConnsMeta#{ConnPid => Meta}
			},
			case is_degraded(StateData) of
				true -> {keep_state, StateData};
				false -> {next_state, operational, StateData#state{await_up=[]},
					[{reply, ReplyTo, ok} || ReplyTo <- AwaitUp]}
			end
	end.

is_degraded(#state{conns=Conns0}) ->
	Conns = maps:to_list(Conns0),
	Len = length(Conns),
	Ups = [up || {_, {up, _, _}} <- Conns],
	Len =/= length(Ups).

operational(Type, Event, StateData) ->
	handle_common(Type, Event, ?FUNCTION_NAME, StateData).

handle_common({call, From}, {checkout, _ReqOpts}, _,
		StateData=#state{conns_meta=ConnsMeta}) ->
	case find_available_connection(StateData) of
		none ->
			{keep_state_and_data, {reply, From, undefined}};
		ConnPid ->
			Meta = maps:get(ConnPid, ConnsMeta, #{}),
			{keep_state_and_data, {reply, From, {ConnPid, Meta}}}
	end;
handle_common(info, {gun_notify, ConnPid, settings_changed, Settings}, _, StateData=#state{conns=Conns}) ->
	%% Assert that the state is correct.
	{up, http2, _} = maps:get(ConnPid, Conns),
	{keep_state, StateData#state{conns=Conns#{ConnPid => {up, http2, Settings}}}};
handle_common(info, {gun_down, ConnPid, Protocol, _Reason, _KilledStreams}, _, StateData=#state{conns=Conns}) ->
	{up, Protocol, _} = maps:get(ConnPid, Conns),
	{next_state, degraded, StateData#state{conns=Conns#{ConnPid => down}}};
%% @todo We do not want to reconnect automatically when the pool is dynamic.
handle_common(info, {'DOWN', _MRef, process, ConnPid0, Reason}, _,
		StateData=#state{host=Host, port=Port, opts=Opts, table=Tid, conns=Conns0, conns_meta=ConnsMeta0}) ->
	Conns = maps:remove(ConnPid0, Conns0),
	ConnsMeta = maps:remove(ConnPid0, ConnsMeta0),
	case Reason of
		%% The process is down because of a configuration error.
		%% Do NOT attempt to reconnect, leave the pool in a degraded state.
		badarg ->
			{next_state, degraded, StateData#state{conns=Conns, conns_meta=ConnsMeta}};
		_ ->
			ConnOpts = conn_opts(Tid, Opts),
			{ok, ConnPid} = gun:open(Host, Port, ConnOpts),
			_ = monitor(process, ConnPid),
			{next_state, degraded, StateData#state{conns=Conns#{ConnPid => down}, conns_meta=ConnsMeta}}
	end;
handle_common({call, From}, info, StateName, #state{host=Host, port=Port,
		opts=Opts, table=Tid, conns=Conns, conns_meta=ConnsMeta}) ->
	{keep_state_and_data, {reply, From, {StateName, #{
		%% @todo Not sure whether all of this should be documented. Maybe not ConnsMeta for now?
		host => Host,
		port => Port,
		opts => Opts,
		table => Tid,
		conns => Conns,
		conns_meta => ConnsMeta
	}}}};
handle_common({call, From}, await_up, operational, _) ->
	{keep_state_and_data, {reply, From, ok}};
handle_common({call, From}, await_up, _, StateData=#state{await_up=AwaitUp}) ->
	{keep_state, StateData#state{await_up=[From|AwaitUp]}};
handle_common(Type, Event, StateName, StateData) ->
	logger:error("Unexpected event in state ~p of type ~p:~n~w~n~p~n",
		[StateName, Type, Event, StateData]),
	keep_state_and_data.

%% We go over every connection and return the first one
%% we find that has capacity. How we determine whether
%% capacity is available depends on the protocol. For
%% HTTP/2 we look into the protocol settings. The
%% current number of streams is maintained by the
%% event handler gun_pool_events_h.
find_available_connection(#state{table=Tid, conns=Conns}) ->
	I = lists:sort([{rand:uniform(), K} || K <- maps:keys(Conns)]),
	find_available_connection(I, Conns, Tid).

find_available_connection([], _, _) ->
	none;
find_available_connection([{_, ConnPid}|I], Conns, Tid) ->
	case maps:get(ConnPid, Conns) of
		{up, Protocol, Settings} ->
			MaxStreams = max_streams(Protocol, Settings),
			CurrentStreams = case ets:lookup(Tid, ConnPid) of
				[] ->
					0;
				[{_, CS}] ->
					CS
			end,
			if
				CurrentStreams + 1 > MaxStreams ->
					find_available_connection(I, Conns, Tid);
				true ->
					ConnPid
			end;
		_ ->
			find_available_connection(I, Conns, Tid)
	end.

max_streams(http, _) ->
	1;
max_streams(http2, #{max_concurrent_streams := MaxStreams}) ->
	MaxStreams;
max_streams(http2, #{}) ->
	infinity;
%% There are no streams or Gun is not aware of streams when
%% the protocol is Websocket or raw.
max_streams(ws, _) ->
	infinity;
max_streams(raw, _) ->
	infinity.

terminate(Reason, StateName, #state{host=Host, port=Port, opts=Opts, await_up=AwaitUp}) ->
	gen_statem:reply([
		{reply, ReplyTo, {error, {terminate, StateName, Reason}}}
	|| ReplyTo <- AwaitUp]),
	true = ets:delete(gun_pools, gun_pools_key(Host, Port, Opts)),
	ok.
