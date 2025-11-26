%% Copyright (c) Loïc Hoguin <essen@ninenines.eu>
%% Copyright (c) Maria Scott <maria-12648430@hnc-agency.org>
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

%% Make sure to never reload this module outside a release upgrade,
%% as calling l(ranch_conns_sup) twice will kill the process and all
%% the currently open connections.
-module(ranch_conns_sup).

%% API.
-export([start_link/6]).
-export([start_protocol/3]).
-export([active_connections/1]).

%% Supervisor internals.
-export([init/7]).
-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-type conn_type() :: worker | supervisor.
-type shutdown() :: brutal_kill | timeout().

-record(state, {
	parent = undefined :: pid(),
	ref :: ranch:ref(),
	id :: pos_integer(),
	conn_type :: conn_type(),
	shutdown :: shutdown(),
	transport = undefined :: module(),
	protocol = undefined :: module(),
	opts :: any(),
	handshake_timeout :: timeout(),
	max_conns = undefined :: ranch:max_conns(),
	stats_counters_ref :: counters:counters_ref(),
	alarms = #{} :: #{term() => {map(), undefined | reference()}},
	logger = undefined :: module()
}).

%% API.

-spec start_link(ranch:ref(), pos_integer(), module(), any(), module(), module()) -> {ok, pid()}.
start_link(Ref, Id, Transport, TransOpts, Protocol, Logger) ->
	proc_lib:start_link(?MODULE, init,
		[self(), Ref, Id, Transport, TransOpts, Protocol, Logger]).

%% We can safely assume we are on the same node as the supervisor.
%%
%% We can also safely avoid having a monitor and a timeout here
%% because only three things can happen:
%%  *  The supervisor died; rest_for_one strategy killed all acceptors
%%     so this very calling process is going to di--
%%  *  There's too many connections, the supervisor will resume the
%%     acceptor only when we get below the limit again.
%%  *  The supervisor is overloaded, there's either too many acceptors
%%     or the max_connections limit is too large. It's better if we
%%     don't keep accepting connections because this leaves
%%     more room for the situation to be resolved.
%%
%% We do not need the reply, we only need the ok from the supervisor
%% to continue. The supervisor sends its own pid when the acceptor can
%% continue.
-spec start_protocol(pid(), reference(), inet:socket()) -> ok.
start_protocol(SupPid, MonitorRef, Socket) ->
	SupPid ! {?MODULE, start_protocol, self(), Socket},
	receive
		SupPid ->
			ok;
		{'DOWN', MonitorRef, process, SupPid, Reason} ->
			error(Reason)
	end.

%% We can't make the above assumptions here. This function might be
%% called from anywhere.
-spec active_connections(pid()) -> non_neg_integer().
active_connections(SupPid) ->
	Tag = erlang:monitor(process, SupPid),
	catch erlang:send(SupPid, {?MODULE, active_connections, self(), Tag},
		[noconnect]),
	receive
		{Tag, Ret} ->
			erlang:demonitor(Tag, [flush]),
			Ret;
		{'DOWN', Tag, _, _, noconnection} ->
			exit({nodedown, node(SupPid)});
		{'DOWN', Tag, _, _, Reason} ->
			exit(Reason)
	after 5000 ->
		erlang:demonitor(Tag, [flush]),
		exit(timeout)
	end.

%% Supervisor internals.

-spec init(pid(), ranch:ref(), pos_integer(), module(), any(), module(), module()) -> no_return().
init(Parent, Ref, Id, Transport, TransOpts, Protocol, Logger) ->
	process_flag(trap_exit, true),
	ok = ranch_server:set_connections_sup(Ref, Id, self()),
	MaxConns = ranch_server:get_max_connections(Ref),
	Alarms = get_alarms(TransOpts),
	ConnType = maps:get(connection_type, TransOpts, worker),
	Shutdown = maps:get(shutdown, TransOpts, 5000),
	HandshakeTimeout = maps:get(handshake_timeout, TransOpts, 5000),
	ProtoOpts = ranch_server:get_protocol_options(Ref),
	StatsCounters = ranch_server:get_stats_counters(Ref),
	ok = proc_lib:init_ack(Parent, {ok, self()}),
	loop(#state{parent=Parent, ref=Ref, id=Id, conn_type=ConnType,
		shutdown=Shutdown, transport=Transport, protocol=Protocol,
		opts=ProtoOpts, stats_counters_ref=StatsCounters,
		handshake_timeout=HandshakeTimeout,
		max_conns=MaxConns, alarms=Alarms,
		logger=Logger}, 0, 0, []).

loop(State=#state{parent=Parent, ref=Ref, id=Id, conn_type=ConnType,
		transport=Transport, protocol=Protocol, opts=Opts, stats_counters_ref=StatsCounters,
		alarms=Alarms, max_conns=MaxConns, logger=Logger}, CurConns, NbChildren, Sleepers) ->
	receive
		{?MODULE, start_protocol, To, Socket} ->
			try Protocol:start_link(Ref, Transport, Opts) of
				{ok, Pid} ->
					inc_accept(StatsCounters, Id, 1),
					handshake(State, CurConns, NbChildren, Sleepers, To, Socket, Pid, Pid);
				{ok, SupPid, ProtocolPid} when ConnType =:= supervisor ->
					inc_accept(StatsCounters, Id, 1),
					handshake(State, CurConns, NbChildren, Sleepers, To, Socket, SupPid, ProtocolPid);
				Ret ->
					To ! self(),
					ranch:log(error,
						"Ranch listener ~p connection process start failure; "
						"~p:start_link/3 returned: ~0p~n",
						[Ref, Protocol, Ret], Logger),
					Transport:close(Socket),
					loop(State, CurConns, NbChildren, Sleepers)
			catch Class:Reason ->
				To ! self(),
				ranch:log(error,
					"Ranch listener ~p connection process start failure; "
					"~p:start_link/3 crashed with reason: ~p:~0p~n",
					[Ref, Protocol, Class, Reason], Logger),
				Transport:close(Socket),
				loop(State, CurConns, NbChildren, Sleepers)
			end;
		{?MODULE, active_connections, To, Tag} ->
			To ! {Tag, CurConns},
			loop(State, CurConns, NbChildren, Sleepers);
		%% Remove a connection from the count of connections.
		{remove_connection, Ref, Pid} ->
			case put(Pid, removed) of
				active when Sleepers =:= [] ->
					loop(State, CurConns - 1, NbChildren, Sleepers);
				active ->
					[To|Sleepers2] = Sleepers,
					To ! self(),
					loop(State, CurConns - 1, NbChildren, Sleepers2);
				removed ->
					loop(State, CurConns, NbChildren, Sleepers);
				undefined ->
					_ = erase(Pid),
					loop(State, CurConns, NbChildren, Sleepers)
			end;
		%% Upgrade the max number of connections allowed concurrently.
		%% We resume all sleeping acceptors if this number increases.
		{set_max_conns, MaxConns2} when MaxConns2 > MaxConns ->
			_ = [To ! self() || To <- Sleepers],
			loop(State#state{max_conns=MaxConns2},
				CurConns, NbChildren, []);
		{set_max_conns, MaxConns2} ->
			loop(State#state{max_conns=MaxConns2},
				CurConns, NbChildren, Sleepers);
		%% Upgrade the transport options.
		{set_transport_options, TransOpts} ->
			set_transport_options(State, CurConns, NbChildren, Sleepers, TransOpts);
		%% Upgrade the protocol options.
		{set_protocol_options, Opts2} ->
			loop(State#state{opts=Opts2},
				CurConns, NbChildren, Sleepers);
		{timeout, _, {activate_alarm, AlarmName}} when is_map_key(AlarmName, Alarms) ->
			{AlarmOpts, _} = maps:get(AlarmName, Alarms),
			NewAlarm = trigger_alarm(Ref, AlarmName, {AlarmOpts, undefined}, CurConns),
			loop(State#state{alarms=Alarms#{AlarmName => NewAlarm}}, CurConns, NbChildren, Sleepers);
		{timeout, _, {activate_alarm, _}} ->
			loop(State, CurConns, NbChildren, Sleepers);
		{'EXIT', Parent, Reason} ->
			terminate(State, Reason, NbChildren);
		{'EXIT', Pid, Reason} when Sleepers =:= [] ->
			case erase(Pid) of
				active ->
					inc_terminate(StatsCounters, Id, 1),
					report_error(Logger, Ref, Protocol, Pid, Reason),
					loop(State, CurConns - 1, NbChildren - 1, Sleepers);
				removed ->
					inc_terminate(StatsCounters, Id, 1),
					report_error(Logger, Ref, Protocol, Pid, Reason),
					loop(State, CurConns, NbChildren - 1, Sleepers);
				undefined ->
					loop(State, CurConns, NbChildren, Sleepers)
			end;
		%% Resume a sleeping acceptor if needed.
		{'EXIT', Pid, Reason} ->
			case erase(Pid) of
				active when CurConns > MaxConns ->
					inc_terminate(StatsCounters, Id, 1),
					report_error(Logger, Ref, Protocol, Pid, Reason),
					loop(State, CurConns - 1, NbChildren - 1, Sleepers);
				active ->
					inc_terminate(StatsCounters, Id, 1),
					report_error(Logger, Ref, Protocol, Pid, Reason),
					[To|Sleepers2] = Sleepers,
					To ! self(),
					loop(State, CurConns - 1, NbChildren - 1, Sleepers2);
				removed ->
					inc_terminate(StatsCounters, Id, 1),
					report_error(Logger, Ref, Protocol, Pid, Reason),
					loop(State, CurConns, NbChildren - 1, Sleepers);
				undefined ->
					loop(State, CurConns, NbChildren, Sleepers)
			end;
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
				{State, CurConns, NbChildren, Sleepers});
		%% Calls from the supervisor module.
		{'$gen_call', {To, Tag}, which_children} ->
			Children = [{Protocol, Pid, ConnType, [Protocol]}
				|| {Pid, Type} <- get(),
				Type =:= active orelse Type =:= removed],
			To ! {Tag, Children},
			loop(State, CurConns, NbChildren, Sleepers);
		{'$gen_call', {To, Tag}, count_children} ->
			Counts = case ConnType of
				worker -> [{supervisors, 0}, {workers, NbChildren}];
				supervisor -> [{supervisors, NbChildren}, {workers, 0}]
			end,
			Counts2 = [{specs, 1}, {active, NbChildren}|Counts],
			To ! {Tag, Counts2},
			loop(State, CurConns, NbChildren, Sleepers);
		{'$gen_call', {To, Tag}, _} ->
			To ! {Tag, {error, ?MODULE}},
			loop(State, CurConns, NbChildren, Sleepers);
		Msg ->
			ranch:log(error,
				"Ranch listener ~p received unexpected message ~p~n",
				[Ref, Msg], Logger),
			loop(State, CurConns, NbChildren, Sleepers)
	end.

handshake(State=#state{ref=Ref, transport=Transport, handshake_timeout=HandshakeTimeout,
		max_conns=MaxConns, alarms=Alarms0}, CurConns, NbChildren, Sleepers, To, Socket, SupPid, ProtocolPid) ->
	case Transport:controlling_process(Socket, ProtocolPid) of
		ok ->
			ProtocolPid ! {handshake, Ref, Transport, Socket, HandshakeTimeout},
			put(SupPid, active),
			CurConns2 = CurConns + 1,
			Sleepers2 = if CurConns2 < MaxConns ->
					To ! self(),
					Sleepers;
				true ->
					[To|Sleepers]
			end,
			Alarms1 = trigger_alarms(Ref, Alarms0, CurConns2),
			loop(State#state{alarms=Alarms1}, CurConns2, NbChildren + 1, Sleepers2);
		{error, _} ->
			Transport:close(Socket),
			%% Only kill the supervised pid, because the connection's pid,
			%% when different, is supposed to be sitting under it and linked.
			exit(SupPid, kill),
			To ! self(),
			loop(State, CurConns, NbChildren, Sleepers)
	end.

trigger_alarms(Ref, Alarms, CurConns) ->
	maps:map(
		fun
			(AlarmName, Alarm) ->
				trigger_alarm(Ref, AlarmName, Alarm, CurConns)
		end,
		Alarms
	).

trigger_alarm(Ref, AlarmName, {Opts=#{threshold := Threshold, callback := Callback}, undefined}, CurConns) when CurConns >= Threshold ->
	ActiveConns = [Pid || {Pid, active} <- get()],
	case Callback of
		{Mod, Fun} ->
			spawn(Mod, Fun, [Ref, AlarmName, self(), ActiveConns]);
		_ ->
			Self = self(),
			spawn(fun () -> Callback(Ref, AlarmName, Self, ActiveConns) end)
	end,
	{Opts, schedule_activate_alarm(AlarmName, Opts)};
trigger_alarm(_, _, Alarm, _) ->
	Alarm.

schedule_activate_alarm(AlarmName, #{cooldown := Cooldown}) when Cooldown > 0 ->
	erlang:start_timer(Cooldown, self(), {activate_alarm, AlarmName});
schedule_activate_alarm(_, _) ->
	undefined.

get_alarms(#{alarms := Alarms}) when is_map(Alarms) ->
	Alarms1 = ranch:compat_normalize_alarms_option(Alarms),
	maps:fold(
		fun
			(Name, Opts = #{type := num_connections, cooldown := _}, Acc) ->
				Acc#{Name => {Opts, undefined}};
			(Name, Opts = #{type := num_connections}, Acc) ->
				Acc#{Name => {Opts#{cooldown => 5000}, undefined}};
			(_, _, Acc) -> Acc
		end,
		#{},
		Alarms1
	);
get_alarms(_) ->
	#{}.

set_transport_options(State=#state{max_conns=MaxConns0}, CurConns, NbChildren, Sleepers0, TransOpts) ->
	MaxConns1 = maps:get(max_connections, TransOpts, 1024),
	HandshakeTimeout = maps:get(handshake_timeout, TransOpts, 5000),
	Shutdown = maps:get(shutdown, TransOpts, 5000),
	Sleepers1 = case MaxConns1 > MaxConns0 of
		true ->
			_ = [To ! self() || To <- Sleepers0],
			[];
		false ->
			Sleepers0
	end,
	State1=set_alarm_option(State, TransOpts, CurConns),
	loop(State1#state{max_conns=MaxConns1, handshake_timeout=HandshakeTimeout, shutdown=Shutdown},
		CurConns, NbChildren, Sleepers1).

set_alarm_option(State=#state{ref=Ref, alarms=OldAlarms}, TransOpts, CurConns) ->
	NewAlarms0 = get_alarms(TransOpts),
	NewAlarms1 = merge_alarms(OldAlarms, NewAlarms0),
	NewAlarms2 = trigger_alarms(Ref, NewAlarms1, CurConns),
	State#state{alarms=NewAlarms2}.

merge_alarms(Old, New) ->
	OldList = lists:sort(maps:to_list(Old)),
	NewList = lists:sort(maps:to_list(New)),
	Merged = merge_alarms(OldList, NewList, []),
	maps:from_list(Merged).

merge_alarms([], News, Acc) ->
	News ++ Acc;
merge_alarms([{_, {_, undefined}}|Olds], [], Acc) ->
	merge_alarms(Olds, [], Acc);
merge_alarms([{_, {_, Timer}}|Olds], [], Acc) ->
	_ = cancel_alarm_reactivation_timer(Timer),
	merge_alarms(Olds, [], Acc);
merge_alarms([{Name, {OldOpts, Timer}}|Olds], [{Name, {NewOpts, _}}|News], Acc) ->
	merge_alarms(Olds, News, [{Name, {NewOpts, adapt_alarm_timer(Name, Timer, OldOpts, NewOpts)}}|Acc]);
merge_alarms([{OldName, {_, Timer}}|Olds], News=[{NewName, _}|_], Acc) when OldName < NewName ->
	_ = cancel_alarm_reactivation_timer(Timer),
	merge_alarms(Olds, News, Acc);
merge_alarms(Olds, [New|News], Acc) ->
	merge_alarms(Olds, News, [New|Acc]).

%% Not in cooldown.
adapt_alarm_timer(_, undefined, _, _) ->
	undefined;
%% Cooldown unchanged.
adapt_alarm_timer(_, Timer, #{cooldown := Cooldown}, #{cooldown := Cooldown}) ->
	Timer;
%% Cooldown changed to no cooldown, cancel cooldown timer.
adapt_alarm_timer(_, Timer, _, #{cooldown := 0}) ->
	_ = cancel_alarm_reactivation_timer(Timer),
	undefined;
%% Cooldown changed, cancel current and start new timer taking the already elapsed time into account.
adapt_alarm_timer(Name, Timer, #{cooldown := OldCooldown}, #{cooldown := NewCooldown}) ->
	OldTimeLeft = cancel_alarm_reactivation_timer(Timer),
	case NewCooldown-OldCooldown+OldTimeLeft of
		NewTimeLeft when NewTimeLeft>0 ->
			erlang:start_timer(NewTimeLeft, self(), {activate_alarm, Name});
		_ ->
			undefined
	end.

cancel_alarm_reactivation_timer(Timer) ->
	case erlang:cancel_timer(Timer) of
		%% Timer had already expired when we tried to cancel it, so we flush the
		%% reactivation message it sent and return 0 as remaining time.
		false ->
			ok = receive {timeout, Timer, {activate_alarm, _}} -> ok after 0 -> ok end,
			0;
		%% Timer has not yet expired, we return the amount of time that was remaining.
		TimeLeft ->
			TimeLeft
	end.

-spec terminate(#state{}, any(), non_neg_integer()) -> no_return().
terminate(#state{shutdown=brutal_kill, id=Id,
		stats_counters_ref=StatsCounters}, Reason, NbChildren) ->
	kill_children(get_keys(active)),
	kill_children(get_keys(removed)),
	inc_terminate(StatsCounters, Id, NbChildren),
	exit(Reason);
%% Attempt to gracefully shutdown all children.
terminate(#state{shutdown=Shutdown, id=Id,
		stats_counters_ref=StatsCounters}, Reason, NbChildren) ->
	shutdown_children(get_keys(active)),
	shutdown_children(get_keys(removed)),
	_ = if
		Shutdown =:= infinity ->
			ok;
		true ->
			erlang:send_after(Shutdown, self(), kill)
	end,
	wait_children(NbChildren),
	inc_terminate(StatsCounters, Id, NbChildren),
	exit(Reason).

inc_accept(StatsCounters, Id, N) ->
	%% Accepts are counted in the odd indexes.
	counters:add(StatsCounters, 2*Id-1, N).

inc_terminate(StatsCounters, Id, N) ->
	%% Terminates are counted in the even indexes.
	counters:add(StatsCounters, 2*Id, N).

%% Kill all children and then exit. We unlink first to avoid
%% getting a message for each child getting killed.
kill_children(Pids) ->
	_ = [begin
		unlink(P),
		exit(P, kill)
	end || P <- Pids],
	ok.

%% Monitor processes so we can know which ones have shutdown
%% before the timeout. Unlink so we avoid receiving an extra
%% message. Then send a shutdown exit signal.
shutdown_children(Pids) ->
	_ = [begin
		monitor(process, P),
		unlink(P),
		exit(P, shutdown)
	end || P <- Pids],
	ok.

wait_children(0) ->
	ok;
wait_children(NbChildren) ->
	receive
		{'DOWN', _, process, Pid, _} ->
			case erase(Pid) of
				active -> wait_children(NbChildren - 1);
				removed -> wait_children(NbChildren - 1);
				_ -> wait_children(NbChildren)
			end;
		kill ->
			Active = get_keys(active),
			_ = [exit(P, kill) || P <- Active],
			Removed = get_keys(removed),
			_ = [exit(P, kill) || P <- Removed],
			ok
	end.

-spec system_continue(_, _, any()) -> no_return().
system_continue(_, _, {State, CurConns, NbChildren, Sleepers}) ->
	loop(State, CurConns, NbChildren, Sleepers).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, {State, _, NbChildren, _}) ->
	terminate(State, Reason, NbChildren).

-spec system_code_change(any(), _, _, _) -> {ok, any()}.
system_code_change(Misc, _, _, _) ->
	{ok, Misc}.

%% We use ~0p here instead of ~w because the latter doesn't
%% support printable strings.
report_error(_, _, _, _, normal) ->
	ok;
report_error(_, _, _, _, shutdown) ->
	ok;
report_error(_, _, _, _, {shutdown, _}) ->
	ok;
report_error(Logger, Ref, Protocol, Pid, Reason) ->
	ranch:log(error,
		"Ranch listener ~p had connection process started with "
		"~p:start_link/3 at ~p exit with reason: ~0p~n",
		[Ref, Protocol, Pid, Reason], Logger).
