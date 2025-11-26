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

-module(gun_pool_events_h).

-export([init/2]).
-export([domain_lookup_start/2]).
-export([domain_lookup_end/2]).
-export([connect_start/2]).
-export([connect_end/2]).
-export([tls_handshake_start/2]).
-export([tls_handshake_end/2]).
-export([request_start/2]).
-export([request_headers/2]).
-export([request_end/2]).
-export([push_promise_start/2]).
-export([push_promise_end/2]).
-export([response_start/2]).
-export([response_inform/2]).
-export([response_headers/2]).
-export([response_trailers/2]).
-export([response_end/2]).
-export([ws_upgrade/2]).
-export([ws_recv_frame_start/2]).
-export([ws_recv_frame_header/2]).
-export([ws_recv_frame_end/2]).
-export([ws_send_frame_start/2]).
-export([ws_send_frame_end/2]).
-export([protocol_changed/2]).
-export([origin_changed/2]).
-export([cancel/2]).
-export([disconnect/2]).
-export([terminate/2]).

init(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

domain_lookup_start(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

domain_lookup_end(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

connect_start(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

connect_end(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

tls_handshake_start(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

tls_handshake_end(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

request_start(Event=#{stream_ref := StreamRef}, State=#{table := Tid}) ->
	_ = ets:update_counter(Tid, self(), +1, {self(), 0}),
	propagate(Event, State#{
		StreamRef => {nofin, nofin}
	}, ?FUNCTION_NAME).

request_headers(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

request_end(Event=#{stream_ref := StreamRef}, State0=#{table := Tid}) ->
	State = case State0 of
		#{StreamRef := {nofin, fin}} ->
			_ = ets:update_counter(Tid, self(), -1),
			maps:remove(StreamRef, State0);
		#{StreamRef := {nofin, IsFin}} ->
			State0#{StreamRef => {fin, IsFin}}
	end,
	propagate(Event, State, ?FUNCTION_NAME).

push_promise_start(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

push_promise_end(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

response_start(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

response_inform(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

response_headers(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

response_trailers(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

response_end(Event=#{stream_ref := StreamRef}, State0=#{table := Tid}) ->
	State = case State0 of
		#{StreamRef := {fin, nofin}} ->
			_ = ets:update_counter(Tid, self(), -1),
			maps:remove(StreamRef, State0);
		#{StreamRef := {IsFin, nofin}} ->
			State0#{StreamRef => {IsFin, fin}}
	end,
	propagate(Event, State, ?FUNCTION_NAME).

ws_upgrade(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

ws_recv_frame_start(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

ws_recv_frame_header(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

ws_recv_frame_end(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

ws_send_frame_start(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

ws_send_frame_end(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

protocol_changed(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

origin_changed(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

cancel(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

disconnect(Event, State=#{table := Tid}) ->
	%% The ets:delete/2 call might fail when the pool has shut down.
	try
		true = ets:delete(Tid, self())
	catch _:_ ->
		ok
	end,
	propagate(Event, maps:with([event_handler, table], State), ?FUNCTION_NAME).

terminate(Event, State) ->
	propagate(Event, State, ?FUNCTION_NAME).

propagate(Event, State=#{event_handler := {Mod, ModState0}}, Fun) ->
	ModState = Mod:Fun(Event, ModState0),
	State#{event_handler => {Mod, ModState}};
propagate(_, State, _) ->
	State.
