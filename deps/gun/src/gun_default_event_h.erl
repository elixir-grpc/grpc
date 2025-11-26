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

-module(gun_default_event_h).
-behavior(gun_event).

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

init(_EventData, State) ->
	State.

domain_lookup_start(_EventData, State) ->
	State.

domain_lookup_end(_EventData, State) ->
	State.

connect_start(_EventData, State) ->
	State.

connect_end(_EventData, State) ->
	State.

tls_handshake_start(_EventData, State) ->
	State.

tls_handshake_end(_EventData, State) ->
	State.

request_start(_EventData, State) ->
	State.

request_headers(_EventData, State) ->
	State.

request_end(_EventData, State) ->
	State.

push_promise_start(_EventData, State) ->
	State.

push_promise_end(_EventData, State) ->
	State.

response_start(_EventData, State) ->
	State.

response_inform(_EventData, State) ->
	State.

response_headers(_EventData, State) ->
	State.

response_trailers(_EventData, State) ->
	State.

response_end(_EventData, State) ->
	State.

ws_upgrade(_EventData, State) ->
	State.

ws_recv_frame_start(_EventData, State) ->
	State.

ws_recv_frame_header(_EventData, State) ->
	State.

ws_recv_frame_end(_EventData, State) ->
	State.

ws_send_frame_start(_EventData, State) ->
	State.

ws_send_frame_end(_EventData, State) ->
	State.

protocol_changed(_EventData, State) ->
	State.

origin_changed(_EventData, State) ->
	State.

cancel(_EventData, State) ->
	State.

disconnect(_EventData, State) ->
	State.

terminate(_EventData, State) ->
	State.
