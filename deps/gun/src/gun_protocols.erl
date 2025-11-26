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

-module(gun_protocols).

-export([add_stream_ref/2]).
-export([handler/1]).
-export([handler_and_opts/2]).
-export([negotiated/2]).
-export([stream_ref/1]).

-spec add_stream_ref(Protocol, undefined | gun:stream_ref())
	-> Protocol when Protocol :: gun:protocol().
add_stream_ref(Protocol, undefined) ->
	Protocol;
add_stream_ref({ProtocolName, ProtocolOpts}, StreamRef) ->
	{ProtocolName, ProtocolOpts#{stream_ref => StreamRef}};
add_stream_ref(ProtocolName, StreamRef) ->
	{ProtocolName, #{stream_ref => StreamRef}}.

-spec handler(gun:protocol()) -> module().
handler(http) -> gun_http;
handler({http, _}) -> gun_http;
handler(http2) -> gun_http2;
handler({http2, _}) -> gun_http2;
handler(http3) -> gun_http3;
handler({http3, _}) -> gun_http3;
handler(raw) -> gun_raw;
handler({raw, _}) -> gun_raw;
handler(socks) -> gun_socks;
handler({socks, _}) -> gun_socks;
handler(ws) -> gun_ws;
handler({ws, _}) -> gun_ws.

-spec handler_and_opts(gun:protocol(), map()) -> {module(), map()}.
handler_and_opts({ProtocolName, ProtocolOpts}, _) ->
	{handler(ProtocolName), ProtocolOpts};
handler_and_opts(ProtocolName, Opts) ->
	Protocol = handler(ProtocolName),
	{Protocol, maps:get(Protocol:opts_name(), Opts, #{})}.

-spec negotiated({ok, binary()} | {error, protocol_not_negotiated}, gun:protocols())
	-> gun:protocol().
negotiated({ok, <<"h2">>}, Protocols) ->
	lists:foldl(fun
		(E = http2, _) -> E;
		(E = {http2, _}, _) -> E;
		(_, Acc) -> Acc
	end, http2, Protocols);
negotiated({ok, <<"http/1.1">>}, Protocols) ->
	lists:foldl(fun
		(E = http, _) -> E;
		(E = {http, _}, _) -> E;
		(_, Acc) -> Acc
	end, http, Protocols);
negotiated({error, protocol_not_negotiated}, [Protocol]) ->
	Protocol;
negotiated({error, protocol_not_negotiated}, _) ->
	http.

-spec stream_ref(gun:protocol()) -> undefined | gun:stream_ref().
stream_ref({_, ProtocolOpts}) -> maps:get(stream_ref, ProtocolOpts, undefined);
stream_ref(_) -> undefined.
