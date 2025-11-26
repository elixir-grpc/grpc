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

%% Transport callback for ssl.

-module(gun_tls_proxy_cb).

-export([connect/4]).
-export([controlling_process/2]).
-export([send/2]).
-export([setopts/2]).
-export([close/1]).

%% The connect/4 function is called by the process
%% that calls ssl:connect/2,3,4.
connect(_Address, _Port, Opts, _Timeout) ->
	{_, GunPid} = lists:keyfind(gun_tls_proxy, 1, Opts),
	{ok, GunPid}.

controlling_process(Socket, ControllingPid) ->
	gun_tls_proxy:cb_controlling_process(Socket, ControllingPid).

send(Socket, Data) ->
	gun_tls_proxy:cb_send(Socket, Data).

setopts(Socket, Opts) ->
	gun_tls_proxy:cb_setopts(Socket, Opts).

close(_) ->
	ok.
