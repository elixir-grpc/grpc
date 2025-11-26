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

%% This module contains functions and types common
%% to all or most HTTP versions.
-module(cow_http).

%% The HTTP/1 functions have been moved to cow_http1.
%% In order to remain backward compatible we redirect
%% calls to cow_http1. The type version() was moved
%% and no fallback is provided.
%%
%% @todo Remove the aliases in Cowlib 3.0.
-export([parse_request_line/1]).
-export([parse_status_line/1]).
-export([status_to_integer/1]).
-export([parse_headers/1]).
-export([parse_fullpath/1]).
-export([parse_version/1]).
-export([request/4]).
-export([response/3]).
-export([headers/1]).
-export([version/1]).

%% Functions used by HTTP/2+.

-export([format_semantic_error/1]).
-export([merge_pseudo_headers/2]).
-export([process_headers/5]).
-export([remove_http1_headers/1]).

%% Types used by all versions of HTTP.

-type status() :: 100..999.
-export_type([status/0]).

-type headers() :: [{binary(), iodata()}].
-export_type([headers/0]).

%% Types used by HTTP/2+.

-type pseudo_headers() :: #{} %% Trailers
	| #{ %% Responses.
		status := cow_http:status()
	} | #{ %% Normal CONNECT requests.
		method := binary(),
		authority := binary()
	} | #{ %% Extended CONNECT requests.
		method := binary(),
		scheme := binary(),
		authority := binary(),
		path := binary(),
		protocol := binary()
	} | #{ %% Other requests.
		method := binary(),
		scheme := binary(),
		authority => binary(),
		path := binary()
	}.
-export_type([pseudo_headers/0]).

-type fin() :: fin | nofin.
-export_type([fin/0]).

%% HTTP/1 function aliases.

-spec parse_request_line(binary()) -> {binary(), binary(), cow_http1:version(), binary()}.
parse_request_line(Data) -> cow_http1:parse_request_line(Data).

-spec parse_status_line(binary()) -> {cow_http1:version(), status(), binary(), binary()}.
parse_status_line(Data) -> cow_http1:parse_status_line(Data).

-spec status_to_integer(status() | binary()) -> status().
status_to_integer(Status) -> cow_http1:status_to_integer(Status).

-spec parse_headers(binary()) -> {[{binary(), binary()}], binary()}.
parse_headers(Data) -> cow_http1:parse_headers(Data).

-spec parse_fullpath(binary()) -> {binary(), binary()}.
parse_fullpath(Fullpath) -> cow_http1:parse_fullpath(Fullpath).

-spec parse_version(binary()) -> cow_http1:version().
parse_version(Data) -> cow_http1:parse_version(Data).

-spec request(binary(), iodata(), cow_http1:version(), headers()) -> iodata().
request(Method, Path, Version, Headers) -> cow_http1:request(Method, Path, Version, Headers).

-spec response(status() | binary(), cow_http1:version(), headers()) -> iodata().
response(Status, Version, Headers) -> cow_http1:response(Status, Version, Headers).

-spec headers(headers()) -> iodata().
headers(Headers) -> cow_http1:headers(Headers).

-spec version(cow_http1:version()) -> binary().
version(Version) -> cow_http1:version(Version).

%% Functions used by HTTP/2+.

%% Semantic errors are common to all HTTP versions.

-spec format_semantic_error(atom()) -> atom().

format_semantic_error(connect_invalid_content_length_2xx) ->
	'Content-length header received in a 2xx response to a CONNECT request. (RFC7230 3.3.2).';
format_semantic_error(invalid_content_length_header) ->
	'The content-length header is invalid. (RFC7230 3.3.2)';
format_semantic_error(invalid_content_length_header_1xx) ->
	'Content-length header received in a 1xx response. (RFC7230 3.3.2)';
format_semantic_error(invalid_content_length_header_204) ->
	'Content-length header received in a 204 response. (RFC7230 3.3.2)';
format_semantic_error(multiple_content_length_headers) ->
	'Multiple content-length headers were received. (RFC7230 3.3.2)'.

%% Merge pseudo headers at the start of headers.

-spec merge_pseudo_headers(pseudo_headers(), headers()) -> headers().

merge_pseudo_headers(PseudoHeaders, Headers0) ->
	lists:foldl(fun
		({status, Status}, Acc) when is_integer(Status) ->
			[{<<":status">>, integer_to_binary(Status)}|Acc];
		({Name, Value}, Acc) ->
			[{iolist_to_binary([$:, atom_to_binary(Name, latin1)]), Value}|Acc]
		end, Headers0, maps:to_list(PseudoHeaders)).

%% Process HTTP/2+ headers. This is done after decoding them.

-spec process_headers(headers(), request | push_promise | response | trailers,
		binary() | undefined, fin(), #{enable_connect_protocol => boolean(), any() => any()})
	-> {headers, headers(), pseudo_headers(), non_neg_integer() | undefined}
	| {push_promise, headers(), pseudo_headers()}
	| {trailers, headers()}
	| {error, atom()}.

process_headers(Headers0, Type, ReqMethod, IsFin, LocalSettings)
		when Type =:= request; Type =:= push_promise ->
	IsExtendedConnectEnabled = maps:get(enable_connect_protocol, LocalSettings, false),
	case request_pseudo_headers(Headers0, #{}) of
		%% Extended CONNECT method (HTTP/2: RFC8441, HTTP/3: RFC9220).
		{ok, PseudoHeaders=#{method := <<"CONNECT">>, scheme := _,
			authority := _, path := _, protocol := _}, Headers}
			when IsExtendedConnectEnabled ->
			regular_headers(Headers, Type, ReqMethod, IsFin, PseudoHeaders);
		{ok, #{method := <<"CONNECT">>, scheme := _,
			authority := _, path := _}, _}
			when IsExtendedConnectEnabled ->
			{error, extended_connect_missing_protocol};
		{ok, #{protocol := _}, _} ->
			{error, invalid_protocol_pseudo_header};
		%% Normal CONNECT (no scheme/path).
		{ok, PseudoHeaders = #{method := <<"CONNECT">>, authority := _}, Headers}
				when map_size(PseudoHeaders) =:= 2 ->
			regular_headers(Headers, Type, ReqMethod, IsFin, PseudoHeaders);
		{ok, #{method := <<"CONNECT">>, authority := _}, _} ->
			{error, connect_invalid_pseudo_header};
		{ok, #{method := <<"CONNECT">>}, _} ->
			{error, connect_missing_authority};
		%% Other requests.
		{ok, PseudoHeaders = #{method := _, scheme := _, path := _}, Headers} ->
			regular_headers(Headers, Type, ReqMethod, IsFin, PseudoHeaders);
		{ok, _, _} ->
			{error, missing_pseudo_header};
		Error = {error, _} ->
			Error
	end;
process_headers(Headers0, Type = response, ReqMethod, IsFin, _LocalSettings) ->
	case response_pseudo_headers(Headers0, #{}) of
		{ok, PseudoHeaders=#{status := _}, Headers} ->
			regular_headers(Headers, Type, ReqMethod, IsFin, PseudoHeaders);
		{ok, _, _} ->
			{error, missing_pseudo_header};
		Error = {error, _} ->
			Error
	end;
process_headers(Headers, Type = trailers, ReqMethod, IsFin, _LocalSettings) ->
	case trailers_have_pseudo_headers(Headers) of
		false ->
			regular_headers(Headers, Type, ReqMethod, IsFin, #{});
		true ->
			{error, trailer_invalid_pseudo_header}
	end.

request_pseudo_headers([{<<":method">>, _}|_], #{method := _}) ->
	{error, multiple_method_pseudo_headers};
request_pseudo_headers([{<<":method">>, Method}|Tail], PseudoHeaders) ->
	request_pseudo_headers(Tail, PseudoHeaders#{method => Method});
request_pseudo_headers([{<<":scheme">>, _}|_], #{scheme := _}) ->
	{error, multiple_scheme_pseudo_headers};
request_pseudo_headers([{<<":scheme">>, Scheme}|Tail], PseudoHeaders) ->
	request_pseudo_headers(Tail, PseudoHeaders#{scheme => Scheme});
request_pseudo_headers([{<<":authority">>, _}|_], #{authority := _}) ->
	{error, multiple_authority_pseudo_headers};
request_pseudo_headers([{<<":authority">>, Authority}|Tail], PseudoHeaders) ->
	request_pseudo_headers(Tail, PseudoHeaders#{authority => Authority});
request_pseudo_headers([{<<":path">>, _}|_], #{path := _}) ->
	{error, multiple_path_pseudo_headers};
request_pseudo_headers([{<<":path">>, Path}|Tail], PseudoHeaders) ->
	request_pseudo_headers(Tail, PseudoHeaders#{path => Path});
request_pseudo_headers([{<<":protocol">>, _}|_], #{protocol := _}) ->
	{error, multiple_protocol_pseudo_headers};
request_pseudo_headers([{<<":protocol">>, Protocol}|Tail], PseudoHeaders) ->
	request_pseudo_headers(Tail, PseudoHeaders#{protocol => Protocol});
request_pseudo_headers([{<<":", _/bits>>, _}|_], _) ->
	{error, invalid_pseudo_header};
request_pseudo_headers(Headers, PseudoHeaders) ->
	{ok, PseudoHeaders, Headers}.

response_pseudo_headers([{<<":status">>, _}|_], #{status := _}) ->
	{error, multiple_status_pseudo_headers};
response_pseudo_headers([{<<":status">>, Status}|Tail], PseudoHeaders) ->
	try cow_http:status_to_integer(Status) of
		IntStatus ->
			response_pseudo_headers(Tail, PseudoHeaders#{status => IntStatus})
	catch _:_ ->
		{error, invalid_status_pseudo_header}
	end;
response_pseudo_headers([{<<":", _/bits>>, _}|_], _) ->
	{error, invalid_pseudo_header};
response_pseudo_headers(Headers, PseudoHeaders) ->
	{ok, PseudoHeaders, Headers}.

trailers_have_pseudo_headers([]) ->
	false;
trailers_have_pseudo_headers([{<<":", _/bits>>, _}|_]) ->
	true;
trailers_have_pseudo_headers([_|Tail]) ->
	trailers_have_pseudo_headers(Tail).

%% Rejecting invalid regular headers might be a bit too strong for clients.
regular_headers(Headers, Type, ReqMethod, IsFin, PseudoHeaders) ->
	case regular_headers(Headers, Type) of
		ok when Type =:= request ->
			request_expected_size(Headers, IsFin, PseudoHeaders);
		ok when Type =:= push_promise ->
			return_push_promise(Headers, PseudoHeaders);
		ok when Type =:= response ->
			response_expected_size(Headers, ReqMethod, IsFin, PseudoHeaders);
		ok when Type =:= trailers ->
			return_trailers(Headers);
		Error = {error, _} ->
			Error
	end.

regular_headers([{<<>>, _}|_], _) ->
	{error, empty_header_name};
regular_headers([{<<":", _/bits>>, _}|_], _) ->
	{error, pseudo_header_after_regular};
regular_headers([{<<"connection">>, _}|_], _) ->
	{error, invalid_connection_header};
regular_headers([{<<"keep-alive">>, _}|_], _) ->
	{error, invalid_keep_alive_header};
regular_headers([{<<"proxy-authenticate">>, _}|_], _) ->
	{error, invalid_proxy_authenticate_header};
regular_headers([{<<"proxy-authorization">>, _}|_], _) ->
	{error, invalid_proxy_authorization_header};
regular_headers([{<<"transfer-encoding">>, _}|_], _) ->
	{error, invalid_transfer_encoding_header};
regular_headers([{<<"upgrade">>, _}|_], _) ->
	{error, invalid_upgrade_header};
regular_headers([{<<"te">>, Value}|_], request) when Value =/= <<"trailers">> ->
	{error, invalid_te_value};
regular_headers([{<<"te">>, _}|_], Type) when Type =/= request ->
	{error, invalid_te_header};
regular_headers([{Name, _}|Tail], Type) ->
	Pattern = [
		<<$A>>, <<$B>>, <<$C>>, <<$D>>, <<$E>>, <<$F>>, <<$G>>, <<$H>>, <<$I>>,
		<<$J>>, <<$K>>, <<$L>>, <<$M>>, <<$N>>, <<$O>>, <<$P>>, <<$Q>>, <<$R>>,
		<<$S>>, <<$T>>, <<$U>>, <<$V>>, <<$W>>, <<$X>>, <<$Y>>, <<$Z>>
	],
	case binary:match(Name, Pattern) of
		nomatch -> regular_headers(Tail, Type);
		_ -> {error, uppercase_header_name}
	end;
regular_headers([], _) ->
	ok.

request_expected_size(Headers, IsFin, PseudoHeaders) ->
	case [CL || {<<"content-length">>, CL} <- Headers] of
		[] when IsFin =:= fin ->
			return_headers(Headers, PseudoHeaders, 0);
		[] ->
			return_headers(Headers, PseudoHeaders, undefined);
		[<<"0">>] ->
			return_headers(Headers, PseudoHeaders, 0);
		[_] when IsFin =:= fin ->
			{error, non_zero_length_with_fin_flag};
		[BinLen] ->
			parse_expected_size(Headers, PseudoHeaders, BinLen);
		_ ->
			{error, multiple_content_length_headers}
	end.

response_expected_size(Headers, ReqMethod, IsFin, PseudoHeaders = #{status := Status}) ->
	case [CL || {<<"content-length">>, CL} <- Headers] of
		[] when IsFin =:= fin ->
			return_headers(Headers, PseudoHeaders, 0);
		[] ->
			return_headers(Headers, PseudoHeaders, undefined);
		[_] when Status >= 100, Status =< 199 ->
			{error, invalid_content_length_header_1xx};
		[_] when Status =:= 204 ->
			{error, invalid_content_length_header_204};
		[_] when Status >= 200, Status =< 299, ReqMethod =:= <<"CONNECT">> ->
			{error, connect_invalid_content_length_2xx};
		%% Responses to HEAD requests, and 304 responses may contain
		%% a content-length header that must be ignored. (RFC7230 3.3.2)
		[_] when ReqMethod =:= <<"HEAD">> ->
			return_headers(Headers, PseudoHeaders, 0);
		[_] when Status =:= 304 ->
			return_headers(Headers, PseudoHeaders, 0);
		[<<"0">>] when IsFin =:= fin ->
			return_headers(Headers, PseudoHeaders, 0);
		[_] when IsFin =:= fin ->
			{error, non_zero_length_with_fin_flag};
		[BinLen] ->
			parse_expected_size(Headers, PseudoHeaders, BinLen);
		_ ->
			{error, multiple_content_length_headers}
	end.

parse_expected_size(Headers, PseudoHeaders, BinLen) ->
	try cow_http_hd:parse_content_length(BinLen) of
		Len ->
			return_headers(Headers, PseudoHeaders, Len)
	catch _:_ ->
		{error, invalid_content_length_header}
	end.

return_headers(Headers, PseudoHeaders, Len) ->
	{headers, Headers, PseudoHeaders, Len}.

return_push_promise(Headers, PseudoHeaders) ->
	{push_promise, Headers, PseudoHeaders}.

return_trailers(Headers) ->
	{trailers, Headers}.

%% Remove HTTP/1-specific headers.

-spec remove_http1_headers(headers()) -> headers().

remove_http1_headers(Headers) ->
	RemoveHeaders0 = [
		<<"keep-alive">>,
		<<"proxy-connection">>,
		<<"transfer-encoding">>,
		<<"upgrade">>
	],
	RemoveHeaders = case lists:keyfind(<<"connection">>, 1, Headers) of
		false ->
			RemoveHeaders0;
		{_, ConnHd} ->
			%% We do not need to worry about any "close" header because
			%% that header name is reserved.
			Connection = cow_http_hd:parse_connection(ConnHd),
			Connection ++ [<<"connection">>|RemoveHeaders0]
	end,
	lists:filter(fun({Name, _}) ->
		not lists:member(Name, RemoveHeaders)
	end, Headers).
