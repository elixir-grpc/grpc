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

-module(gun_sse_h).
-behavior(gun_content_handler).

-export([init/5]).
-export([handle/3]).

-record(state, {
	reply_to :: gun:reply_to(),
	stream_ref :: reference(),
	sse_state :: cow_sse:state()
}).

%% @todo In the future we want to allow different media types.

-spec init(pid(), reference(), _, cow_http:headers(), _)
	-> {ok, #state{}} | disable.
init(ReplyTo, StreamRef, _, Headers, _) ->
	case lists:keyfind(<<"content-type">>, 1, Headers) of
		{_, ContentType} ->
			case cow_http_hd:parse_content_type(ContentType) of
				{<<"text">>, <<"event-stream">>, _Ignored} ->
					{ok, #state{reply_to=ReplyTo, stream_ref=StreamRef,
						sse_state=cow_sse:init()}};
				_ ->
					disable
			end;
		_ ->
			disable
	end.

-spec handle(_, binary(), State) -> {done, non_neg_integer(), State} when State::#state{}.
handle(IsFin, Data, State) ->
	handle(IsFin, Data, State, 0).

handle(IsFin, Data, State=#state{reply_to=ReplyTo, stream_ref=StreamRef, sse_state=SSE0}, Flow) ->
	case cow_sse:parse(Data, SSE0) of
		{event, Event, SSE} ->
			gun:reply(ReplyTo, {gun_sse, self(), StreamRef, Event}),
			handle(IsFin, <<>>, State#state{sse_state=SSE}, Flow + 1);
		{more, SSE} ->
			Inc = case IsFin of
				fin ->
					gun:reply(ReplyTo, {gun_sse, self(), StreamRef, fin}),
					1;
				_ ->
					0
			end,
			{done, Flow + Inc, State#state{sse_state=SSE}}
	end.
