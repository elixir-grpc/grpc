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

%% The prefixed integer and the string formats are common
%% to both HPACK and QPACK. They are included directly in
%% each module in order to avoid fully-qualified calls and
%% slightly improve performance.
%%
%% Some functions are only used in one or the other even
%% though the format is the same. In that case the functions
%% can be found in the relevant module.
%%
%% Any tests relevant to these functions should be added to
%% cow_hpack since HPACK is where these originate from.

%% Prefix decoding.
%%
%% The HPACK format has 4 different integer prefixes length (from 4 to 7)
%% and each can be used to create an indefinite length integer if all bits
%% of the prefix are set to 1.

dec_int5(<<2#11111:5, Rest/bits>>) ->
	dec_big_int(Rest, 31, 0);
dec_int5(<<Int:5, Rest/bits>>) ->
	{Int, Rest}.

dec_big_int(<<0:1, Value:7, Rest/bits>>, Int, M) ->
	{Int + (Value bsl M), Rest};
dec_big_int(<<1:1, Value:7, Rest/bits>>, Int, M) ->
	dec_big_int(Rest, Int + (Value bsl M), M + 7).

%% Prefix encoding.

enc_int5(Int, Prefix) when Int < 31 ->
	<<Prefix:3, Int:5>>;
enc_int5(Int, Prefix) ->
	enc_big_int(Int - 31, <<Prefix:3, 2#11111:5>>).

enc_int6(Int, Prefix) when Int < 63 ->
	<<Prefix:2, Int:6>>;
enc_int6(Int, Prefix) ->
	enc_big_int(Int - 63, <<Prefix:2, 2#111111:6>>).

enc_int7(Int, Prefix) when Int < 127 ->
	<<Prefix:1, Int:7>>;
enc_int7(Int, Prefix) ->
	enc_big_int(Int - 127, <<Prefix:1, 2#1111111:7>>).

enc_big_int(Int, Acc) when Int < 128 ->
	<<Acc/binary, Int:8>>;
enc_big_int(Int, Acc) ->
	enc_big_int(Int bsr 7, <<Acc/binary, 1:1, Int:7>>).

%% String decoding.
%%
%% We use a lookup table that allows us to benefit from
%% the binary match context optimization. A more naive
%% implementation using bit pattern matching cannot reuse
%% a match context because it wouldn't always match on
%% byte boundaries.
%%
%% See cow_hpack_dec_huffman_lookup.hrl for more details.

dec_huffman(<<A:4, B:4, R/bits>>, Len, Huff0, Acc) when Len > 1 ->
	{_, CharA, Huff1} = dec_huffman_lookup(Huff0, A),
	{_, CharB, Huff} = dec_huffman_lookup(Huff1, B),
	case {CharA, CharB} of
		{undefined, undefined} -> dec_huffman(R, Len - 1, Huff, Acc);
		{CharA, undefined} -> dec_huffman(R, Len - 1, Huff, <<Acc/binary, CharA>>);
		{undefined, CharB} -> dec_huffman(R, Len - 1, Huff, <<Acc/binary, CharB>>);
		{CharA, CharB} -> dec_huffman(R, Len - 1, Huff, <<Acc/binary, CharA, CharB>>)
	end;
dec_huffman(<<A:4, B:4, Rest/bits>>, 1, Huff0, Acc) ->
	{_, CharA, Huff} = dec_huffman_lookup(Huff0, A),
	{ok, CharB, _} = dec_huffman_lookup(Huff, B),
	case {CharA, CharB} of
		%% {undefined, undefined} (> 7-bit final padding) is rejected with a crash.
		{CharA, undefined} ->
			{<<Acc/binary, CharA>>, Rest};
		{undefined, CharB} ->
			{<<Acc/binary, CharB>>, Rest};
		_ ->
			{<<Acc/binary, CharA, CharB>>, Rest}
	end;
%% Can only be reached when the string length to decode is 0.
dec_huffman(Rest, 0, _, <<>>) ->
	{<<>>, Rest}.

-include("cow_hpack_dec_huffman_lookup.hrl").

%% String encoding.

enc_str(Str, huffman) ->
	Str2 = enc_huffman(Str, <<>>),
	[enc_int7(byte_size(Str2), 2#1)|Str2];
enc_str(Str, no_huffman) ->
	[enc_int7(byte_size(Str), 2#0)|Str].

enc_huffman(<<>>, Acc) ->
	case bit_size(Acc) rem 8 of
		1 -> <<Acc/bits, 2#1111111:7>>;
		2 -> <<Acc/bits, 2#111111:6>>;
		3 -> <<Acc/bits, 2#11111:5>>;
		4 -> <<Acc/bits, 2#1111:4>>;
		5 -> <<Acc/bits, 2#111:3>>;
		6 -> <<Acc/bits, 2#11:2>>;
		7 -> <<Acc/bits, 2#1:1>>;
		0 -> Acc
	end;
enc_huffman(<<0, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111000:13>>);
enc_huffman(<<1, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111011000:23>>);
enc_huffman(<<2, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111100010:28>>);
enc_huffman(<<3, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111100011:28>>);
enc_huffman(<<4, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111100100:28>>);
enc_huffman(<<5, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111100101:28>>);
enc_huffman(<<6, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111100110:28>>);
enc_huffman(<<7, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111100111:28>>);
enc_huffman(<<8, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111101000:28>>);
enc_huffman(<<9, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111101010:24>>);
enc_huffman(<<10, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111111111100:30>>);
enc_huffman(<<11, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111101001:28>>);
enc_huffman(<<12, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111101010:28>>);
enc_huffman(<<13, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111111111101:30>>);
enc_huffman(<<14, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111101011:28>>);
enc_huffman(<<15, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111101100:28>>);
enc_huffman(<<16, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111101101:28>>);
enc_huffman(<<17, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111101110:28>>);
enc_huffman(<<18, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111101111:28>>);
enc_huffman(<<19, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111110000:28>>);
enc_huffman(<<20, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111110001:28>>);
enc_huffman(<<21, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111110010:28>>);
enc_huffman(<<22, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111111111110:30>>);
enc_huffman(<<23, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111110011:28>>);
enc_huffman(<<24, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111110100:28>>);
enc_huffman(<<25, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111110101:28>>);
enc_huffman(<<26, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111110110:28>>);
enc_huffman(<<27, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111110111:28>>);
enc_huffman(<<28, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111111000:28>>);
enc_huffman(<<29, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111111001:28>>);
enc_huffman(<<30, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111111010:28>>);
enc_huffman(<<31, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111111011:28>>);
enc_huffman(<<32, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#010100:6>>);
enc_huffman(<<33, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111000:10>>);
enc_huffman(<<34, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111001:10>>);
enc_huffman(<<35, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111010:12>>);
enc_huffman(<<36, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111001:13>>);
enc_huffman(<<37, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#010101:6>>);
enc_huffman(<<38, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111000:8>>);
enc_huffman(<<39, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111010:11>>);
enc_huffman(<<40, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111010:10>>);
enc_huffman(<<41, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111011:10>>);
enc_huffman(<<42, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111001:8>>);
enc_huffman(<<43, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111011:11>>);
enc_huffman(<<44, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111010:8>>);
enc_huffman(<<45, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#010110:6>>);
enc_huffman(<<46, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#010111:6>>);
enc_huffman(<<47, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#011000:6>>);
enc_huffman(<<48, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#00000:5>>);
enc_huffman(<<49, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#00001:5>>);
enc_huffman(<<50, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#00010:5>>);
enc_huffman(<<51, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#011001:6>>);
enc_huffman(<<52, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#011010:6>>);
enc_huffman(<<53, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#011011:6>>);
enc_huffman(<<54, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#011100:6>>);
enc_huffman(<<55, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#011101:6>>);
enc_huffman(<<56, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#011110:6>>);
enc_huffman(<<57, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#011111:6>>);
enc_huffman(<<58, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1011100:7>>);
enc_huffman(<<59, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111011:8>>);
enc_huffman(<<60, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111100:15>>);
enc_huffman(<<61, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#100000:6>>);
enc_huffman(<<62, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111011:12>>);
enc_huffman(<<63, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111100:10>>);
enc_huffman(<<64, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111010:13>>);
enc_huffman(<<65, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#100001:6>>);
enc_huffman(<<66, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1011101:7>>);
enc_huffman(<<67, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1011110:7>>);
enc_huffman(<<68, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1011111:7>>);
enc_huffman(<<69, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1100000:7>>);
enc_huffman(<<70, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1100001:7>>);
enc_huffman(<<71, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1100010:7>>);
enc_huffman(<<72, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1100011:7>>);
enc_huffman(<<73, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1100100:7>>);
enc_huffman(<<74, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1100101:7>>);
enc_huffman(<<75, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1100110:7>>);
enc_huffman(<<76, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1100111:7>>);
enc_huffman(<<77, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1101000:7>>);
enc_huffman(<<78, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1101001:7>>);
enc_huffman(<<79, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1101010:7>>);
enc_huffman(<<80, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1101011:7>>);
enc_huffman(<<81, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1101100:7>>);
enc_huffman(<<82, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1101101:7>>);
enc_huffman(<<83, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1101110:7>>);
enc_huffman(<<84, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1101111:7>>);
enc_huffman(<<85, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1110000:7>>);
enc_huffman(<<86, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1110001:7>>);
enc_huffman(<<87, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1110010:7>>);
enc_huffman(<<88, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111100:8>>);
enc_huffman(<<89, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1110011:7>>);
enc_huffman(<<90, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111101:8>>);
enc_huffman(<<91, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111011:13>>);
enc_huffman(<<92, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111110000:19>>);
enc_huffman(<<93, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111100:13>>);
enc_huffman(<<94, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111100:14>>);
enc_huffman(<<95, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#100010:6>>);
enc_huffman(<<96, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111101:15>>);
enc_huffman(<<97, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#00011:5>>);
enc_huffman(<<98, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#100011:6>>);
enc_huffman(<<99, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#00100:5>>);
enc_huffman(<<100, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#100100:6>>);
enc_huffman(<<101, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#00101:5>>);
enc_huffman(<<102, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#100101:6>>);
enc_huffman(<<103, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#100110:6>>);
enc_huffman(<<104, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#100111:6>>);
enc_huffman(<<105, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#00110:5>>);
enc_huffman(<<106, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1110100:7>>);
enc_huffman(<<107, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1110101:7>>);
enc_huffman(<<108, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#101000:6>>);
enc_huffman(<<109, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#101001:6>>);
enc_huffman(<<110, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#101010:6>>);
enc_huffman(<<111, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#00111:5>>);
enc_huffman(<<112, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#101011:6>>);
enc_huffman(<<113, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1110110:7>>);
enc_huffman(<<114, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#101100:6>>);
enc_huffman(<<115, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#01000:5>>);
enc_huffman(<<116, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#01001:5>>);
enc_huffman(<<117, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#101101:6>>);
enc_huffman(<<118, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1110111:7>>);
enc_huffman(<<119, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111000:7>>);
enc_huffman(<<120, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111001:7>>);
enc_huffman(<<121, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111010:7>>);
enc_huffman(<<122, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111011:7>>);
enc_huffman(<<123, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111110:15>>);
enc_huffman(<<124, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111100:11>>);
enc_huffman(<<125, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111101:14>>);
enc_huffman(<<126, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111101:13>>);
enc_huffman(<<127, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111111100:28>>);
enc_huffman(<<128, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111100110:20>>);
enc_huffman(<<129, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111010010:22>>);
enc_huffman(<<130, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111100111:20>>);
enc_huffman(<<131, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111101000:20>>);
enc_huffman(<<132, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111010011:22>>);
enc_huffman(<<133, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111010100:22>>);
enc_huffman(<<134, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111010101:22>>);
enc_huffman(<<135, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111011001:23>>);
enc_huffman(<<136, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111010110:22>>);
enc_huffman(<<137, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111011010:23>>);
enc_huffman(<<138, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111011011:23>>);
enc_huffman(<<139, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111011100:23>>);
enc_huffman(<<140, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111011101:23>>);
enc_huffman(<<141, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111011110:23>>);
enc_huffman(<<142, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111101011:24>>);
enc_huffman(<<143, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111011111:23>>);
enc_huffman(<<144, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111101100:24>>);
enc_huffman(<<145, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111101101:24>>);
enc_huffman(<<146, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111010111:22>>);
enc_huffman(<<147, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111100000:23>>);
enc_huffman(<<148, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111101110:24>>);
enc_huffman(<<149, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111100001:23>>);
enc_huffman(<<150, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111100010:23>>);
enc_huffman(<<151, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111100011:23>>);
enc_huffman(<<152, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111100100:23>>);
enc_huffman(<<153, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111011100:21>>);
enc_huffman(<<154, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111011000:22>>);
enc_huffman(<<155, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111100101:23>>);
enc_huffman(<<156, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111011001:22>>);
enc_huffman(<<157, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111100110:23>>);
enc_huffman(<<158, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111100111:23>>);
enc_huffman(<<159, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111101111:24>>);
enc_huffman(<<160, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111011010:22>>);
enc_huffman(<<161, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111011101:21>>);
enc_huffman(<<162, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111101001:20>>);
enc_huffman(<<163, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111011011:22>>);
enc_huffman(<<164, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111011100:22>>);
enc_huffman(<<165, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111101000:23>>);
enc_huffman(<<166, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111101001:23>>);
enc_huffman(<<167, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111011110:21>>);
enc_huffman(<<168, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111101010:23>>);
enc_huffman(<<169, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111011101:22>>);
enc_huffman(<<170, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111011110:22>>);
enc_huffman(<<171, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111110000:24>>);
enc_huffman(<<172, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111011111:21>>);
enc_huffman(<<173, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111011111:22>>);
enc_huffman(<<174, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111101011:23>>);
enc_huffman(<<175, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111101100:23>>);
enc_huffman(<<176, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111100000:21>>);
enc_huffman(<<177, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111100001:21>>);
enc_huffman(<<178, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111100000:22>>);
enc_huffman(<<179, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111100010:21>>);
enc_huffman(<<180, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111101101:23>>);
enc_huffman(<<181, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111100001:22>>);
enc_huffman(<<182, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111101110:23>>);
enc_huffman(<<183, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111101111:23>>);
enc_huffman(<<184, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111101010:20>>);
enc_huffman(<<185, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111100010:22>>);
enc_huffman(<<186, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111100011:22>>);
enc_huffman(<<187, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111100100:22>>);
enc_huffman(<<188, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111110000:23>>);
enc_huffman(<<189, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111100101:22>>);
enc_huffman(<<190, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111100110:22>>);
enc_huffman(<<191, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111110001:23>>);
enc_huffman(<<192, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111111100000:26>>);
enc_huffman(<<193, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111111100001:26>>);
enc_huffman(<<194, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111101011:20>>);
enc_huffman(<<195, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111110001:19>>);
enc_huffman(<<196, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111100111:22>>);
enc_huffman(<<197, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111110010:23>>);
enc_huffman(<<198, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111101000:22>>);
enc_huffman(<<199, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111101100:25>>);
enc_huffman(<<200, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111111100010:26>>);
enc_huffman(<<201, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111111100011:26>>);
enc_huffman(<<202, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111111100100:26>>);
enc_huffman(<<203, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111011110:27>>);
enc_huffman(<<204, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111011111:27>>);
enc_huffman(<<205, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111111100101:26>>);
enc_huffman(<<206, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111110001:24>>);
enc_huffman(<<207, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111101101:25>>);
enc_huffman(<<208, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111110010:19>>);
enc_huffman(<<209, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111100011:21>>);
enc_huffman(<<210, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111111100110:26>>);
enc_huffman(<<211, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111100000:27>>);
enc_huffman(<<212, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111100001:27>>);
enc_huffman(<<213, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111111100111:26>>);
enc_huffman(<<214, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111100010:27>>);
enc_huffman(<<215, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111110010:24>>);
enc_huffman(<<216, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111100100:21>>);
enc_huffman(<<217, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111100101:21>>);
enc_huffman(<<218, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111111101000:26>>);
enc_huffman(<<219, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111111101001:26>>);
enc_huffman(<<220, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111111101:28>>);
enc_huffman(<<221, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111100011:27>>);
enc_huffman(<<222, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111100100:27>>);
enc_huffman(<<223, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111100101:27>>);
enc_huffman(<<224, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111101100:20>>);
enc_huffman(<<225, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111110011:24>>);
enc_huffman(<<226, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111101101:20>>);
enc_huffman(<<227, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111100110:21>>);
enc_huffman(<<228, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111101001:22>>);
enc_huffman(<<229, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111100111:21>>);
enc_huffman(<<230, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111101000:21>>);
enc_huffman(<<231, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111110011:23>>);
enc_huffman(<<232, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111101010:22>>);
enc_huffman(<<233, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111101011:22>>);
enc_huffman(<<234, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111101110:25>>);
enc_huffman(<<235, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111101111:25>>);
enc_huffman(<<236, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111110100:24>>);
enc_huffman(<<237, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111110101:24>>);
enc_huffman(<<238, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111111101010:26>>);
enc_huffman(<<239, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111110100:23>>);
enc_huffman(<<240, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111111101011:26>>);
enc_huffman(<<241, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111100110:27>>);
enc_huffman(<<242, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111111101100:26>>);
enc_huffman(<<243, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111111101101:26>>);
enc_huffman(<<244, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111100111:27>>);
enc_huffman(<<245, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111101000:27>>);
enc_huffman(<<246, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111101001:27>>);
enc_huffman(<<247, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111101010:27>>);
enc_huffman(<<248, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111101011:27>>);
enc_huffman(<<249, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#1111111111111111111111111110:28>>);
enc_huffman(<<250, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111101100:27>>);
enc_huffman(<<251, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111101101:27>>);
enc_huffman(<<252, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111101110:27>>);
enc_huffman(<<253, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111101111:27>>);
enc_huffman(<<254, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#111111111111111111111110000:27>>);
enc_huffman(<<255, R/bits>>, A) -> enc_huffman(R, <<A/bits, 2#11111111111111111111101110:26>>).
