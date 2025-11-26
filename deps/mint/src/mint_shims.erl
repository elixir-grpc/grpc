%% Shims for functions introduced in recent Erlang/OTP releases,
%% to enable use of Mint on older releases. The code in this module
%% was taken directly from the Erlang/OTP project.
%%
%% File: lib/public_key/src/public_key.erl
%% Tag: OTP-20.3.4
%% Commit: f2c1d537dc28ffbde5d42aedec70bf4c6574c3ea
%% Changes from original file:
%% - extracted pkix_verify_hostname/2 and /3, and any private
%%   functions they depend upon
%% - replaced local calls to other public functions in the
%%   'public_key' module with fully qualified equivalents
%% - replaced local type references with fully qualified equivalents
%%
%% The original license follows:

%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2017. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(mint_shims).

-include_lib("public_key/include/public_key.hrl").

-export([pkix_verify_hostname/2, pkix_verify_hostname/3]).

%--------------------------------------------------------------------
-spec pkix_verify_hostname(Cert :: #'OTPCertificate'{} | binary(),
			   ReferenceIDs :: [{uri_id | dns_id | ip | srv_id | public_key:oid(),  string()}]) -> boolean().

-spec pkix_verify_hostname(Cert :: #'OTPCertificate'{} | binary(),
			   ReferenceIDs :: [{uri_id | dns_id | ip | srv_id | public_key:oid(),  string()}],
			   Options :: proplists:proplist()) -> boolean().

%% Description: Validates a hostname to RFC 6125
%%--------------------------------------------------------------------
pkix_verify_hostname(Cert, ReferenceIDs) ->
    pkix_verify_hostname(Cert, ReferenceIDs, []).

pkix_verify_hostname(BinCert, ReferenceIDs, Options)  when is_binary(BinCert) ->
    pkix_verify_hostname(public_key:pkix_decode_cert(BinCert,otp), ReferenceIDs, Options);

pkix_verify_hostname(Cert = #'OTPCertificate'{tbsCertificate = TbsCert}, ReferenceIDs0, Opts) ->
    MatchFun = proplists:get_value(match_fun,     Opts, undefined),
    FailCB   = proplists:get_value(fail_callback, Opts, fun(_Cert) -> false end),
    FqdnFun  = proplists:get_value(fqdn_fun,      Opts, fun verify_hostname_extract_fqdn_default/1),

    ReferenceIDs = [{T,to_string(V)} || {T,V} <- ReferenceIDs0],
    PresentedIDs =
	try lists:keyfind(?'id-ce-subjectAltName',
			  #'Extension'.extnID,
			  TbsCert#'OTPTBSCertificate'.extensions)
	of
	    #'Extension'{extnValue = ExtVals} ->
		[{T,to_string(V)} || {T,V} <- ExtVals];
	    false ->
		[]
	catch
	    _:_ -> []
	end,
    %% PresentedIDs example: [{dNSName,"ewstest.ericsson.com"}, {dNSName,"www.ericsson.com"}]}
    case PresentedIDs of
	[] ->
	    %% Fallback to CN-ids [rfc6125, ch6]
	    case TbsCert#'OTPTBSCertificate'.subject of
		{rdnSequence,RDNseq} ->
		    PresentedCNs =
			[{cn, to_string(V)}
			 || ATVs <- RDNseq, % RDNseq is list-of-lists
			    #'AttributeTypeAndValue'{type = ?'id-at-commonName',
						     value = {_T,V}} <- ATVs
						% _T = kind of string (teletexString etc)
			],
		    %% Example of PresentedCNs:  [{cn,"www.ericsson.se"}]
		    %% match ReferenceIDs to PresentedCNs
		    verify_hostname_match_loop(verify_hostname_fqnds(ReferenceIDs, FqdnFun),
					       PresentedCNs,
					       MatchFun, FailCB, Cert);

		_ ->
		    false
	    end;
	_ ->
	    %% match ReferenceIDs to PresentedIDs
	    case verify_hostname_match_loop(ReferenceIDs, PresentedIDs,
					    MatchFun, FailCB, Cert) of
		false ->
		    %% Try to extract DNS-IDs from URIs etc
		    DNS_ReferenceIDs =
			[{dns_id,X} || X <- verify_hostname_fqnds(ReferenceIDs, FqdnFun)],
		    verify_hostname_match_loop(DNS_ReferenceIDs, PresentedIDs,
					       MatchFun, FailCB, Cert);
		true ->
		    true
	    end
    end.

%%%----------------------------------------------------------------
%%% pkix_verify_hostname help functions
verify_hostname_extract_fqdn_default({dns_id,S}) ->
    S;
verify_hostname_extract_fqdn_default({uri_id,URI}) ->
    % Modified from original to remove dependency on http_uri:parse/1 from inets
    #{scheme := <<"https">>, host := Host} = 'Elixir.URI':parse(list_to_binary(URI)),
    binary_to_list(Host).


verify_hostname_fqnds(L, FqdnFun) ->
    [E || E0 <- L,
	  E <- [try case FqdnFun(E0) of
			default -> verify_hostname_extract_fqdn_default(E0);
                        undefined -> undefined; % will make the "is_list(E)" test fail
			Other -> Other
		    end
		catch _:_-> undefined % will make the "is_list(E)" test fail
		end],
	  is_list(E),
	  E =/= "",
	  {error,einval} == inet:parse_address(E)
    ].


-define(srvName_OID, {1,3,6,1,4,1,434,2,2,1,37,0}).

verify_hostname_match_default(Ref, Pres) ->
    verify_hostname_match_default0(to_lower_ascii(Ref), to_lower_ascii(Pres)).

verify_hostname_match_default0(FQDN=[_|_], {cn,FQDN}) ->
    not lists:member($*, FQDN);
verify_hostname_match_default0(FQDN=[_|_], {cn,Name=[_|_]}) ->
    [F1|Fs] = string:tokens(FQDN, "."),
    [N1|Ns] = string:tokens(Name, "."),
    match_wild(F1,N1) andalso Fs==Ns;
verify_hostname_match_default0({dns_id,R}, {dNSName,P}) ->
    R==P;
verify_hostname_match_default0({uri_id,R}, {uniformResourceIdentifier,P}) ->
    R==P;
verify_hostname_match_default0({ip,R}, {iPAddress,P}) when length(P) == 4 ->
    %% IPv4
    try
        list_to_tuple(P)
            == if is_tuple(R), size(R)==4 -> R;
                  is_list(R) -> ok(inet:parse_ipv4strict_address(R))
               end
    catch
        _:_ ->
            false
    end;

verify_hostname_match_default0({ip,R}, {iPAddress,P}) when length(P) == 16 ->
    %% IPv6. The length 16 is due to the certificate specification.
    try
        l16_to_tup(P)
            == if is_tuple(R), size(R)==8 -> R;
                  is_list(R) -> ok(inet:parse_ipv6strict_address(R))
               end
    catch
        _:_ ->
            false
    end;
verify_hostname_match_default0({srv_id,R}, {srvName,P}) ->
    R==P;
verify_hostname_match_default0({srv_id,R}, {?srvName_OID,P}) ->
    R==P;
verify_hostname_match_default0(_, _) ->
    false.

ok({ok,X}) -> X.

l16_to_tup(L) -> list_to_tuple(l16_to_tup(L, [])).
%%
l16_to_tup([A,B|T], Acc) -> l16_to_tup(T, [(A bsl 8) bor B | Acc]);
l16_to_tup([], Acc) -> lists:reverse(Acc).

match_wild(A,     [$*|B]) -> match_wild_suffixes(A, B);
match_wild([C|A], [ C|B]) -> match_wild(A, B);
match_wild([],        []) -> true;
match_wild(_,          _) -> false.

%% Match the parts after the only wildcard by comparing them from the end
match_wild_suffixes(A, B) -> match_wild_sfx(lists:reverse(A), lists:reverse(B)).

match_wild_sfx([$*|_],      _) -> false; % Bad name (no wildcards allowed)
match_wild_sfx(_,      [$*|_]) -> false; % Bad pattern (no more wildcards allowed)
match_wild_sfx([A|Ar], [A|Br]) -> match_wild_sfx(Ar, Br);
match_wild_sfx(Ar,         []) -> not lists:member($*, Ar); % Chk for bad name (= wildcards)
match_wild_sfx(_,           _) -> false.


verify_hostname_match_loop(Refs0, Pres0, undefined, FailCB, Cert) ->
    Pres = lists:map(fun to_lower_ascii/1, Pres0),
    Refs = lists:map(fun to_lower_ascii/1, Refs0),
    lists:any(
      fun(R) ->
	      lists:any(fun(P) ->
                                verify_hostname_match_default(R,P) orelse FailCB(Cert)
			end, Pres)
      end, Refs);
verify_hostname_match_loop(Refs, Pres, MatchFun, FailCB, Cert) ->
    lists:any(
      fun(R) ->
	      lists:any(fun(P) ->
				(case MatchFun(R,P) of
				     default -> verify_hostname_match_default(R,P);
				     Bool -> Bool
				 end) orelse FailCB(Cert)
			end,
			Pres)
      end,
      Refs).


to_lower_ascii({ip,_}=X) -> X;
to_lower_ascii({iPAddress,_}=X) -> X;
to_lower_ascii(S) when is_list(S) -> lists:map(fun to_lower_ascii/1, S);
to_lower_ascii({T,S}) -> {T, to_lower_ascii(S)};
to_lower_ascii(C) when $A =< C,C =< $Z -> C + ($a-$A);
to_lower_ascii(C) -> C.

to_string(S) when is_list(S) -> S;
to_string(B) when is_binary(B) -> binary_to_list(B);
to_string(X) -> X.
