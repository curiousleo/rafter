-module(rafter_gen).

-compile([export_all]).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").

-define(MAX_RUNNING, 40).

%% ====================================================================
%% EQC Generators 
%% ====================================================================
me() ->
    peer1.

peers() ->
    [peer2, peer3, peer4, peer5].

peer() ->
    oneof(peers()).

server() ->
    ?LET(Servers, servers(), oneof(Servers)).

vsgen() ->
    oneof([{rafter_voting_majority, majority},
           {rafter_voting_grid, grid}]).

vstruct(Peers) when is_list(Peers) ->
    ?LET({Mod, Fun}, vsgen(), apply(Mod, Fun, [Peers]));
vstruct(Peer) ->
    ?LET(Servers, servers(),
         vstruct(shuffle([Peer|Servers]))).

vstruct() ->
    ?LET(Servers, servers(), vstruct(Servers)).

servers() ->
    ?LET(N, choose(3, ?MAX_RUNNING),
         shuffle([list_to_atom(integer_to_list(I)) || I <- lists:seq(0, N)])).

max_running() ->
    ?MAX_RUNNING.

%% Generate a lower 7-bit ACSII character that should not cause any problems
%% with utf8 conversion.
lower_char() ->
    choose(16#20, 16#7f).

not_empty(G) ->
    ?SUCHTHAT(X, G, X /= [] andalso X /= <<>>).

non_blank_string() ->
    ?LET(X, not_empty(list(lower_char())), list_to_binary(X)).

non_neg_integer() -> 
    ?LET(X, int(), abs(X)).

consistent_terms() ->
    ?SUCHTHAT({CurrentTerm, LastLogTerm}, 
              {non_neg_integer(), non_neg_integer()},
              CurrentTerm >= LastLogTerm).

uuid() ->
    druuid:v4().

-endif.
