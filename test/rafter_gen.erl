-module(rafter_gen).

-compile([export_all]).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").

-define(MAX_RUNNING, 40).

%% ====================================================================
%% EQC Generators 
%% ====================================================================

server() ->
    ?LET(Servers, servers(), oneof(Servers)).

vsgen() ->
    oneof([{rafter_voting_majority, majority},
           {rafter_voting_tree, tree},
           {rafter_voting_grid, grid}]).

vstruct(Peers) when is_list(Peers) ->
    ?LET({{Mod, Fun}, D, R}, {vsgen(), oneof([2, 3, 4]), bool()},
         case Fun of
             tree -> apply(Mod, Fun, [Peers, D]);
             grid -> apply(Mod, Fun, [Peers, R]);
             _ -> apply(Mod, Fun, [Peers])
         end);
vstruct(Peer) ->
    ?LET(Servers, servers(), vstruct(shuffle([Peer|Servers]))).

vstruct() ->
    ?LET(Servers, servers(), vstruct(Servers)).

servers() ->
    ?LET(N, choose(3, ?MAX_RUNNING),
         shuffle([list_to_atom(integer_to_list(I)) || I <- lists:seq(1, N)])).

max_running() ->
    ?MAX_RUNNING.

non_neg_integer() -> 
    ?LET(X, int(), abs(X)).

-endif.
