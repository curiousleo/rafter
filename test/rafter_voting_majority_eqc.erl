-module(rafter_voting_majority_eqc).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("rafter.hrl").

-compile(export_all).

prop_majority_quorum() ->
    ?FORALL(N, nat(),
        ?FORALL(YesVotes,
            lists:sublist(
              choose(0, N),
              shuffle(lists:seq(0, N))),
            begin
                Nodes = lists:seq(0, N),
                IsQuorum = length(Nodes) > N div 2,
                IsQuorum =:= has_quorum(Nodes, YesVotes)
            end)
        ).

has_quorum(Nodes, YesVotes) ->
    Struct = rafter_voting_majority:majority(Nodes),
    Votes = dict:from_list([{Node, true} || Node <- YesVotes]),
    rafter_voting:quorum(Struct, Votes).

-endif.
