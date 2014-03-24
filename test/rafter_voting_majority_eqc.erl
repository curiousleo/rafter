-module(rafter_voting_majority_eqc).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("rafter.hrl").

-compile(export_all).

eqc_test_() ->
    ?_assertEqual(true, eqc:quickcheck(eqc:numtests(50, prop_majority_quorum()))).

prop_majority_quorum() ->
    ?FORALL(N, choose(2, 100),
        ?FORALL({Yes, Nodes}, {choose(1, N), shuffle(lists:seq(1, N))},
            begin
                YesVotes = lists:sublist(Nodes, Yes),
                IsQuorum = length(YesVotes) > N div 2,
                IsQuorum =:= has_quorum(Nodes, YesVotes)
            end)
        ).

has_quorum(Nodes, YesVotes) ->
    Struct = rafter_voting_majority:majority(Nodes),
    Votes = dict:from_list([{Node, true} || Node <- YesVotes]),
    rafter_voting:quorum(Struct, Votes).

-endif.
