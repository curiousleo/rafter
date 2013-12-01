-module(voting).

-export([init_vstate/1, vote/3]).

-include("voting.hrl").

-spec init_vstate(#vstruct{}) -> #vstate{}.

init_vstate(#vstruct{tree = Tree, indices = Indices}) ->
    #vstate{tree = init_vstate_tree(undefined, Tree), indices = Indices}.

-spec init_vstate_tree(#vstruct_v{} | undefined, #vstruct_p{} | #vstruct_v{}) ->
    #vstate_p{} | #vstate_v{}.

init_vstate_tree(Parent, #vstruct_p{}) ->
    #vstate_p{parent = Parent};

init_vstate_tree(Parent,
                 Struct = #vstruct_v{thresh = T, children = Structs}) ->
    States = lists:map(fun(Child) -> init_vstate_tree(Struct, Child) end,
                       Structs),
    #vstate_v{parent = Parent, children = States, thresh = T}.

-spec vote(#vstate_v{} | #vstate_p{}, [ non_neg_integer ], yes | no) ->
    #vstate_v{} | accept | reject.

vote(State = #vstate_v{children = States, yes_votes = YesVotes, thresh = T,
                       no_votes = NoVotes},
     [Index|Path], Vote) ->
    {Init, [Node1|Tail]} = lists:split(Index, States),
    case vote(Node1, Path, Vote) of
        accept ->
            case YesVotes + 1 >= T of
                true -> accept;
                false -> State#vstate_v{yes_votes = YesVotes + 1}
            end;
        reject ->
            case NoVotes + 1 > length(States) - T of
                true -> reject;
                false -> State#vstate_v{no_votes = NoVotes + 1}
            end;
        Node2 ->
            State#vstate_v{children = Init ++ [Node2|Tail]}
    end;

vote(#vstate_p{vote = pending}, [], yes) -> accept;
vote(#vstate_p{vote = pending}, [], no) -> reject.
