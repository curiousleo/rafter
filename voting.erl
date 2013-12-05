-module(voting).

-export([init_vstate/1, vote/3]).

-include("voting.hrl").

-spec init_vstate(#vstruct_v{}) -> #vstate{}.

init_vstate(Struct) -> init_vstate_rec(undefined, Struct).

-spec init_vstate_rec(#vstruct_v{} | undefined, #vstruct_p{}) -> #vstate{}
                   ; (#vstruct_v{} | undefined, #vstruct_v{}) -> #vstate{}.

init_vstate_rec(Parent, #vstruct_p{id = Id}) ->
    Leaf = #vstate_p{parent = Parent},
    #vstate{state = Leaf, phys = [{Id, Leaf}]};

init_vstate_rec(Parent,
                Struct = #vstruct_v{thresh = T, children = Structs}) ->
    States = lists:map(fun(Child) -> init_vstate_rec(Struct, Child) end,
                       Structs),
    #vstate_v{parent = Parent, children = States, thresh = T}.

-spec vote(#vstate{}, vid(), yes | no) -> #vstate{} | accept | reject.

vote(State = #vstate{tree = Tree, phys = Phys}, Vid, Vote) ->
    Paths = orddict:fetch(Vid, Phys),
    case lists:foldl(
                fun(accept, _Path) -> accept;
                   (reject, _Path) -> reject;
                   (S, Path) -> vote_rec(S, Path, Vote) end,
                Tree, Paths) of
        accept -> accept;
        reject -> reject;
        NewTree -> State#vstate{tree = NewTree}
    end.

-spec vote_rec(#vstate_v{} | #vstate_p{}, [ non_neg_integer ], yes | no) ->
    #vstate_v{} | accept | reject.

vote_rec(State = #vstate_v{children = States, yes_votes = YesVotes,
                           thresh = T, no_votes = NoVotes},
         [Index|Path], Vote) ->
    {Init, [Node|Tail]} = lists:split(Index, States),
    case vote_rec(Node, Path, Vote) of
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
        NewNode ->
            State#vstate_v{children = Init ++ [NewNode|Tail]}
    end;

vote_rec(#vstate_p{vote = pending}, [], yes) -> accept;
vote_rec(#vstate_p{vote = pending}, [], no) -> reject.
