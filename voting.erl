-module(voting).

-export([merge_vstructs/3, init_vstate/1, vote/3]).

-include("voting.hrl").

-spec merge_vstruct(#vstruct_v{}, #vstruct{}, #vstruct{}) -> #vstruct{}.

merge_vstruct(#vstruct_v{votes = V, thresh = T},
              #vstruct{tree = T1, indices = I1},
              #vstruct{tree = T2, indices = I2}) ->
    NewI1 = orddict:map(fun(_, Paths) ->
                                lists:map(fun(Path) -> [0|Path] end, Paths)
                        end, I1),
    NewI2 = orddict:map(fun(_, Paths) ->
                                lists:map(fun(Path) -> [1|Path] end, Paths)
                        end, I2),
    I3 = orddict:fold(fun(Id, Paths, Indices) ->
                              lists:foldl(fun(Path, Indices1) ->
                                                  orddict:append(Id, Path, Indices1)
                                          end, Indices, Paths)
                      end, NewI1, NewI2),
    Root = #vstruct_v{votes = V, thresh = T, children = [T1, T2]},
    #vstruct{tree = Root, indices = I3}.

merge_vstructs(Votes, Thresh, Structs) ->
    {Children, {_, Indices}} = lists:mapfoldl(
                                 fun(Struct, {I, AllIndices}) ->
                                         {Child, Indices} = prepend_paths(I, Struct),
                                         NewIndices = combine_indices(AllIndices, Indices),
                                         {Child, {I + 1, NewIndices}}
                                 end, {0, orddict:new()}, Structs),
    Root = #vstruct_v{votes = Votes, thresh = Thresh, children = Children},
    #vstruct{tree = Root, indices = Indices}.

combine_indices(I1, I2) ->
    orddict:fold(
      fun(Id, Paths, Indices) ->
              lists:foldl(fun(Path, Indices1) ->
                                  orddict:append(Id, Path, Indices1)
                          end, Indices, Paths)
      end, I1, I2).

prepend_paths(Index, #vstruct{tree = T, indices = I}) ->
    NewI = orddict:map(
             fun(_, Paths) ->
                     lists:map(fun(Path) -> [Index|Path] end, Paths)
             end, I),
    {T, NewI}.

-spec init_vstate(#vstruct{}) -> #vstate{}.

init_vstate(#vstruct{tree = Tree, indices = Indices}) ->
    #vstate{tree = init_vstate_rec(Tree), indices = Indices}.

-spec init_vstate_rec(#vstruct_p{}) -> #vstate_p{}
                   ; (#vstruct_v{}) -> #vstate_v{}.

init_vstate_rec(#vstruct_p{votes = V}) ->
    #vstate_p{votes = V};

init_vstate_rec(#vstruct_v{votes = V, thresh = T, children = Structs}) ->
    States = lists:map(fun init_vstate_rec/1, Structs),
    #vstate_v{votes = V, thresh = T, children = States}.

-spec vote(#vstate{}, vid(), yes | no) -> #vstate{} | accept | reject.

vote(State = #vstate{tree = Tree, indices = Indices}, Vid, Vote) ->
    Paths = orddict:fetch(Vid, Indices),
    case lists:foldl(
                fun(_Path, accept) -> accept;
                   (_Path, reject) -> reject;
                   (Path, S) -> case vote_rec(S, Path, Vote) of
                                    {accept, _} -> accept;
                                    {reject, _} -> reject;
                                    Node -> Node
                                end
                end,
                Tree, Paths) of
        accept -> accept;
        reject -> reject;
        NewTree -> State#vstate{tree = NewTree}
    end.

-spec vote_rec(#vstate_v{} | #vstate_p{}, path(), yes | no) ->
    #vstate_v{} | {accept, non_neg_integer()} | {reject, non_neg_integer()}.

vote_rec(State = #vstate_v{children = States, votes = Votes, thresh = T,
                           yes_votes = YesVotes, no_votes = NoVotes},
         [Index|Path], Vote) ->
    {Init, [Node|Tail]} = lists:split(Index, States),
    YesNode = Node#vstate_p{vote = yes},
    NoNode = Node#vstate_p{vote = no},
    case vote_rec(Node, Path, Vote) of
        {accept, V} ->
            case YesVotes + V >= T of
                true -> {accept, Votes};
                false -> State#vstate_v{children = Init ++ [YesNode|Tail],
                                        yes_votes = YesVotes + V}
            end;
        {reject, V} ->
            case NoVotes + V > length(States) - T of
                true -> {reject, Votes};
                false -> State#vstate_v{children = Init ++ [NoNode|Tail],
                                        no_votes = NoVotes + V}
            end;
        NewNode ->
            State#vstate_v{children = Init ++ [NewNode|Tail]}
    end;

vote_rec(#vstate_p{vote = pending, votes = V}, [], yes) -> {accept, V};
vote_rec(#vstate_p{vote = pending, votes = V}, [], no) -> {reject, V}.
