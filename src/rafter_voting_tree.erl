-module(rafter_voting_tree).

-export([tree/1, tree/2]).

-include("rafter.hrl").

-spec tree([peer()]) -> #vstruct{}.
tree(Ids) ->
    tree(Ids, 2).

-spec tree([peer()], pos_integer()) -> #vstruct{}.
tree([RootId], _D) ->
    #vstruct{tree = #vstruct_v{thresh = 1,
                               children = [#vstruct_p{id = RootId}]},
             indices = dict:from_list([{RootId, [[0]]}])};
tree([RootId|Ids], D)
  when length(Ids) > D ->
    SubIdsList = rafter_voting:chunk(rafter_voting:ceil(length(Ids) / D), Ids),
    SubTrees = [tree(SubIds, D) || SubIds <- SubIdsList],
    #vstruct{tree = Tree, indices = Indices} = rafter_voting:merge_vstructs(
                                                 1, 1, SubTrees),
    Root = #vstruct_p{id = RootId},
    NewTree = #vstruct_v{thresh = 2, children = [Tree, Root]},
    NewIndices = dict:map(fun(_, Paths) -> [[0|P] || P <- Paths] end, Indices),
    #vstruct{tree = NewTree, indices = dict:append(RootId, [1], NewIndices)};
tree([RootId|Ids], _D) ->
    % length(Ids) <= D
    Leaves = [#vstruct_p{id = Id} || Id <- Ids],
    {Indices, _} = lists:mapfoldl(fun(Id, K) -> {{Id, [[0, K]]}, K+1} end,
                                  0, Ids),
    Tree = #vstruct_v{thresh = 1, children = Leaves},
    Root = #vstruct_p{id = RootId},
    NewTree = #vstruct_v{thresh = 2, children = [Tree, Root]},
    NewIndices = dict:append(RootId, [1], dict:from_list(Indices)),
    #vstruct{tree = NewTree, indices = NewIndices}.
