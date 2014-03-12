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
    SubTrees = lists:map(fun(SubIds) -> tree(SubIds, D) end,
                         chunk(ceil(length(Ids) / D), Ids)),
    #vstruct{tree = Tree, indices = Indices} = rafter_voting:merge_vstructs(
                                                 1, 1, SubTrees),
    Root = #vstruct_p{id = RootId},
    NewTree = #vstruct_v{thresh = 2, children = [Tree, Root]},
    NewIndices = dict:append(RootId, [length(Tree#vstruct_v.children)], Indices),
    #vstruct{tree = NewTree, indices = NewIndices};
tree([RootId|Ids], _D) ->
    % length(Ids) <= D
    Leaves = lists:map(fun(Id) -> #vstruct_p{id = Id} end, Ids),
    {Indices, _} = lists:mapfoldl(fun(Id, K) -> {{Id, [[0, K]]}, K+1} end,
                                  0, Ids),
    Tree = #vstruct_v{thresh = 1, children = Leaves},
    Root = #vstruct_p{id = RootId},
    NewTree = #vstruct_v{thresh = 2, children = [Tree, Root]},
    NewIndices = dict:append(RootId, [1], dict:from_list(Indices)),
    #vstruct{tree = NewTree, indices = NewIndices}.

-spec drop(non_neg_integer(), list()) -> list().
drop(_, []) ->
    [];
drop(0, L) ->
    L;
drop(N, [_|L]) ->
    drop(N-1, L).

-spec chunk(non_neg_integer(), list()) -> [ list() ].
chunk(_, []) ->
    [];
chunk(N, L) ->
    [lists:sublist(L, N) | chunk(N, drop(N, L))].

ceil(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.
