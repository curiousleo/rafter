-module(rafter_voting_grid).

-export([grid/2, grid/1]).

-include("rafter.hrl").
-type grid_spec() :: {pos_integer(), pos_integer(), non_neg_integer()}.

-spec grid([peer()]) -> #vstruct{}.
grid(Ids) ->
    grid(Ids, true).

-spec grid([peer()], boolean()) -> #vstruct{}.
grid(Ids, FavoringRows) ->
    GridSpec = makeGrid(length(Ids), FavoringRows),
    rafter_voting:merge_vstructs(1, 2, [columnCovers(Ids, GridSpec),
                                        completeColumnCovers(Ids, GridSpec)]).

-spec columnCovers([peer()], grid_spec()) -> #vstruct{}.
columnCovers(Ids, {Rows, Cols, D}) ->
    Covers = lists:map(
               fun(Col) -> columnCover(Ids, Col, {Rows, Cols, D}) end,
               lists:seq(1, Cols)),
    rafter_voting:merge_vstructs(1, Cols, Covers).

-spec completeColumnCovers([peer()], grid_spec()) -> #vstruct{}.
completeColumnCovers(Ids, {Rows, Cols, D}) ->
    Covers = lists:map(
               fun(Col) -> completeColumnCover(Ids, Col, {Rows, Cols, D}) end,
               lists:seq(1, Cols)),
    rafter_voting:merge_vstructs(1, 1, Covers).


-spec completeColumnCover([peer()], non_neg_integer(), grid_spec()) ->
    #vstruct{}.
completeColumnCover(Ids, Col, {_Rows, Cols, _D}) ->
    RowsIds = rafter_voting:chunk(Cols, Ids),
    %% ColIds = lists:map(fun(RowIds) -> lists:nth(Col, RowIds) end, RowsIds),
    %% cannot use map here: lists:nth may fail
    ColIds = lists:foldl(
               fun(RowIds, Acc) ->
                       try lists:nth(Col, RowIds) of
                           Id -> Acc ++ [Id]
                       catch
                           error:function_clause -> Acc
                       end
               end, [], RowsIds),
    Phys = lists:map(fun(Id) -> #vstruct_p{id = Id} end, ColIds),
    {Indices, _} = lists:mapfoldl(fun(Id, K) -> {{Id, [[K]]}, K+1} end,
                                  0, ColIds),
    #vstruct{tree = #vstruct_v{thresh = length(Phys), children = Phys},
             indices = dict:from_list(Indices)}.

-spec columnCover([peer()], non_neg_integer(), grid_spec()) -> #vstruct{}.
columnCover(Ids, Col, GridSpec) ->
    #vstruct{tree = T, indices = I} = completeColumnCover(Ids, Col, GridSpec),
    #vstruct{tree = T#vstruct_v{thresh = 1}, indices = I}.

-spec makeGrid(pos_integer(), boolean()) -> grid_spec().
makeGrid(N, FavoringRows) ->
    {Rows, Cols} = makeGridFavoringRows(N),
    D = Rows * Cols - N,
    if FavoringRows -> {Rows, Cols, D};
       true -> {Cols, Rows, D}
    end.

-spec makeGridFavoringRows(pos_integer()) ->
    {pos_integer(), pos_integer()}.
makeGridFavoringRows(N) when N < 4 ->
    {N, 1};
makeGridFavoringRows(N) ->
    Sqrt = math:sqrt(N),
    Rows = rafter_voting:ceil(Sqrt),
    Floor = rafter_voting:floor(Sqrt),
    Cols = if Rows * Floor < N -> Rows;
              true -> Floor
           end,
    {Rows, Cols}.
