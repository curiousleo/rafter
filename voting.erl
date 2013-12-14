-module(voting).

-export([merge_vstructs/3, init_vstate/1, vote/3, vote/1]).

-include("voting.hrl").

-spec merge_vstructs(non_neg_integer(), non_neg_integer(), [#vstruct{}]) ->
    #vstruct{}.
merge_vstructs(Votes, Thresh, Structs) ->
    {Children, {_, Indices}} = lists:mapfoldl(
                                 fun(Struct, {I, AllIndices}) ->
                                         {Child, Indices} = prepend_paths(
                                                              I, Struct),
                                         NewIndices = combine_indices(
                                                        AllIndices, Indices),
                                         {Child, {I + 1, NewIndices}}
                                 end, {0, orddict:new()}, Structs),
    Root = #vstruct_v{votes = Votes, thresh = Thresh, children = Children},
    #vstruct{tree = Root, indices = Indices}.

-spec combine_indices([ index() ], [ index() ]) -> [ index() ].
combine_indices(I1, I2) ->
    orddict:fold(
      fun(Id, Paths, Indices) ->
              lists:foldl(fun(Path, Indices1) ->
                                  orddict:append(Id, Path, Indices1)
                          end, Indices, Paths)
      end, I1, I2).

-spec prepend_paths(non_neg_integer(), #vstruct{}) ->
    {#vstruct_p{} | #vstruct_v{}, [ index() ]}.
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

-spec vote(#vstate{}, vid(), yes | no) -> #vstate{}.
vote(#vstate{tree = Tree, indices = Indices}, Vid, Vote) ->
    Paths = orddict:fetch(Vid, Indices),
    NewTree = lists:foldl(
                fun(Path, State) ->
                        vote_rec(State, Path, Vote)
                end,
                Tree, Paths),
    #vstate{tree = NewTree, indices = orddict:erase(Vid, Indices)}.

-spec vote(#vstate{} | #vstate_v{} | #vstate_p{}) -> vote().
vote(#vstate_v{yes_votes = Yes, thresh = T})
  when Yes >= T ->
    yes;
vote(#vstate_v{thresh = T, no_votes = No, children = States})
  when No > length(States) - T ->
    no;
vote(#vstate_v{}) ->
    pending;
vote(#vstate{tree = T}) ->
    vote(T);
vote(#vstate_p{vote = V}) ->
    V.

-spec vote_rec(#vstate_v{} | #vstate_p{}, path(), yes | no) ->
    #vstate_v{} | #vstate_p{}.
vote_rec(State = #vstate_v{children = States}, [Index|Path], Vote) ->
    {States1, [SubState|States2]} = lists:split(Index, States),
    NewSubState= vote_rec(SubState, Path, Vote),
    NewState = State#vstate_v{children = States1 ++ [NewSubState|States2]},
    acc_votes(NewState);
vote_rec(State = #vstate_p{vote = pending}, [], Vote) ->
    State#vstate_p{vote = Vote}.

-spec acc_votes(#vstate_v{}) -> #vstate_v{}.
acc_votes(State = #vstate_v{children = States}) ->
    Voted = fun(Vote) -> fun(S) -> vote(S) =:= Vote end end,
    Yes = length(lists:filter(Voted(yes), States)),
    No = length(lists:filter(Voted(no), States)),
    State#vstate_v{yes_votes = Yes, no_votes = No}.
