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
    {NewTree, _SubVote} = lists:foldl(
                            fun(Path, {State, _SubVote}) ->
                                    vote_rec(State, Path, Vote)
                            end,
                            {Tree, pending}, Paths),
    #vstate{tree = NewTree, indices = orddict:erase(Vid, Indices)}.

-spec vote(#vstate{}) -> vote().
vote(#vstate{tree = #vstate_v{yes_votes = Yes, thresh = T}})
  when Yes >= T ->
    yes;
vote(#vstate{tree = #vstate_v{thresh = T, no_votes = No, children = States}})
  when No > length(States) - T ->
    no;
vote(#vstate{tree = #vstate_v{}}) ->
    pending.

-spec vote_rec(#vstate_v{} | #vstate_p{}, path(), yes | no) ->
    {#vstate_v{} | #vstate_p{}, vote()}.
vote_rec(State = #vstate_v{children = States}, [Index|Path], Vote) ->
    {States1, [SubState|States2]} = lists:split(Index, States),
    {NewSubState, SubVote} = vote_rec(SubState, Path, Vote),
    NewState = State#vstate_v{children = States1 ++ [NewSubState|States2]},
    vote_rec_aux(NewState, votes(NewSubState), SubVote);
vote_rec(State = #vstate_p{vote = pending}, [], Vote) ->
    {State#vstate_p{vote = Vote}, Vote}.

-spec vote_rec_aux(#vstate_v{}, non_neg_integer(), vote()) ->
    {#vstate_v{}, vote()}.
vote_rec_aux(State = #vstate_v{yes_votes = Yes, thresh = T}, Votes, yes)
  when Yes + Votes >= T ->
    {State#vstate_v{yes_votes = Yes + Votes}, yes};
vote_rec_aux(State = #vstate_v{yes_votes = Yes}, Votes, yes) ->
    {State#vstate_v{yes_votes = Yes + Votes}, pending};
vote_rec_aux(State = #vstate_v{thresh = T, no_votes = No, children = States},
             Votes, no)
  when No + Votes > length(States) - T ->
    {State#vstate_v{no_votes = No + Votes}, no};
vote_rec_aux(State = #vstate_v{no_votes = No}, Votes, no) ->
    {State#vstate_v{no_votes = No + Votes}, pending};
vote_rec_aux(State, _Votes, pending) ->
    {State, pending}.

-spec votes(#vstate_v{} | #vstate_p{}) -> non_neg_integer().
votes(#vstate_v{votes = V}) -> V;
votes(#vstate_p{votes = V}) -> V.
