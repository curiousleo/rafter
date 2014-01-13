-module(rafter_voting).

-export([quorum/2, merge_vstructs/3, init_vstate/1, vote/3, vote/1,
         to_list/1, member/2]).

-include("rafter.hrl").

-spec merge_vstructs(pos_integer(), pos_integer(), [#vstruct{}]) ->
    #vstruct{}.
merge_vstructs(Votes, Thresh, Structs0) ->
    {Structs1, {_, Indices}} = lists:mapfoldl(
                                 fun(Struct0, {I, AccIndices}) ->
                                         {Struct1, Indices} = prepend_paths(
                                                                I, Struct0),
                                         NewIndices = combine_indices(
                                                        AccIndices, Indices),
                                         {Struct1, {I + 1, NewIndices}}
                                 end, {0, dict:new()}, Structs0),
    Root = #vstruct_v{votes = Votes, thresh = Thresh, children = Structs1},
    #vstruct{tree = Root, indices = Indices}.

-spec combine_indices(dict(), dict()) -> dict().
combine_indices(IndicesA, IndicesB) ->
    dict:fold(
      fun(Id, Paths, AccIndices) ->
              lists:foldl(fun(Path, Indices) ->
                                  dict:append(Id, Path, Indices)
                          end, AccIndices, Paths)
      end, IndicesA, IndicesB).

-spec prepend_paths(non_neg_integer(), #vstruct{}) ->
    {#vstruct_p{} | #vstruct_v{}, dict()}.
prepend_paths(Index, #vstruct{tree = Tree, indices = Indices}) ->
    NewIndices = dict:map(
                   fun(_, Paths) ->
                           lists:map(fun(Path) -> [Index|Path] end, Paths)
                   end, Indices),
    {Tree, NewIndices}.

-spec init_vstate(#vstruct{}) -> #vstate{}.
init_vstate(#vstruct{tree = Tree, indices = Indices}) ->
    #vstate{tree = init_vstate_rec(Tree), indices = Indices}.

-spec init_vstate_rec(#vstruct_p{}) -> #vstate_p{}
                   ; (#vstruct_v{}) -> #vstate_v{}.
init_vstate_rec(#vstruct_p{votes = V}) ->
    #vstate_p{votes = V};
init_vstate_rec(#vstruct_v{votes = V, thresh = T, children = Structs}) ->
    StatesList = lists:map(fun init_vstate_rec/1, Structs),
    States = array:fix(array:from_list(StatesList)),
    #vstate_v{votes = V, thresh = T,
              children = States, num_children = length(Structs)}.

-spec quorum(#vstruct{}, dict()) -> boolean().
quorum(Struct, Votes) ->
    State0 = init_vstate(Struct),
    State1 = dict:fold(
               fun(_Vid, _Vote, true) -> true;
                  (_Vid, _Vote, false) -> false;
                  (Vid, Vote, State) ->
                       NewState = vote(State, Vid, Vote),
                       case vote(NewState) of
                           pending -> NewState;
                           Result -> Result
                       end
               end,
               State0, Votes),
    case State1 of true -> true; _ -> false end.

-spec vote(#vstate{}, peer(), boolean()) -> #vstate{}.
vote(S = #vstate{tree = Tree, indices = Indices}, Vid, Vote) ->
    try dict:fetch(Vid, Indices) of
        Paths ->
            NewTree = lists:foldl(
                        fun(Path, State) -> vote_rec(State, Path, Vote) end,
                        Tree, Paths),
            #vstate{tree = NewTree, indices = dict:erase(Vid, Indices)}
    catch
        error:badarg ->
            %% node identifier not found in indices list
            S
    end.

-spec vote(#vstate{} | #vstate_v{} | #vstate_p{}) -> vote().
vote(#vstate_p{vote = V}) ->
    V;
vote(#vstate_v{yes_votes = Yes, thresh = T})
  when Yes >= T ->
    true;
vote(#vstate_v{thresh = T, no_votes = No, num_children = N})
  when No > N - T ->
    false;
vote(#vstate_v{}) ->
    pending;
vote(#vstate{tree = T}) ->
    vote(T).

-spec vote_rec(#vstate_v{} | #vstate_p{}, path(), boolean()) ->
    #vstate_v{} | #vstate_p{}.
vote_rec(State = #vstate_v{children = States}, [Index|Path], Vote) ->
    SubState= vote_rec(array:get(Index, States), Path, Vote),
    NewStates = array:set(Index, SubState, States),
    acc_votes(State#vstate_v{children = NewStates});
vote_rec(State = #vstate_p{vote = pending}, [], Vote) ->
    State#vstate_p{vote = Vote}.

-spec acc_votes(#vstate_v{}) -> #vstate_v{}.
acc_votes(State = #vstate_v{children = States}) ->
    {Yes, No} = array:foldl(
                  fun(_, S, {Yes, No}) ->
                          case vote(S) of
                              true -> {Yes + 1, No};
                              false -> {Yes, No + 1};
                              _ -> {Yes, No}
                          end
                  end, {0, 0}, States),
    State#vstate_v{yes_votes = Yes, no_votes = No}.

-spec to_list(#vstate{} | #vstruct{} | undefined) -> [peer()].
to_list(#vstate{indices = Indices}) ->
    dict:fetch_keys(Indices);
to_list(#vstruct{indices = Indices}) ->
    dict:fetch_keys(Indices).

-spec member(peer(), #vstate{} | #vstruct{}) -> boolean().
member(I, S) ->
    lists:member(I, to_list(S)).
