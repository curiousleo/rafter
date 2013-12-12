-module(voting).

-export([merge_vstructs/3, init_vstate/1, vote/3]).

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

-spec vote(#vstate{}, vid(), yes | no) -> #vstate{} | accept | reject.

vote(#vstate{tree = Tree, indices = Indices}, Vid, Vote) ->
    Paths = orddict:fetch(Vid, Indices),
    case lists:foldl(
                fun(_Path, accept) -> accept;
                   (_Path, reject) -> reject;
                   (Path, S) -> case vote_rec(S, Path, Vote) of
                                    {_, yes} -> accept;
                                    {_, no} -> reject;
                                    {Node, pending} -> Node
                                end
                end,
                Tree, Paths) of
        accept -> accept;
        reject -> reject;
        NewTree -> #vstate{tree = NewTree,
                           indices = orddict:erase(Vid, Indices)}
    end.

-spec vote_rec(#vstate_v{} | #vstate_p{}, path(), yes | no) ->
    {#vstate_v{} | #vstate_p{}, vote()}.

vote_rec(S = #vstate_v{children = States, thresh = T,
                       yes_votes = YesVotes, no_votes = NoVotes},
         [Index|Path], Vote) ->
    {Init, [Node|Tail]} = lists:split(Index, States),
    {State, SubVote} = vote_rec(Node, Path, Vote),
    Votes = case State of
                #vstate_v{votes = V} -> V;
                #vstate_p{votes = V} -> V
            end,
    NewS = S#vstate_v{children = Init ++ [State|Tail]},
    case SubVote of
        yes ->
            NewVote = case YesVotes + Votes >= T of
                          true -> yes;
                          false -> pending
                      end,
            {NewS#vstate_v{yes_votes = YesVotes + Votes}, NewVote};
        no ->
            NewVote = case NoVotes + Votes > length(States) - T of
                          true -> no;
                          false -> pending
                      end,
            {NewS#vstate_v{no_votes = NoVotes + Votes}, NewVote};
        pending ->
            {NewS, pending}
    end;

vote_rec(S = #vstate_p{vote = pending}, [], Vote) ->
    {S#vstate_p{vote = Vote}, Vote}.
