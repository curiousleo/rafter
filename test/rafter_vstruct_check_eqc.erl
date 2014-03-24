-module(rafter_vstruct_check_eqc).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("rafter.hrl").

-compile(export_all).

eqc_test_() ->
    ?_assertEqual(true, eqc:quickcheck(eqc:numtests(50, prop_correct_vstruct()))).

prop_correct_vstruct() ->
    ?FORALL(Struct, rafter_gen:vstruct(), check_all(Struct)).

-spec check_all(#vstruct{}) -> boolean().
check_all(Struct) ->
    dict:fold(
      fun(_Id, Results, true) ->
              lists:foldl(
                fun(ok, true) -> true; (_,  _   ) -> false end, true, Results);
         (_Id, _Results, false) -> false
      end, true, check(Struct)).

-spec check(#vstruct{}) -> dict(). % dict({peer(), [boolean()]}).
check(#vstruct{tree = Tree, indices = Indices}) ->
    dict:map(fun(Peer, Paths) ->
                     lists:map(
                       fun(Path) -> leads_to(Path, Tree, Peer) end , Paths)
             end, Indices).

-spec leads_to([non_neg_integer()], #vstruct_v{}, peer()) -> ok | {error, term()}.
leads_to([Index|Path], #vstruct_v{children = Children}, PeerId)
  when length(Children) > Index ->
    case lists:nth(Index + 1, Children) of
        Tree = #vstruct_v{} -> leads_to(Path, Tree, PeerId);
        #vstruct_p{id = Id} -> if Id =:= PeerId -> ok;
                                  true -> {error, mismatch} end
    end;
leads_to(_, _, _) ->
    {error, list_length}.

-endif.
