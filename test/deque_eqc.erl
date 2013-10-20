-module(deque_eqc).
-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

prop_list_conversion() ->
    ?FORALL(List, list(int()), deque:to_list(deque:from_list(List)) == List).

prop_empty_is_null() ->
    deque:null(deque:empty()).

prop_head_after_cons() ->
    ?FORALL({Head,Tail}, {int(),list(int())},
            deque:head(deque:from_list([Head|Tail])) == Head).

prop_tail_after_cons() ->
    ?FORALL({Head,Tail}, {int(),list(int())},
            deque:to_list(deque:tail(deque:from_list([Head|Tail]))) == Tail).

prop_cons_snoc_equivalence() ->
    FromListSnoc = fun(List) ->
                          lists:foldl(
                            fun(X,Q) -> deque:snoc(Q,X) end,
                            deque:empty(),
                            List)
                   end,
    FromListCons = fun(List) ->
                          lists:foldr(fun deque:cons/2, deque:empty(), List)
                   end,
    ?FORALL(List, list(int()),
            deque:to_list(FromListSnoc(List)) ==
            deque:to_list(FromListCons(List))).
