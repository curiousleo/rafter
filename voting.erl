-module(voting).

-export([init_vstate/1]).

-include("voting.hrl").

-spec init_vstate(#vstruct{}) -> #vstate{}.

init_vstate(#vstruct{tree = Tree, indices = Indices}) ->
    #vstate{tree = init_vstate_tree(undefined, Tree), indices = Indices}.

-spec init_vstate_tree(#vstruct_v{}, #vstruct_p{} | #vstruct_v{}) ->
    #vstate_p{} | #vstate_v{}.

init_vstate_tree(Parent, #vstruct_p{}) ->
    #vstate_p{parent = Parent};

init_vstate_tree(Parent,
                 Struct = #vstruct_v{thresh = T, children = Structs}) ->
    States = lists:map(fun(Child) -> init_vstate_tree(Struct, Child) end,
                       Structs),
    #vstate_v{parent = Parent, children = States, thresh = T}.
