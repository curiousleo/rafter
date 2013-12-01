-module(voting).

-export([init_vstate/2]).

-include("voting.hrl").

-spec init_vstate(#vstruct_v{}, #vstruct_p{} | #vstruct_v{}) ->
    #vstate_p{} | #vstate_v{}.

init_vstate(Parent, #vstruct_p{}) ->
    #vstate_p{parent = Parent};

init_vstate(Parent, Struct = #vstruct_v{thresh = T, children = Structs}) ->
    States = lists:map(fun(Child) -> init_vstate(Struct, Child) end,
                       Structs),
    #vstate_v{parent = Parent, children = States, thresh = T}.
