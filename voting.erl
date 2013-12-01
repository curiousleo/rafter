-module(voting).

-export([init_vstate/2]).

-include("voting.hrl").

-spec init_vstate(#vstruct_v{}, #vstruct_p{} | #vstruct_v{}) ->
    #vstate_p{} | #vstate_v{}.
%% Leaf vstruct
init_vstate(Parent, #vstruct_p{}) ->
    #vstate_p{parent = Parent};
init_vstate(Parent, VStruct = #vstruct_v{children = Children}) ->
    ChildStates = lists:map(
                   fun(Child) -> init_vstate(VStruct, Child) end,
                   Children),
    #vstate_v{parent = Parent, children = ChildStates}.
