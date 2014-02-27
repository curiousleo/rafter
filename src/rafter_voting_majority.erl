-module(rafter_voting_majority).

-export([majority/1]).

-include("rafter.hrl").

-spec majority([peer()]) -> #vstruct{}.
majority(Ids) ->
    T = round((length(Ids) + 1) / 2),
    Phys = lists:map(fun(Id) -> #vstruct_p{id = Id} end, Ids),
    {Indices, _} = lists:mapfoldl(fun(Id, K) -> {{Id, [[K]]}, K+1} end,
                                  0, Ids),
    #vstruct{tree = #vstruct_v{thresh = T, children = Phys},
             indices = dict:from_list(Indices)}.
