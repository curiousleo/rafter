-module(rafter_voting_majority).

-export([majority/1]).

-include("rafter.hrl").

-spec majority(list(peer())) -> #vstruct{}.
majority(Ids) ->
    Treshold = round((length(Ids) + 1) / 2),
    Nodes = [#vstruct_p{id = Id } || Id <- Ids],
    {Indices, _} = lists:mapfoldl(fun(Id, K) -> {{Id, [[K]]}, K+1} end,
                                  0, Ids),
    #vstruct{tree = #vstruct_v{thresh = Treshold, children = Nodes},
             indices = dict:from_list(Indices)}.
