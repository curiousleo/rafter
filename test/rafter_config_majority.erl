-module(rafter_config_majority).

-export([quorum/3, quorum_max/3]).

-include("rafter.hrl").

quorum(Me, Servers, Responses) ->
    TrueResponses = [R || {Peer, R} <- dict:to_list(Responses), R =:= true,
                                        lists:member(Peer, Servers)],
    case lists:member(Me, Servers) of
        true ->
            length(TrueResponses) + 1 > length(Servers)/2;
        false ->
            %% We are about to commit a new configuration that doesn't contain
            %% the local leader. We must therefore have responses from a
            %% majority of the other servers to have a quorum.
            length(TrueResponses) > length(Servers)/2
    end.

quorum_max(_, [], _) ->
    0;
quorum_max(Me, Servers, Responses) when (length(Servers) rem 2) =:= 0->
    Values = sorted_values(Me, Servers, Responses),
    lists:nth(length(Values) div 2, Values);
quorum_max(Me, Servers, Responses) ->
    Values = sorted_values(Me, Servers, Responses),
    lists:nth(length(Values) div 2 + 1, Values).

-spec sorted_values(peer(), [peer()], dict()) -> [non_neg_integer()].
sorted_values(Me, Servers, Responses) ->
    Vals = lists:sort(lists:map(fun(S) -> value(S, Responses) end, Servers)),
    case lists:member(Me, Servers) of
        true ->
            %% Me is always in front because it is 0 from having no response
            %% Strip it off the front, and add the max to the end of the list
            [_ | T] = Vals,
            lists:reverse([lists:max(Vals) | lists:reverse(T)]);
        false ->
            Vals
    end.

-spec value(peer(), dict()) -> non_neg_integer().
value(Peer, Responses) ->
    case dict:find(Peer, Responses) of
        {ok, Value} ->
            Value;
        error ->
            0
    end.
