-module(rafter_config).

-include("rafter.hrl").

%% API
-export([init_vstate/1, quorum/3, quorum_max/3, voters/1, voters/2,
         followers/2, reconfig/2, allow_config/2, has_vote/2]).

%%====================================================================
%% API
%%====================================================================

-spec init_vstate(#config{}) -> #vstate{}.
init_vstate(#config{state=stable, oldvstruct=Old}) ->
    rafter_voting:init_vstate(Old);
init_vstate(#config{state=staging, oldvstruct=Old}) ->
    rafter_voting:init_vstate(Old);
init_vstate(#config{state=transitional, oldvstruct=Old, newvstruct=New}) ->
    rafter_voting:init_vstate(rafter_voting:merge_vstructs(1, 2, [Old, New])).

-spec quorum_max(peer(), #config{} | #vstruct{}, dict()) -> non_neg_integer().
quorum_max(_Me, #config{state=blank}, _) ->
    0;
quorum_max(Me, #config{state=stable, oldvstruct=OldServers}, Responses) ->
    quorum_max(Me, OldServers, Responses);
quorum_max(Me, #config{state=staging, oldvstruct=OldServers}, Responses) ->
    quorum_max(Me, OldServers, Responses);
quorum_max(Me, #config{state=transitional,
                   oldvstruct=Old,
                   newvstruct=New}, Responses) ->
    min(quorum_max(Me, Old, Responses), quorum_max(Me, New, Responses));

%% Sort the values received from the peers from lowest to highest
%% Peers that haven't responded have a 0 for their value.
%% This peer (Me) will always have the maximum value
quorum_max(Me, Servers, Responses) ->
    Indices = unique_sort(
                lists:map(fun(S) -> index(S, Responses) end,
                          rafter_voting:to_list(Servers))),
    Accepted = lists:takewhile(
                 fun(Index) -> has_quorum(Me, Servers, Responses, Index) end,
                 Indices),
    case Accepted of [] -> 0; _ -> lists:last(Accepted) end.

-spec quorum(peer(), #config{} | #vstruct{}, dict()) -> boolean().
quorum(_Me, #config{state=blank}, _Responses) ->
    false;
quorum(Me, #config{state=stable, oldvstruct=OldServers}, Responses) ->
    quorum(Me, OldServers, Responses);
quorum(Me, #config{state=staging, oldvstruct=OldServers}, Responses) ->
    quorum(Me, OldServers, Responses);
quorum(Me, #config{state=transitional, oldvstruct=Old, newvstruct=New}, Responses) ->
    quorum(Me, Old, Responses) andalso quorum(Me, New, Responses);

%% Responses doesn't contain a local vote which must be true if the local
%% server is a member of the consensus group. Add 1 to TrueResponses in
%% this case.
quorum(Me, Struct, Responses) ->
    Servers = rafter_voting:to_list(Struct),
    Votes = dict:filter(
              fun(Peer, R) -> R =:= true andalso
                              lists:member(Peer, Servers) end,
              dict:store(Me, true, Responses)),
    rafter_voting:quorum(Struct, Votes).

%% @doc list of voters excluding me
-spec voters(peer(), #config{}) -> list().
voters(Me, Config) ->
    lists:delete(Me, voters(Config)).

%% @doc list of all voters
-spec voters(#config{}) -> list().
voters(#config{state=transitional, oldvstruct=Old, newvstruct=New}) ->
    sets:to_list(sets:from_list(
                   rafter_voting:to_list(Old) ++ rafter_voting:to_list(New)));
voters(#config{oldvstruct=Old}) ->
    rafter_voting:to_list(Old).

-spec has_vote(peer(), #config{}) -> boolean().
has_vote(_Me, #config{state=blank}) ->
    false;
has_vote(Me, #config{state=transitional, oldvstruct=Old, newvstruct=New})->
    rafter_voting:member(Me, Old) orelse rafter_voting:member(Me, New);
has_vote(Me, #config{oldvstruct=Old}) ->
    rafter_voting:member(Me, Old).

%% @doc All followers. In staging, some followers are not voters.
-spec followers(peer(), #config{}) -> list().
followers(Me, #config{state=transitional, oldvstruct=Old, newvstruct=New}) ->
    lists:delete(Me, sets:to_list(sets:from_list(
                                    rafter_voting:to_list(Old) ++
                                    rafter_voting:to_list(New))));
followers(Me, #config{state=staging, oldvstruct=Old, newvstruct=New}) ->
    lists:delete(Me, sets:to_list(sets:from_list(
                                    rafter_voting:to_list(Old) ++
                                    rafter_voting:to_list(New))));
followers(Me, #config{oldvstruct=Old}) ->
    lists:delete(Me, rafter_voting:to_list(Old)).

%% @doc Go right to stable mode if this is the initial configuration.
-spec reconfig(#config{}, #vstruct{}) -> #config{}.
reconfig(#config{state=blank}=Config, Struct) ->
    Config#config{state=stable, oldvstruct=Struct};
reconfig(#config{state=stable}=Config, Struct) ->
    Config#config{state=transitional, newvstruct=Struct}.

-spec allow_config(#config{}, list()) -> boolean().
allow_config(#config{state=blank}, _NewServers) ->
    true;
allow_config(#config{state=stable, oldvstruct=OldServers}, NewServers)
    when NewServers =/= OldServers ->
    %% TODO: is inequality well-defined on arbitrary records?
    true;
allow_config(#config{oldvstruct=OldServers}, NewServers)
    when NewServers =:= OldServers ->
    {error, not_modified};
allow_config(_Config, _NewServers) ->
    {error, config_in_progress}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec index(atom() | {atom(), atom()}, dict()) -> non_neg_integer().
index(Peer, Responses) ->
    case dict:find(Peer, Responses) of
        {ok, Index} ->
            Index;
        error ->
            0
    end.

-spec unique_sort(list()) -> list().
unique_sort(L) ->
    ordsets:to_list(ordsets:from_list(L)).

-spec has_quorum(term(), #vstruct{}, dict(), non_neg_integer()) ->
    boolean().
has_quorum(Me, Servers, Responses, Index) ->
    Positive = dict:filter(fun(_, I) -> I >= Index end, Responses),
    Votes = dict:map(fun(_, _) -> true end, Positive),
    case rafter_voting:member(Me, Servers) of
        true -> rafter_voting:quorum(Servers, dict:store(Me, true, Votes));
        false -> rafter_voting:quorum(Servers, Votes)
    end.
