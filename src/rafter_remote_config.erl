-module(rafter_remote_config).

-export([remote_config/3]).

remote_config(Leader, Followers, Protocol) ->
    Vstruct = generate([Leader|Followers], Protocol),
    rafter:set_config(Leader, Vstruct).

generate(Peers, majority) ->
    rafter_voting_majority:majority(Peers);
generate(Peers, grid) ->
    rafter_voting_grid:grid(Peers);
generate(Peers, {tree, D}) ->
    rafter_voting_tree:tree(Peers, D).
