-module(rafter_remote_config).

-export([remote_config/3]).

remote_config(Leader, Followers, Protocol) ->
    {Vstruct, Name} = generate([Leader|Followers], Protocol),
    rafter:set_config(Leader, Vstruct),
    rafter_ttc_log ! {set_experiment, {length(Followers) + 1, Name}}.

generate(Peers, majority) ->
    {rafter_voting_majority:majority(Peers), "majority"};
generate(Peers, grid) ->
    {rafter_voting_grid:grid(Peers), "grid"};
generate(Peers, {tree, D}) ->
    {rafter_voting_tree:tree(Peers, D), "tree" ++ integer_to_list(D)};
generate(Peers, plain) ->
    {Peers, plain}.
