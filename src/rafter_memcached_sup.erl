-module(rafter_memcached_tcp).

-behaviour(supervisor).

-include("rafter.hrl").
-include("rafter_opts.hrl").

%% api
-export([start_link/2]).

%% supervisor callbacks
-export([init/1]).

%%
%% api
%%

start_link(Peer, Opts = #rafter_opts{state_machine = rafter_backend_memcached}) ->
    supervisor:start_link({local, server_name(Peer)}, ?MODULE, [Peer, Opts], []).

%%
%% supervisor callbacks
%%

init([_Name, _Opts]) ->
    Port = 8091,
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, once}, {packet, line}]),
    spawn_link(fun empty_listeners/0),
    {ok, {{simple_one_for_one, 60, 3600},
          [{socket,
            {rafter_memcached_sock, start_link, [ListenSocket]},
            temporary, 1000, worker, [rafter_memcached_sock]}
          ]}}.

%%
%% helper functions
%%

server_name(Me) ->
    list_to_atom(atom_to_list(Me) ++ "_memcached").

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_listeners() ->
    [start_socket() || _ <- lists:seq(1, 20)],
    ok.
