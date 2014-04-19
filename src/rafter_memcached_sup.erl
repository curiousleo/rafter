-module(rafter_memcached_sup).

-behaviour(supervisor).

-include("rafter.hrl").
-include("rafter_opts.hrl").

%% api
-export([start_link/2, start_socket/1]).

%% supervisor callbacks
-export([init/1]).

%%
%% api
%%

start_link(Peer, Opts = #rafter_opts{state_machine = rafter_backend_memcached}) ->
    supervisor:start_link({local, server_name(Peer)}, ?MODULE, [Peer, Opts]).

start_socket(Peer) ->
    supervisor:start_child(server_name(Peer), []).

%%
%% supervisor callbacks
%%

init([Peer, _Opts]) ->
    Port = 11211,
    ListenSocket = listen(Port),
    spawn_link(fun () -> empty_listeners(Peer) end),
    {ok, {{simple_one_for_one, 60, 3600},
          [{rafter_memcached_sock,
            {rafter_memcached_sock, start_link, [ListenSocket, Peer]},
            temporary, 1000, worker, [rafter_memcached_sock]}
          ]}}.

%%
%% helper functions
%%

server_name(Me) ->
    list_to_atom(atom_to_list(Me) ++ "_memcached").

empty_listeners(Peer) ->
    [start_socket(Peer) || _ <- lists:seq(1, 100)],
    ok.

listen(Port) ->
    case gen_tcp:listen(Port, [binary, {active, once}]) of
        {ok, Socket} ->
            Socket;
        {error, eaddrinuse} ->
            listen(Port + 1)
    end.
