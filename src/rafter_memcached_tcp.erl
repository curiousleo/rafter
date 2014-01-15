-module(rafter_memcached_tcp).

-behaviour(gen_server).

-include("rafter.hrl").
-include("rafter_opts.hrl").

%% api
-export([start_link/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%
%% api
%%

start_link(Peer, Opts = #rafter_opts{state_machine = rafter_backend_memcached}) ->
    gen_server:start_link({local, server_name(Peer)}, ?MODULE, [Peer, Opts], []).

stop(Peer) ->
    gen_server:cast(server_name(Peer), stop).

%%
%% gen_server callbacks
%%

init([_Name, _Opts]) ->
    start_server(8091).

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% helper functions
%%

server_name(Me) ->
    list_to_atom(atom_to_list(Me) ++ "_memcached").

start_server(Port) ->
    Pid = spawn_link(fun() ->
                             {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
                             spawn(fun() -> acceptor(Listen) end),
                             timer:sleep(infinity)
                     end),
    {ok, Pid}.

acceptor(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> acceptor(ListenSocket) end),
    handle(Socket).

%% Echoing back whatever was obtained
handle(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"quit", _/binary>>} ->
            gen_tcp:close(Socket);
        {tcp, Socket, Msg} ->
            gen_tcp:send(Socket, Msg),
            handle(Socket)
    end.
