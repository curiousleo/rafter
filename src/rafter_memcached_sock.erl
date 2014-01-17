-module(rafter_memcached_sock).

-behaviour(gen_server).

%% api
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

%%
%% api
%%

start_link(ListenSocket, Peer) ->
    gen_server:start_link(?MODULE, [ListenSocket, Peer], []).

%%
%% gen_server callbacks
%%

init([ListenSocket, Peer]) ->
    gen_server:cast(self(), accept),
    {ok, {ListenSocket, Peer}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(accept, {ListenSocket, Peer}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    rafter_memcached_sup:start_socket(Peer),
    send(AcceptSocket, "WHATS UP", []),
    {noreply, {AcceptSocket, Peer}}.

handle_info({tcp, Socket, Str}, {AcceptSocket, Peer}) ->
    send(Socket, Str, []),
    {noreply, {AcceptSocket, Peer}};
handle_info({tcp_closed, _Socket}, S) ->
    {stop, normal, S};
handle_info({tcp_error, _Socket}, S) ->
    {stop, normal, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%
%% helper functions
%%

send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str ++ "~n", Args)),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.
