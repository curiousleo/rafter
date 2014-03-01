-module(rafter_ttc_log).

-behaviour(gen_server).

%% api
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link({local, rafter_ttc_log}, ?MODULE, [], []).

init(_) ->
    file:open("/tmp/rafter_ttc_log.log", [write]).

handle_call(_Request, _From, State) ->
    {stop, handle_call_called, State}.
handle_cast(_Request, State) ->
    {stop, handle_cast_called, State}.
handle_info(Info, F) ->
    case Info of
        {undefined, T} ->
            io:format(F, "W ~p~n", [T]);
        {_, T} ->
            io:format(F, "R ~p~n", [T])
    end,
    {noreply, F}.
terminate(_Reason, F) ->
    file:close(F).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
