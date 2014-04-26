-module(rafter_ttc_log).

-behaviour(gen_server).

%% api
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
          logfile :: file:io_device(),
          experiment :: term(),
          cluster_size :: pos_integer()}).

start_link() ->
    gen_server:start_link({local, rafter_ttc_log}, ?MODULE, [], []).

init(_) ->
    {ok, F} = file:open("/tmp/rafter_ttc_log.log", [write]),
    io:format(F, "Experiment,Cluster,Operation,TTC", []),
    {ok, #state{logfile = F}}.

handle_info({log_write, T},
            S = #state{logfile = F, experiment = E, cluster_size = C}) ->
    io:format(F, "~p,~p,Write,~p~n", [E, C, T]),
    {noreply, S};
handle_info({log_read, T},
            S = #state{logfile = F, experiment = E, cluster_size = C}) ->
    io:format(F, "~p,~p,Read,~p~n", [E, C, T]),
    {noreply, S};
handle_info({set_experiment, {C, E}}, S) ->
    {noreply, S#state{cluster_size = C, experiment = E}};
handle_info(failed, S = #state{logfile = F}) ->
    io:format(F, "FAILED", []),
    S;
handle_info(restarted, S = #state{logfile = F}) ->
    io:format(F, "RESTARTED", []),
    S.

handle_call(_Request, _From, State) ->
    {stop, handle_call_called, State}.
handle_cast(_Request, State) ->
    {stop, handle_cast_called, State}.
terminate(_Reason, F) ->
    file:close(F).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
