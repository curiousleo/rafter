-module(bmsup).
-behaviour(gen_fsm).

-export([init/1, terminate/3,
         handle_event/3, handle_sync_event/4,
         handle_info/3, code_change/4,
         waiting/2, running/2]).

init(Distr) ->
    {ok, waiting, Distr}.

waiting({start, {Func, Conc, Times}}, Distr) ->
    Sup = supervisor:start_link(bmworker, {Func, Times, self()}),
    lists:foreach(fun(_N) -> supervisor:start_child(Sup, []) end, lists:seq(1, Conc)),
    {next_state, running, {[], Conc, Sup, Distr}}.

running({done, Latency}, {Latencies, 0, Sup, Distr}) ->
    Distr ! {self(), done, [Latency|Latencies]},
    {next_state, waiting, Sup, Distr};

running({done, Latency}, {Latencies, Conc, Sup, Distr}) ->
    {next_state, running, {[Latency|Latencies], Conc-1, Sup, Distr}}.

terminate(_Reason, waiting, _StateData) ->
    ok;

terminate(_Reason, running, {_Latencies, N, Sup, _Distr}) ->
    lists:foreach(fun(_N) -> kill_child(Sup, bmworker_id) end, lists:seq(1, N)),
    ok.

% helper function for terminate/2
kill_child(Sup, ChildId) ->
    supervisor:terminate_child(Sup, ChildId),
    supervisor:delete_child(Sup, ChildId).

handle_event(Event, _StateName, _StateData) ->
    {stop, "unknown asynchronous event occurred", Event}.

handle_sync_event(Event, _From, _StateName, StateData) ->
    {stop, "unknown synchronous event occurred", Event}.

handle_info(Info, _StateName, _StateData) ->
    {stop, "unknown event occurred", Info}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
