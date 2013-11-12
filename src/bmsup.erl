-module(bmsup).
-behaviour(gen_fsm).

-export([start_bm/5]).
-export([init/1, terminate/3,
         handle_event/3, handle_sync_event/4,
         handle_info/3, code_change/4,
         waiting/2, running/3]).

-record(state, {
    latencies=[] :: list(),
    func :: fun(),
    conc=0 :: non_neg_integer(),
    times=0 :: non_neg_integer(),
    sup :: pid(),
    distr :: pid(),
    children=[] :: list()
}).

start_bm(Sup, Func, Conc, Times, Distr) ->
    S = #state{func=Func, conc=Conc, times=Times, sup=Sup, distr=Distr},
    gen_fsm:send_event(Sup, {start, S}).

init([]) ->
    {ok, waiting, []}.

waiting({start, S=#state{func=Func, conc=Conc, times=Times}}, []) ->
    {ok, Sup} = supervisor:start_link(bmworker, {Func, Times, self()}),
    Children = lists:map(fun(_N) -> io:format("child started~n"), {ok, Child} = supervisor:start_child(Sup, []), Child end, lists:seq(1, Conc)),
    {next_state, running, S#state{children=Children}}.

running({done, Latency}, _From, S=#state{latencies=Latencies, conc=0}) ->
    {reply, [Latency|Latencies], waiting, S#state{latencies=[]}};

running({done, Latency}, _From, S=#state{latencies=Latencies}) ->
    {next_state, running, S#state{latencies=[Latency|Latencies]}}.

terminate(_Reason, waiting, _StateData) ->
    ok;

terminate(_Reason, running, #state{sup=Sup, children=Children}) ->
    lists:foreach(fun(Child) -> kill_child(Sup, Child) end, Children),
    ok.

% helper function for terminate/2
kill_child(Sup, ChildId) ->
    supervisor:terminate_child(Sup, ChildId),
    supervisor:delete_child(Sup, ChildId).

handle_event(Event, _StateName, _StateData) ->
    {stop, "unknown asynchronous event occurred", Event}.

handle_sync_event(Event, _From, _StateName, _StateData) ->
    {stop, "unknown synchronous event occurred", Event}.

handle_info(Info, _StateName, _StateData) ->
    {stop, "unknown event occurred", Info}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
