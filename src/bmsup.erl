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
    gen_fsm:send_event(Sup, {start, {Func, Conc, Times, Distr}}).

init([]) ->
    {ok, waiting, []}.

waiting({start, {Func, Conc, Times, Distr}}, []) ->
    {ok, Sup} = supervisor:start_link(bmworker, {Func, Times, self()}),
    Children = lists:map(fun(_N) -> start_child(Sup) end, lists:seq(1, Conc)),
    S = #state{func=Func, conc=Conc, times=Times, sup=Sup, distr=Distr,
               children=Children},
    {next_state, running, S}.

% helper function for waiting/2
start_child(Sup) ->
    {ok, Child} = supervisor:start_child(Sup, []),
    Child.

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

handle_event(Event, StateName, StateData) ->
    {stop, {"unknown asynchronous event", StateName, Event}, StateData}.

handle_sync_event(Event, From, StateName, StateData) ->
    {stop, {"unknown synchronous event", From, StateName, Event}, StateData}.

handle_info(Info, StateName, StateData) ->
    {stop, {"unknown event", StateName, Info}, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
