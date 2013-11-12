-module(bmdistr).
-behaviour(gen_fsm).

-export([benchmark/4]).
-export([init/1, terminate/3,
         handle_event/3, handle_sync_event/4,
         handle_info/3, code_change/4,
         waiting/2, running/2]).

-record(state, {
    nodes=[] :: list(),
    conc=0 :: non_neg_integer(),
    times=0 :: non_neg_integer(),
    latencies=[] :: list(),
    running=0 :: non_neg_integer()
}).

benchmark(Func, Nodes, Conc, Times) ->
    {ok, Distr} = gen_fsm:start_link(?MODULE, Nodes, []),
    gen_fsm:send_event(Distr, {start, {Func, Conc, Times}}).

init(Nodes) ->
    {ok, waiting, #state{nodes=Nodes}}.

waiting({add_node, Node}, State=#state{nodes=Nodes}) ->
    {next_state, waiting, State#state{nodes=[Node|Nodes]}};

waiting({start, {Func, Conc, Times}}, State=#state{nodes=Nodes}) ->
    Msg = {start, {Func, Conc, Times, self()}},
    lists:foreach(fun(Node) ->  {bmsup_p, Node} ! Msg end, Nodes),
    NewState = State#state{conc=Conc, times=Times, running=length(Nodes)},
    {next_state, running, NewState}.

running({done, Latencies}, State=#state{latencies=AvgLatencies, running=0}) ->
    Avg = lists:sum(Latencies) / State#state.times,
    lists:foreach(fun(L) -> io:format("Latency: ~w~n", L) end, [Avg|AvgLatencies]),
    {next_state, waiting, State}; % ??

running({done, Latencies}, State=#state{latencies=AvgLatencies, running=R}) ->
    Avg = lists:sum(Latencies) / State#state.times,
    {next_state, running, State#state{latencies=[Avg|AvgLatencies], running=R-1}}.

terminate(_Reason, waiting, _StateData) ->
    ok;

terminate(_Reason, running, #state{nodes=_Nodes}) ->
    % TODO: do something
    ok.

handle_event(Event, _StateName, _StateData) ->
    {stop, "unknown asynchronous event occurred", Event}.

handle_sync_event(Event, _From, _StateName, _StateData) ->
    {stop, "unknown synchronous event occurred", Event}.

handle_info(Info, _StateName, _StateData) ->
    {stop, "unknown event occurred", Info}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
