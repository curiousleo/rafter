-module(bmdistr).
-behaviour(gen_fsm).

% -export([benchmark/4]).
-export([init/1, terminate/3,
         handle_event/3, handle_sync_event/4,
         handle_info/3, code_change/4,
         waiting/2, running/2]).

-record(state, {
    nodes=[] :: list(),
    times=0 :: non_neg_integer(),
    latencies=[] :: list(),
    running=0 :: non_neg_integer()
}).

% benchmark(Func, Nodes, Conc, Times) ->
    % lists:foreach(fun(N) -> {bmsup_p, N} ! {self(), {init, {Conc, Func, Times}}} end, Nodes),
    % await_ready(length(Nodes)),
    % lists:foreach(fun(N) -> N ! start end, Nodes),
    % collect_results(length(Nodes), []).

init(Nodes) ->
    {ok, waiting, #state{nodes=Nodes}}.

waiting({add_node, Node}, State=#state{nodes=Nodes}) ->
    {next_state, waiting, State#state{nodes=[Node|Nodes]}};

waiting({start, {Func, N}}, State=#state{nodes=Nodes}) ->
    lists:foreach(fun(Node) ->  {Node, bmsup_p} ! {start, {Func, N}} end, Nodes),
    {next_state, running, State#state{running=length(Nodes)}}.

running({done, Latencies}, State=#state{latencies=AvgLatencies, running=0}) ->
    lists:foreach(fun(L) -> io:format("Latency: ~w~n", L) end, AvgLatencies),
    {next_state, waiting, State}; % ??

running({done, Latencies}, State=#state{latencies=AvgLatencies, running=R}) ->
    Avg = lists:sum(Latencies) / State#state.times,
    {next_state, running, State#state{latencies=[Avg|AvgLatencies], running=R-1}}.

terminate(_Reason, waiting, _StateData) ->
    ok;

terminate(_Reason, running, #state{nodes=Nodes}) ->
    % TODO: do something
    ok.

handle_event(Event, _StateName, _StateData) ->
    {stop, "unknown asynchronous event occurred", Event}.

handle_sync_event(Event, _From, _StateName, StateData) ->
    {stop, "unknown synchronous event occurred", Event}.

handle_info(Info, _StateName, _StateData) ->
    {stop, "unknown event occurred", Info}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
