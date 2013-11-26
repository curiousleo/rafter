-module(bmdistr).
-behaviour(gen_server).

-export([benchmark/4]).
-export([callback/3]).
-export([init/1, terminate/2,
         handle_call/3, handle_cast/2,
         handle_info/2, code_change/3]).

-record(state, {
    nodes=[] :: list(),
    conc=0 :: non_neg_integer(),
    times=0 :: non_neg_integer(),
    latencies=[] :: list(),
    running=0 :: non_neg_integer()
}).

benchmark(Nodes, Func, Conc, Times) ->
    {ok, Distr} = gen_fsm:start_link(?MODULE, Nodes, []),
    gen_fsm:send_event(Distr, {start, {Func, Conc, Times}}).

init(Nodes) ->
    {ok, waiting, #state{nodes=Nodes}}.

handle_call(_, _, S) -> {stop, error, S}.

handle_cast({start, {Func, Conc, Times}}, State=#state{nodes=Nodes}) ->
    Msg = {start, {Func, Conc, Times}},
    Args = [Msg, self()],
    lists:foreach(fun(Node) -> spawn(?MODULE, callback, [Node|Args]) end,
                  Nodes),
    NewState = State#state{conc=Conc, times=Times, running=length(Nodes)},
    {noreply, NewState};

handle_cast({done, Latencies}, State=#state{latencies=AvgLatencies, running=1}) ->
    io:format("running received ~w (last one)~n", [{done, Latencies}]),
    Avg = lists:sum(Latencies) / State#state.conc,
    {stop, normal, State#state{latencies=[Avg|AvgLatencies]}}; % ??

handle_cast({done, Latencies}, State=#state{latencies=AvgLatencies, running=R}) ->
    io:format("running received ~w (~w to go)~n", [{done, Latencies}, R]),
    Avg = lists:sum(Latencies) / State#state.conc,
    {noreply, State#state{latencies=[Avg|AvgLatencies], running=R-1}}.

callback(Node, Msg, Distr) ->
    {done, Latencies} = gen_server:call({bmsup_p, Node}, Msg),
    io:format("callback received ~w~n", [{done, Latencies}]),
    gen_fsm:send_event(Distr, {done, Latencies}).

terminate(normal, #state{latencies=Latencies}) ->
    % TODO: do something
    io:format("Latencies: ~w~n", [Latencies]),
    ok;

terminate(_Reason, _StateData) ->
    ok.

handle_info(Info, _StateData) ->
    io:format("unknown event: ~w~n", [Info]),
    {stop, "unknown event occurred", Info}.

code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.
