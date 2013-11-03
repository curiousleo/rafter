-module(bmsup).
-behaviour(gen_fsm).

-export([init/1, terminate/3,
         handle_event/3, handle_sync_event/4,
         handle_info/3, code_change/4,
         waiting/2, running/2]).

init(Distr) ->
    {ok, waiting, Distr}.

waiting({start, {Func, N}}, Distr) ->
    Sup = supervisor:start_link(bmworker, {Func, N}),
    lists:foreach(fun(_N) -> supervisor:start_child(Sup, []) end, lists:seq(1, N)),
    {next_state, running, {[], N, Sup, Distr}}.

running({done, Latency}, {Latencies, 0, Sup, Distr}) ->
    Distr ! {self(), done, [Latency|Latencies]},
    {next_state, waiting, Sup, Distr};

running({done, Latency}, {Latencies, N, Sup, Distr}) ->
    {next_state, running, {[Latency|Latencies], N-1, Sup, Distr}}.
